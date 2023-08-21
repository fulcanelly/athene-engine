import { ModelFactory, ModelRelatedNodesI, NeogmaInstance, QueryBuilder, QueryRunner } from 'neogma';
import { Post, PostInstance } from './post';
import { neogma } from '../neo4j';
import { User, UserInstance } from './user';
import { NotificationInstance, Notification } from './notification';


export type ChannelProps = {
    uuid: string;
    channel_id: string;
};

export interface ChannelRelatedNodesI {
    notificated: ModelRelatedNodesI<typeof Notification, NotificationInstance>

    posts: ModelRelatedNodesI<typeof Post, PostInstance, {
        created_at: string
    }>;
    likes: ModelRelatedNodesI<typeof Channel, ChannelInstance, {
        created_at: string
    }>;
    dislikes: ModelRelatedNodesI<typeof Channel, ChannelInstance, {
        created_at: string
    }>;
    user: ModelRelatedNodesI<typeof User, UserInstance>;
}

export type ChannelInstance = NeogmaInstance<ChannelProps, ChannelRelatedNodesI>;


export interface ChannelMethods {
    like(channel: ChannelInstance): Promise<void>;
    dislike(channel: ChannelInstance): Promise<void>;
    findPartner(): Promise<ChannelInstance | undefined>;
}

export const Channel = ModelFactory<ChannelProps, ChannelRelatedNodesI, {}, ChannelMethods>(
    {
        label: 'Channel',
        schema: {
            channel_id: {
                type: 'string',
                required: true
            },
            uuid: {
                type: 'string',
                required: true,
            },
        },
        methods: {
            async like(channel: ChannelInstance) {
                await (this as ChannelInstance).relateTo({
                    alias: 'likes',
                    where: {
                        channel_id: channel.channel_id
                    },
                    properties: {
                        created_at: new Date().toString()
                    }
                })
                await Notification.createLikeNotification({
                    liked_by_chan_id: (this as ChannelInstance).channel_id,
                    target_chan_id: channel.channel_id
                })
            },

            async dislike(channel: ChannelInstance) {
                await (this as ChannelInstance).relateTo({
                    alias: 'dislikes',
                    where: {
                        channel_id: channel.channel_id
                    },

                    properties: {
                        created_at: new Date().toString()
                    }
                })
            },

            async findPartner(): Promise<ChannelInstance | undefined> {
                const result = await new QueryBuilder()
                    .match({
                        related: [
                            {
                                model: Channel,
                                identifier: 'init',
                                where: {
                                    channel_id: (this as ChannelInstance).channel_id
                                },
                            },
                        ],
                    })
                    .match({
                        model: Channel,
                        identifier: 'result',
                    })
                    .where('not (init)-[:LIKES]->(result) and not (init)-[:DISLIKES]->(result) and init <> result')
                    .return('result')
                    .limit(1)
                    .run(neogma.queryRunner)


                const foundList = QueryRunner.getResultProperties<ChannelProps>(result, 'result')
                if (!foundList.length) {
                    return
                }

                return Channel.buildFromRecord({
                    properties: foundList[0],
                    labels: ['Channel']
                })
            },
        },

        primaryKeyField: 'uuid',
    },
    neogma,
);


