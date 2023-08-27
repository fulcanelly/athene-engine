import { ModelFactory, ModelRelatedNodesI, NeogmaInstance, QueryBuilder, QueryRunner } from 'neogma';
import { PostTemplate, PostTemplateInstance } from './post_template';
import { neogma } from '../neo4j';
import { User, UserInstance } from './user';
import { NotificationInstance, Notification } from './notification';
import { PostSchedule, PostScheduleInstance, PostScheduleProps } from './post_schedule';


export type ChannelProps = {
    uuid: string;
    channel_id: string;
    posting_settings_type: 'by_time' | 'by_count' | 'none' | ''
    post_every_hours?: number,
    post_every_post?: number,
    ready_to_post?: boolean
};


interface CommonRelationProps {
    created_at: string
}

export interface ChannelRelatedNodesI {
    notificated: ModelRelatedNodesI<typeof Notification, NotificationInstance>

    post_templates: ModelRelatedNodesI<typeof PostTemplate, PostTemplateInstance, CommonRelationProps>;
    likes: ModelRelatedNodesI<typeof Channel, ChannelInstance, CommonRelationProps>;
    dislikes: ModelRelatedNodesI<typeof Channel, ChannelInstance, CommonRelationProps>;
    partners: ModelRelatedNodesI<typeof Channel, ChannelInstance, CommonRelationProps>
    user: ModelRelatedNodesI<typeof User, UserInstance>;
    scheduled_posts: ModelRelatedNodesI<typeof PostSchedule, PostScheduleInstance, PostScheduleProps>
}

export type ChannelInstance = NeogmaInstance<ChannelProps, ChannelRelatedNodesI, ChannelMethods>;

export interface ChannelMethods {
    like(channel: ChannelInstance): Promise<void>;
    dislike(channel: ChannelInstance): Promise<void>;
    findPartner(): Promise<ChannelInstance | undefined>;
    markAsReadyToPost(): Promise<void>
    markNotReadyToPost(): Promise<void>
}

export interface ChannelStatics {
    findReadyForMutualAdverts(): AsyncGenerator<ChannelInstance>
}

export const Channel = ModelFactory<ChannelProps, ChannelRelatedNodesI, ChannelStatics, ChannelMethods>(
    {
        label: 'Channel',
        schema: {
            channel_id: {
                type: 'string',
                required: true
            },
            ready_to_post: {
                type: 'boolean',
                required: false
            },
            posting_settings_type: {
                type: 'string',
                required: false,
                allowEmpty: true,
                default: ''
            },
            post_every_hours: {
                type: 'number',
                required: false,
                allowEmpty: true,
                default: () => 0
            },
            post_every_post: {
                type: 'number',
                required: false,
                allowEmpty: true,
                default: 4
            },
            uuid: {
                type: 'string',
                required: true,
            },
        },
        statics: {
            findReadyForMutualAdverts: async function*(): AsyncGenerator<ChannelInstance>  {
                const limit = 1
                let skip = 0
                while (true) {
                    const countQueryResult = await new QueryBuilder()
                        .match({
                            identifier: 'a',
                            model: Channel,
                            where: {
                                ready_to_post: true
                            }
                        })
                        .return('count(distinct a) as cnt')
                        .run(neogma.queryRunner);


                    const num = countQueryResult.records[0].get('cnt')

                    if (skip >= num) {
                        break;
                    }

                    const channels = await Channel.findMany({
                        where: {
                            ready_to_post: true,
                        },
                        order: [
                            ['uuid', 'ASC']
                        ],
                        limit: limit,
                        skip: skip
                    })

                    for (const channel of channels) {
                        yield channel;
                    }

                    skip += limit;
                }

            }
        },
        methods: {
            async markAsReadyToPost(): Promise<void> {
                const self = this as ChannelInstance
                self.ready_to_post = true
                await self.save()
            },

            async markNotReadyToPost() {
                const self = this as ChannelInstance
                self.ready_to_post = false
                await self.save()
            },

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


