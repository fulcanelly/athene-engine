import { ModelFactory, ModelRelatedNodesI, NeogmaInstance } from "neogma";
import { neogma } from "../neo4j";
import { Channel, ChannelInstance } from "./channel";
import { PostTemplate, PostTemplateInstance } from "./post_template";

export type PostLogsProps = {
    uuid: string,
    type: 'regular' | 'advert' | 'sheudled_advert'
};


export interface PostLogRelatedNodesI {
    posted_to: ModelRelatedNodesI<typeof Channel, ChannelInstance>
    instance_of: ModelRelatedNodesI<typeof PostTemplate, PostTemplateInstance>
}

export interface PostLogsMethods {
}

export interface PostLogsStatics {

}
export type PostLogsInstance = NeogmaInstance<PostLogsProps, PostLogRelatedNodesI, PostLogsMethods>;

export const PostLog = ModelFactory<PostLogsProps, PostLogRelatedNodesI, PostLogsStatics, PostLogsMethods>(
    {
        label: 'PostLog',
        schema: {
            type: {
                type: 'string',
                required: true
            },
            uuid: {
                type: 'string',
                required: true,
            },
        },
        statics: {

        },
        methods: {
            async postTo(channel) {
                throw 'not implemented'
            }
        },
        primaryKeyField: 'uuid',
    },
    neogma,
);


PostLog.addRelationships({
    posted_to: {
        model: Channel,
        direction: 'out',
        name: 'POSTED_TO'
    },
    instance_of: {
        model: PostTemplate,
        direction: 'out',
        name: 'BASED_ON'
    }
})
