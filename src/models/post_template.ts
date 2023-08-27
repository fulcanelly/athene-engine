import { ModelFactory, ModelRelatedNodesI, NeogmaInstance, RelationshipsI } from "neogma";
import { neogma } from "../neo4j";
import { Channel, ChannelInstance, ChannelProps } from "./channel";
import { PostLog, PostLogsInstance } from "./post_log";

export type PostTemplateProps = {
    uuid: string;
}

export interface PostTemplateRelatedNodesI {
    belongs_to: ModelRelatedNodesI<typeof Channel, ChannelInstance>
    instances: ModelRelatedNodesI<typeof PostLog, PostLogsInstance>
}

export interface PostTemplateMethods {
    postTo(channel: ChannelInstance | ChannelProps): Promise<void>
}

export interface PostTemplateStatics {

}
export type PostTemplateInstance = NeogmaInstance<PostTemplateProps, PostTemplateRelatedNodesI, PostTemplateMethods>;

export const PostTemplate = ModelFactory<PostTemplateProps, PostTemplateRelatedNodesI, PostTemplateStatics, PostTemplateMethods>(
    {
        label: 'PostTemplate',
        schema: {
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

PostTemplate.addRelationships({
    belongs_to: {
        model: Channel,
        direction: 'in',
        name: 'HAS_POST',
    },
    instances: {
        model: PostLog,
        direction: 'in',
        name: 'BASED_ON'
    }
});
