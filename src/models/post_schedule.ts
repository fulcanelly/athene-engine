import { ModelFactory, ModelRelatedNodesI, NeogmaInstance } from "neogma";
import { neogma } from "../neo4j";
import { Channel, ChannelInstance } from "./channel";
import { PostTemplate, PostTemplateInstance } from "./post_template";

export type PostScheduleProps = {
    uuid: string,
};


export interface PostScheduleRelatedNodesI {
    targeted_to: ModelRelatedNodesI<typeof Channel, ChannelInstance>
    instance_of: ModelRelatedNodesI<typeof PostTemplate, PostTemplateInstance>
}

export interface PostScheduleMethods {
}

export interface PostScheduleStatics {

}
export type PostScheduleInstance = NeogmaInstance<PostScheduleProps, PostScheduleRelatedNodesI, PostScheduleMethods>;

export const PostSchedule = ModelFactory<PostScheduleProps, PostScheduleRelatedNodesI, PostScheduleStatics, PostScheduleMethods>(
    {
        label: 'PostSchedule',
        schema: {
            uuid: {
                type: 'string',
                required: true,
            },
        },
        statics: {

        },
        methods: {

        },
        primaryKeyField: 'uuid',
    },
    neogma,
);


PostSchedule.addRelationships({
    targeted_to: {
        model: Channel,
        direction: 'out',
        name: 'TARGETED_TO'
    },
    instance_of: {
        model: PostTemplate,
        direction: 'out',
        name: 'BASED_ON'
    }
})


Channel.addRelationships({
    scheduled_posts: {
        model: PostSchedule,
        direction: 'in',
        name: 'TARGETED_TO'
    }
})

PostTemplate.addRelationships({

})
