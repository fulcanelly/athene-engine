import { ModelFactory, NeogmaInstance } from "neogma";
import { neogma } from "../neo4j";

export type PostTemplateProps = {
    uuid: string;
};
// TODO: rename to post template
// TODO: add post logs
export type PostTemplateInstance = NeogmaInstance<PostTemplateProps, {}>;

export const PostTemplate = ModelFactory<PostTemplateProps, {}>(
    {
        label: 'Post',
        schema: {
            uuid: {
                type: 'string',
                required: true,
            },
        },
        primaryKeyField: 'uuid',
    },
    neogma,
);
