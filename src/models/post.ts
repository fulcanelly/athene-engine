import { ModelFactory, NeogmaInstance } from "neogma";
import { neogma } from "../neo4j";

export type PostProps = {
    uuid: string;
};
// TODO: rename to post template
// TODO: add post logs
export type PostInstance = NeogmaInstance<PostProps, {}>;

export const Post = ModelFactory<PostProps, {}>(
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
