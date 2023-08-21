
import { ModelFactory, ModelRelatedNodesI, NeogmaInstance } from 'neogma';
import { neogma } from '../neo4j';
import { Channel, ChannelInstance } from './channel';

export type UserProps = {
    uuid: string;
};

export interface UserRelatedNodesI {
    channels: ModelRelatedNodesI<typeof Channel, ChannelInstance>;
}


export interface UserStatics {
    create(): void
}

export type UserInstance = NeogmaInstance<UserProps, UserRelatedNodesI>;

export const User = ModelFactory<UserProps, UserRelatedNodesI, UserStatics>(
    {
        label: 'User',
        schema: {
            uuid: {
                type: 'string',
                required: true,
            },
        },
        statics: {
            create() {

            }
        },
        primaryKeyField: 'uuid',
    },
    neogma,
);


