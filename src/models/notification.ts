import { ModelFactory, ModelRelatedNodesI, NeogmaInstance } from "neogma";
import { Channel, ChannelInstance } from "./channel";
import { neogma } from "../neo4j";
import { v4 as uuidv4 } from 'uuid';

type NotificationAction = 'new_partnership' | 'new_like'

export type NotificationProps = {
    created_at: string;
    updated_at: string;
    done: boolean;
    action: NotificationAction;
    uuid: string;
};

export interface NotificationRelatedNodesI {
    channel_to_notify: ModelRelatedNodesI<typeof Channel, ChannelInstance>;
    liked_by?: ModelRelatedNodesI<typeof Channel, ChannelInstance>;
    got_new_partner?: ModelRelatedNodesI<typeof Channel, ChannelInstance>;
}

export type NotificationInstance = NeogmaInstance<NotificationProps, NotificationRelatedNodesI>;

export interface NotificationStatics {
    create(obj : { action: NotificationAction, channel_id: string }): Promise<NotificationInstance>
    createLikeNotification(params: {
        liked_by_chan_id: string, target_chan_id: string}): Promise<NotificationInstance>


    createNewPartnerNotification(obj: {
        to_notify_chan_id: string, new_partner_chan_id: string
    }): Promise<NotificationInstance>
}

export const Notification = ModelFactory<NotificationProps, NotificationRelatedNodesI, NotificationStatics>(
    {
        label: 'Notification',
        statics: {
            async create({ action, channel_id }): Promise<NotificationInstance> {
                return await Notification.createOne({
                    created_at: new Date().toISOString(),
                    updated_at: new Date().toISOString(),
                    done: false,
                    action: action,
                    uuid: uuidv4(),
                    channel_to_notify: {
                        where: {
                            params: { channel_id }
                        }
                    }
                })
            },

            async createNewPartnerNotification({ new_partner_chan_id, to_notify_chan_id }): Promise<NotificationInstance> {
                return await Notification.createOne({
                    created_at: new Date().toISOString(),
                    updated_at: new Date().toISOString(),
                    done: false,
                    action: 'new_partnership',
                    uuid: uuidv4(),
                    channel_to_notify: {
                        where: {
                            params: {
                                channel_id: to_notify_chan_id
                            }
                        }
                    },
                    got_new_partner: {
                        where: {
                            params: {
                                channel_id: new_partner_chan_id
                            }
                        }
                    }
                })
            },

            async createLikeNotification({ liked_by_chan_id, target_chan_id }): Promise<NotificationInstance> {
                return await Notification.createOne({
                    created_at: new Date().toISOString(),
                    updated_at: new Date().toISOString(),
                    done: false,
                    action: 'new_like',
                    uuid: uuidv4(),
                    liked_by: {
                        where: {
                            params: {
                                channel_id: liked_by_chan_id
                            }
                        }
                    },
                    channel_to_notify: {
                        where: {
                            params: {
                                channel_id: target_chan_id
                            }
                        }
                    }
                })
            }
        },
        schema: {
            created_at: {
                type: 'string',
            },
            updated_at: {
                type: 'string',
            },
            done: {
                type: 'boolean',
            },
            action: {
                type: 'string',
            },
            uuid: {
                type: 'string',
                required: true,
            },
        },
        primaryKeyField: 'uuid',
    },
    neogma,
);
