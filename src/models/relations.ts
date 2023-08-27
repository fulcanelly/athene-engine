import { ModelRelatedNodesI, RelationshipPropertiesI, RelationshipsI } from "neogma";
import { Channel, ChannelInstance, ChannelRelatedNodesI } from "./channel";
import { Notification } from "./notification";
import { PostTemplate, PostTemplateRelatedNodesI } from "./post_template";
import { User, UserRelatedNodesI } from "./user";
import { PostLog } from "./post_log";

// Relationships
User.addRelationships({
    channels: {
        model: Channel,
        direction: 'out',
        name: 'HAS_CHANNEL',
    },
});

Channel.addRelationships({
    notificated: {
        model: Notification,
        direction: 'in',
        name: 'CHANNEL_TO_NOTIFY'
    },
    partners: {
        model: Channel,
        direction: 'out',
        name: 'PARTNERS_WITH', //PARTERNS_WITH
        properties: {
            created_at: {
                property: 'created_at' as any,
                schema: {
                    type: 'any',
                    required: true
                }
            }
        }
    },
    user: {
        model: User,
        direction: 'in',
        name: 'HAS_CHANNEL',
    },
    post_templates: {
        model: PostTemplate,
        direction: 'out',
        name: 'HAS_POST_TEMPLATE',
    },
    likes: {
        model: Channel,
        direction: 'out',
        name: 'LIKES',
        properties: {
            created_at: {
                property: 'created_at' as any,
                schema: {
                    type: 'any',
                    required: true
                }
            }
        }
    },
    dislikes: {
        model: Channel,
        direction: 'out',
        name: 'DISLIKES',
        properties: {
            created_at: {
                property: 'created_at' as any,
                schema: {
                    type: 'any',
                    required: true
                }
            }
        }
    },
});






Notification.addRelationships({
    channel_to_notify: {
        model: Channel,
        direction: 'out',
        name: 'CHANNEL_TO_NOTIFY',
    },
    liked_by: {
        model: Channel,
        direction: 'out',
        name: 'LIKED_BY',
    },
    got_new_partner: {
        model: Channel,
        direction: 'out',
        name: 'GOT_NEW_PARTNER',
    },
});
