import { ModelRelatedNodesI } from "neogma";
import { Channel, ChannelInstance } from "./channel";
import { Notification } from "./notification";
import { PostTemplate } from "./post_template";
import { User } from "./user";

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
    post_templates: {
        model: PostTemplate,
        direction: 'in',
        name: 'HAS_POST_TEMPLATE',
    },
    likes: {
        model: Channel,
        direction: 'out',
        name: 'LIKES',
    },
    dislikes: {
        model: Channel,
        direction: 'out',
        name: 'DISLIKES',
    },
});



// Channel to User (reverse)
Channel.addRelationships({
    partners: {
        model: Channel,
        direction: 'out',
        name: 'PARTERNS_WITH',
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

// Post to Channel (reverse)
PostTemplate.addRelationships({
    channel: {
        model: Channel,
        direction: 'in',
        name: 'HAS_POST',
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
