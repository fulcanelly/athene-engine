import { QueryBuilder } from 'neogma';
import { Channel, ChannelProps } from '../src/models/channel';
import '../src/models/relations';
import { neogma } from '../src/neo4j';

export async function deleteAll() {
    await new QueryBuilder()
        .match({
            model: Channel,
            identifier: 'a'
        })
        .match({
            optional: true,
            related: [
                {
                    model: Channel,
                    identifier: 'a'
                },
                {
                    direction: 'none',
                    identifier: 'b'
                },
                {
                    identifier: 'c'
                }
            ]
        })
        .delete('a, b, c')
        .run(neogma.queryRunner)
}

export const defaultChannelParams = {
    posting_settings_type: '',
    post_every_hours: 0,
    post_every_post: 0
} as ChannelProps
