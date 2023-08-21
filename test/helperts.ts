import { QueryBuilder } from 'neogma';
import { Channel } from '../src/models/channel';
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
