import { QueryBuilder } from 'neogma';
import { Channel, ChannelProps } from '../src/models/channel';
import '../src/models/relations';
import { neogma } from '../src/neo4j';

export async function deleteAll() {
    await neogma.queryRunner.run("MATCH (a:Channel) DETACH DELETE a")
}

export const defaultChannelParams = {
    posting_settings_type: '',
    post_every_hours: 0,
    post_every_post: 0
} as ChannelProps
