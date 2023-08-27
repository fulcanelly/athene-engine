import { describe, it, expect, test, beforeAll, beforeEach} from '@jest/globals';
import { defaultChannelParams, deleteAll } from './helperts';
import { Channel, ChannelInstance, ChannelProps } from '../src/models/channel';
import { Notification, NotificationProps } from '../src/models/notification';
import { QueryBuilder, QueryRunner } from 'neogma';
import { neogma } from '../src/neo4j';
import { ChannelPartnerService } from '../src/services/chan_partners';
import timekeeper from 'timekeeper';
import moment from 'moment';
import { PostTemplate } from '../src/models/post_template';
import { PostLog } from '../src/models/post_log';

class PostingSchedulerService {
    constructor() {

    }

    async handle(): Promise<void> {
        await this.handleReadyToPost()
    }

    protected async handleReadyToPost() {
        for await (const ch of Channel.findReadyForMutualAdverts()) {
            console.log(ch)
        }
    }
}

export const makePostingTests = () => describe('Mutual ads posting of few partners channels', () => {
    let a: ChannelInstance, b: ChannelInstance

    beforeEach(async () => {
        await deleteAll()

        const aTemplatePost = await PostTemplate.createOne({
            uuid: 'p1'
        })

        const bTemplatePost = await PostTemplate.createOne({
            uuid: 'p2'
        })

        a = await Channel.createOne({
            uuid: 'a',
            channel_id: '1',
            posting_settings_type: 'by_time',
            post_every_hours: 3,
        })

        b = await Channel.createOne({
            uuid: 'b',
            channel_id: '2',
            posting_settings_type: 'by_time',
            post_every_hours: 3
        })

        await a.relateTo({
            alias: 'post_templates',
            where: {
                uuid: aTemplatePost.uuid
            }
        })

        await b.relateTo({
            alias: 'post_templates',
            where: {
                uuid: bTemplatePost.uuid
            }
        })

        await Promise.all([
                a.like(b),
                b.like(a),
            ])

        await new ChannelPartnerService().createNewPartnersIfPresent()
    })

    describe('With post by request', () => {
        describe('When no posting logs', () => {
            it('Submits new posts and creates logs', async () => {
                await Promise.all([
                    a.markAsReadyToPost(),
                    b.markAsReadyToPost()])

                await new PostingSchedulerService().handle()

                // for await (const ch of Channel.findReadyForMutualAdverts()) {
                //     console.log(ch)
                // }


                const result3 = await new QueryBuilder()
                    .match({
                        related: [
                            {
                                model: Channel,
                                identifier: 'start',
                                where: {
                                    channel_id: b.channel_id
                                }
                            },
                            {
                                ...Channel.getRelationshipByAlias('post_templates'),
                                identifier: 'rel',
                                direction: 'none',
                            },
                            {
                                model: PostLog,
                                identifier: 'end',
                                where: {
                                    channel_id: a.channel_id
                                }
                            }
                        ]
                    })
                    .return(['start', 'rel', 'end'])
                    .run(neogma.queryRunner)



                expect(2)
            })
        })
    })
})

makePostingTests()
