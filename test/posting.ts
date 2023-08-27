import { describe, it, expect, test, beforeAll, beforeEach} from '@jest/globals';
import { defaultChannelParams, deleteAll } from './helperts';
import { Channel, ChannelInstance, ChannelProps } from '../src/models/channel';
import { Notification, NotificationProps } from '../src/models/notification';
import { QueryBuilder, QueryRunner } from 'neogma';
import { neogma } from '../src/neo4j';
import { ChannelPartnerService } from '../src/services/chan_partners';
import timekeeper from 'timekeeper';
import moment from 'moment';
import { PostTemplate, PostTemplateInstance } from '../src/models/post_template';

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

class PostingService {

    public async post(channel: ChannelInstance, post: PostTemplateInstance) {
        //TODO telegram
        //TODO add log
    }


}

export const makePostingTests = () => describe('Mutual ads posting of few partners channels', () => {
    const fixedTime = new Date('2093-08-21T21:42:06.583Z')
    beforeEach(() => {
        timekeeper.freeze(fixedTime);
    })

    it('Creates channels with posting settings', async () => {
        await Channel.createOne({
            uuid: '0',
            channel_id: '0',
            posting_settings_type: 'by_count'
        })
    })

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

    describe('With time settings', () => {



        describe('When no posting logs yet', () => {
            it('Schedules posts immediately', async() => {
                await new PostingSchedulerService().handle()
                // TODO expect scheduled posts for both at fixedTime
            })
        })

        describe('When last post was just now', () => {
            it('Schedules post to shifted time', async () => {
                await new PostingSchedulerService().handle()
                // TODO expect scheduled post at
                moment().add(3, 'hours').fromNow()

            })
        })

        describe('When one of channels posted just now but other not', () => {
            it('Schedules post to shifted time', async () => {

            })
        })

    })

    describe('With post by count settings', () => {

    })

    describe('With post by request', () => {
        describe('When no posting logs', () => {
            it('Submits new posts and creates logs', async () => {
                await Promise.all([
                    a.markAsReadyToPost(),
                    b.markAsReadyToPost()])

                await new PostingSchedulerService().handle()

                for await (const ch of Channel.findReadyForMutualAdverts()) {
                    console.log(ch)
                }



            })
        })
    })

    describe('When posting limit per day reached', () => {

    })


    it("returns time", async () => {
        console.log(moment().add(5, 'minutes').toDate())
        console.log(new Date())
    })

})

