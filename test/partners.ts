import { describe, it, expect, test, beforeAll, beforeEach } from '@jest/globals';
import { defaultChannelParams, deleteAll } from './helperts';
import { Channel, ChannelInstance, ChannelProps } from '../src/models/channel';
import { Notification, NotificationProps } from '../src/models/notification';
import { QueryBuilder, QueryRunner } from 'neogma';
import { neogma } from '../src/neo4j';
import { ChannelPartnerService } from '../src/services/chan_partners';


export const makeChannelPartnersServiceTest = () => describe('Channel partner service', () => {

    beforeEach(deleteAll)


    it('when two channels liked each other, notifcation & partner created for both', async () => {

        const a = await Channel.createOne({
            ...defaultChannelParams,
            channel_id: '1',
            uuid: 'a',
        })

        const b = await Channel.createOne({
            ...defaultChannelParams,
            channel_id: '2',
            uuid: 'b',
        })

        await a.like(b)
        await b.like(a)

        await new ChannelPartnerService().createNewPartnersIfPresent()

        const reuslt1 = await new QueryBuilder()
            .match({
                related: [
                    {
                        model: Channel,
                        where: {
                            channel_id: a.channel_id
                        }
                    },
                    Channel.getRelationshipByAlias('notificated'),
                    {
                        model: Notification,
                        identifier: 'notification',
                        where: {
                            action: 'new_partnership'
                        }
                    }
                ]
            })
            .return('notification')
            .run(neogma.queryRunner)

        const first_chan_nots = QueryRunner.getResultProperties<NotificationProps>(reuslt1, 'notification')


        const reuslt2 = await new QueryBuilder()
            .match({
                related: [
                    {
                        model: Channel,
                        where: {
                            channel_id: b.channel_id
                        }
                    },
                    Channel.getRelationshipByAlias('notificated'),
                    {
                        model: Notification,
                        identifier: 'notification',
                        where: {
                            action: 'new_partnership'
                        }
                    }
                ]
            })
            .return('notification')
            .run(neogma.queryRunner)

        const second_chan_nots = QueryRunner.getResultProperties<NotificationProps>(reuslt2, 'notification')


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
                        ...Channel.getRelationshipByAlias('partners'),
                        identifier: 'rel',
                        direction: 'none',
                    },
                    {
                        model: Channel,
                        identifier: 'end',
                        where: {
                            channel_id: a.channel_id
                        }
                    }
                ]
            })
            .return(['start', 'rel', 'end'])
            .run(neogma.queryRunner)



        const start = QueryRunner.getResultProperties<ChannelProps>(result3, 'start')[0]
        const end =  QueryRunner.getResultProperties<ChannelProps>(result3, 'end')[0]

        expect(start.channel_id).toBe(b.channel_id)
        expect(end.channel_id).toBe(a.channel_id)


        expect(first_chan_nots.length).toBe(1)
        expect(second_chan_nots.length).toBe(1)
    })
})
