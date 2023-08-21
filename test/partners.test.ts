import { describe, it, expect, test, beforeAll, beforeEach} from '@jest/globals';
import { deleteAll } from './helperts';
import { Channel, ChannelInstance, ChannelProps } from '../src/models/channel';
import { Notification, NotificationProps } from '../src/models/notification';
import { QueryBuilder, QueryRunner } from 'neogma';
import { neogma } from '../src/neo4j';

interface ChannelPartnerServiceI {

    detectToBePartners(): Promise<ChannelInstance[]>;
    createNewPartnersIfPresent(): Promise<void>;
}

class ChannelPartnerService implements ChannelPartnerServiceI {

    async createNewPartnersIfPresent(): Promise<void> {

        let toBe = await this.detectToBePartners()

        while (!toBe.length) {
            const [first, last] = toBe

            await Promise.all([
                Notification.createNewPartnerNotification({
                    to_notify_chan_id: first.channel_id,
                    new_partner_chan_id: last.channel_id
                }),
                Notification.createNewPartnerNotification({
                    to_notify_chan_id: last.channel_id,
                    new_partner_chan_id: first.channel_id
                })
            ])
            // return
        }

    }

    async detectToBePartners(): Promise<ChannelInstance[]> {

        //check channels that likes each other
        const result = await new QueryBuilder()
            .match({
                related: [
                    {
                        model: Channel,
                        identifier: 'a'
                    },
                    Channel.getRelationshipByAlias('likes'),
                    {
                        model: Channel,
                        identifier: 'b'
                    }
                ]
            })
            .match({
                related: [
                    {
                        model: Channel,
                        identifier: 'b'
                    },
                    Channel.getRelationshipByAlias('likes'),
                    {
                        model: Channel,
                        identifier: 'a'
                    }
                ]
            })
            //todo, check that they not partners yet
            .return(['a', 'b'])
            .limit(1)
            .run(neogma.queryRunner)

        const first = QueryRunner.getResultProperties<ChannelProps>(result, 'a')
            .map(it => Channel.buildFromRecord({
                properties: it,
                labels: [ Channel.getLabel() ]
            }))

        const second = QueryRunner.getResultProperties<ChannelProps>(result, 'b')
            .map(it => Channel.buildFromRecord({
                properties: it,
                labels: [ Channel.getLabel() ]
            }))

        return [
            ...second, ...first
        ]

    }
}
describe('Channel partner service', () => {

    beforeEach(deleteAll)

    it('when two channels liked each other, notifcation & partner created for both', async () => {

        const a = await Channel.createOne({
            channel_id: '1',
            uuid: 'a'
        })

        const b = await Channel.createOne({
            channel_id: '2',
            uuid: 'b'
        })

        await a.like(b)
        await b.like(a)

        const part = await new ChannelPartnerService().detectToBePartners()

        console.log(part)

        const reuslt = await new QueryBuilder()
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

        const nots = QueryRunner.getResultProperties<NotificationProps>(reuslt, 'notification')

        console.log(nots)
        // const notificatiion = await Notification.findOne({
        //     where: {
        //         action: 'new_partnership',
        //         k: {

        //         }
        //     },
        // })

        expect(nots).toBe({})

        // a.findRelationships({
        //     alias: 'notificated',
        //     where: {
        //         relationship: ,

        //     }
        // })

    })
})
