import { QueryBuilder, QueryRunner } from "neogma";
import { Channel, ChannelInstance, ChannelProps } from "../models/channel";
import { Notification } from "../models/notification";
import { neogma } from "../neo4j";

interface ChannelPartnerServiceI {
    createNewPartnersIfPresent(): Promise<void>;
}

export class ChannelPartnerService implements ChannelPartnerServiceI {

    //TODO: i think it can be done with single query
    //TODO: or wrap it in transaction
    async createNewPartnersIfPresent(): Promise<void> {
        let toBe = await this.detectToBePartners()

        while (toBe.length != 0) {
            const [first, last] = toBe

            await Notification.createNewPartnerNotification({
                to_notify_chan_id: first.channel_id,
                new_partner_chan_id: last.channel_id
            })

            await Notification.createNewPartnerNotification({
                to_notify_chan_id: last.channel_id,
                new_partner_chan_id: first.channel_id
            })

            await first.relateTo({
                alias: 'partners',
                where: {
                    channel_id: last.channel_id
                },

                properties: {
                    created_at: new Date().toString()
                },
                session: null
            })

            toBe = await this.detectToBePartners()

        }
        return
    }

    private async detectToBePartners(): Promise<ChannelInstance[]> {

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
            .where('NOT (a)-[:PARTNERS_WITH]-(b)')
            .return(['a', 'b'])
            .limit(1)
            .run(neogma.queryRunner)

        const first = QueryRunner.getResultProperties<ChannelProps>(result, 'a')
            .map(it => Channel.buildFromRecord({
                properties: it,
                labels: [Channel.getLabel()]
            }))

        const second = QueryRunner.getResultProperties<ChannelProps>(result, 'b')
            .map(it => Channel.buildFromRecord({
                properties: it,
                labels: [Channel.getLabel()]
            }))

        return [
            ...second, ...first
        ]

    }
}
