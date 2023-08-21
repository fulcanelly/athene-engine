import { Op, QueryBuilder, QueryRunner, Where } from "neogma";
import { Channel, ChannelProps } from "./models/channel";
import  "./models/relations";
import { neogma } from "./neo4j";



async function test() {



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

    const a = await Channel.createOne({
        channel_id: '1',
        uuid: 'a-'
    })

    a.findRelationships({
        alias: 'likes'
    })

    const b = await Channel.createOne({
        channel_id: '2',
        uuid: 'b-'
    })

    const c = await Channel.createOne({
        channel_id: '3',
        uuid: 'c-'
    })


    const d = await Channel.createOne({
        channel_id: '4',
        uuid: 'd-'
    })


    await a.like(b)
    await a.dislike(c)


    //result <> init,
    const qu = await new QueryBuilder()
        .match({
            related: [
                {
                    model: Channel,
                    identifier: 'init',
                    where: {
                        channel_id: '1'
                    },
                },
            ],
        })
        .match({
            model: Channel,
            identifier: 'result',
        })
        .where('not (init)-[:LIKES]->(result) and not (init)-[:DISLIKES]->(result) and init <> result')
        .return('result')
        .limit(1)
        .run(neogma.queryRunner)

    console.log(
        QueryRunner.getResultProperties<ChannelProps>(qu, 'result')
    )

    console.log(qu)

    const x = QueryRunner.getResultProperties<ChannelProps>(qu, 'result')

    const res = qu
    const that = Channel.buildFromRecord({
        properties: QueryRunner.getResultProperties<ChannelProps>(qu, 'result')[0],
        labels: ['Channel']
    })

    await that.save()

    console.log(that)

    let chan = await Channel!.findOne({
        where: x[0]
    })
    console.log(chan)
    return
    // that.channel_id = 'dfsdf'
    // let chan = await Channel!.findOne({
    //     where: x[0]
    // })

    // if (chan) {
    //     chan.channel_id = 'df'
    //     await chan.save()
    // }


}

test()
// import { setupConstraints } from "./neo4j";
// import { sentry } from "./sentry";
// import "./data/relations";
// import { setupRmq } from "./rmq";


// async function main() {
//     try {
//         await setupConstraints()
//         await setupRmq()
//     } catch (e) {
//         sentry.captureException(e)
//     }
// }

// main()
