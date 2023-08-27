import { Neogma, QueryBuilder } from "neogma";

export const neogma = new Neogma(
    {
        url: 'bolt://' + process.env.NEO4J_HOST,

        username: process.env.NEO4J_USERNAME as string,
        password: process.env.NEO4J_PASSWORD as string,
    },
    {
        logger: console.log,
    },
);


export async function setupConstraints() {
    let constraints = await neogma.queryRunner.run("SHOW CONSTRAINTS")

    if (!constraints.records.find(record => record.get('name') == 'uniq_user_id')) {
        await neogma.queryRunner.run( "CREATE CONSTRAINT uniq_user_id FOR (u:User) REQUIRE u.user_id IS UNIQUE")
    }

}
