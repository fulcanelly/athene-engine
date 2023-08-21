import { makeChannelPartnersServiceTest } from "./partners";
import { makeChannelRatingTest } from "./likes";
import { describe, it, expect, test, beforeAll, beforeEach, afterAll} from '@jest/globals';
import { neogma } from "../src/neo4j";

describe('Athene', () => {
    makeChannelPartnersServiceTest()
    makeChannelRatingTest()

    afterAll(async () => {
        await neogma.driver.close()
    })
})
