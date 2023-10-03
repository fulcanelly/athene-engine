import { makeChannelPartnersServiceTest } from "./partners";
import { makeChannelRatingTest } from "./likes";
import { describe, it, expect, test, beforeAll, beforeEach, afterAll} from '@jest/globals';
import { neogma } from "../src/neo4j";
import { makePostingTests } from "./posting";
import * as dotenv from 'dotenv';

dotenv.config();

describe('Athene', () => {
    makeChannelPartnersServiceTest()
    makeChannelRatingTest()
    makePostingTests()

    afterAll(async () => {
        await neogma.driver.close()
    })
})
