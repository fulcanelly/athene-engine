import { makeChannelPartnersServiceTest } from "./partners";
import { makeChannelRatingTest } from "./likes";
import { describe } from '@jest/globals';
import { makePostingTests } from "./posting";



describe('Athene', () => {
    makeChannelPartnersServiceTest()
    makeChannelRatingTest()
    makePostingTests()
})
