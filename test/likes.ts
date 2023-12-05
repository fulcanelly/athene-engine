import { describe, it, expect, test, beforeAll, beforeEach, afterAll} from '@jest/globals';
import * as jest from '@jest/core';

import { QueryBuilder } from 'neogma';
import { Channel } from '../src/models/channel';

import { neogma } from '../src/neo4j';
import { defaultChannelParams, deleteAll } from './helperts';

export const makeChannelRatingTest = () => describe('Channel rating',  () => {


    beforeAll(()=> {

    })

    beforeEach(async () => {
        console.log("CLEANING BASE")
        await deleteAll()
        console.log('done')
    }, 100_000)

    it('should get a channel that is not rated yet', async () => {
        console.log('start')
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

        const c = await Channel.createOne({
            ...defaultChannelParams,
            channel_id: '3',
            uuid: 'c',
        })

        const d = await Channel.createOne({
            ...defaultChannelParams,
            channel_id: '4',
            uuid: 'd',
        })

        await a.like(b);
        await a.dislike(c)

        const rel = await a.findRelationships({
            alias: 'likes',
            limit: 1
        })

        const potentialPartner = await a.findPartner()
        expect(potentialPartner?.channel_id).toEqual('4');
    });

    it.todo('should not give any channel since they don\'t have common category')

    it('should create notification when channel is liked', async () => {
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

        const notifications = await b.findRelationships({
            alias: 'notificated',
        })

        expect(notifications.length).toBe(1)

        const notification = notifications[0]

        expect(notification.source.channel_id).toEqual(b.channel_id)
        expect(notification.target.action).toEqual('new_like')
        expect(notification.target.done).toEqual(false)

    })


});
