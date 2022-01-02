#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import asyncio
# import logging  # TODO: add logging
from pathlib import Path
from datetime import datetime


from telethon import functions
from telethon import TelegramClient
from telethon.tl.types import Channel as TlChannel

from sqlalchemy import Column, Integer, ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine

from .args import Args
from .config import Config
from .utils import aenumerate


SELF_DIR = Path(__file__).absolute().parent


Base = declarative_base()


class Channel(Base):
    __tablename__ = 'channel'

    id = Column(Integer, nullable=False, primary_key=True)


class Post(Base):
    __tablename__ = 'post'

    id = Column(Integer, nullable=False, primary_key=True)

    channel_id = Column(
        ForeignKey('channel.id'),
        nullable=False, primary_key=True
    )

    timestamp = Column(Integer, nullable=False)


class Subs(Base):
    __tablename__ = 'subs'

    subs = Column(Integer, nullable=False)

    channel_id = Column(
        ForeignKey('channel.id'),
        nullable=False, primary_key=True
    )

    timestamp = Column(Integer, nullable=False, primary_key=True)


class Views(Base):
    __tablename__ = 'views'

    views = Column(Integer, nullable=False)

    post_id = Column(
        ForeignKey('post.id'),
        nullable=False, primary_key=True
    )

    channel_id = Column(
        ForeignKey('channel.id'),
        nullable=False, primary_key=True
    )

    timestamp = Column(Integer, nullable=False, primary_key=True)


class Bot:
    API_ID = 1082434
    API_HASH = '78315dcd0760c41f748d54f837497508'

    def __init__(self, config: Config):
        from urllib.parse import quote

        self.config = config
        self.config.check_paths()

        self.user = TelegramClient(
            str(self.config.session),
            self.API_ID, self.API_HASH,
        )

        self.sql = create_async_engine(
            f'sqlite+aiosqlite:///{quote(str(self.config.db))}'
        )

    async def collect_channel(self, channel: TlChannel) -> None:
        channel = (await self.user(
            functions.channels.GetFullChannelRequest(channel)
        )).full_chat

        now = int(datetime.utcnow().timestamp())

        async with AsyncSession(self.sql) as session, session.begin():
            await session.merge(Channel(id=channel.id))
            session.add(Subs(
                timestamp=now,
                channel_id=channel.id,
                subs=channel.participants_count,
            ))

            async for i, post in aenumerate(self.user.iter_messages(channel)):
                if i >= self.config.last_nth:
                    break

                await session.merge(Post(  # TODO: optimize
                    id=post.id,
                    channel_id=channel.id,
                    timestamp=int(post.date.timestamp()),
                ))

                if post.views is not None:
                    session.add(Views(
                        timestamp=now,
                        post_id=post.id,
                        views=post.views,
                        channel_id=channel.id,
                    ))

    async def run(self) -> None:
        await self.user.start()

        async with self.sql.begin() as conn:
            await conn.run_sync(Base.metadata.create_all)

        while not False:  # TODO: optimize
            with self.config.channels.open() as fptr:
                for channel in fptr:
                    channel = await self.user.get_entity(channel)
                    # TODO: handle private channels

                    if not isinstance(channel, TlChannel):
                        continue

                    await self.collect_channel(channel)

            await asyncio.sleep(self.config.interval)


def main() -> None:
    try:
        asyncio.get_event_loop().run_until_complete(
            Bot(Args().parse_args()).run()
        )

    except KeyboardInterrupt:
        pass
