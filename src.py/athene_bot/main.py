#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import asyncio
from datetime import datetime

from telethon import functions  # type: ignore
from telethon import TelegramClient
from telethon.utils import parse_username  # type: ignore
from telethon.tl.types import Channel as TlChannel  # type: ignore
from telethon.errors import (  # type: ignore
    UserAlreadyParticipantError, RPCError
)

from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, ForeignKey, select
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine

from .args import Args
from .config import Config
from .log import get_logger
from . import __pkg_name__ as name
from .utils import aenumerate, no_nl, attrget, compose


PLUS_RE = re.compile(r'.*/\+([^/]+)$')
Base = declarative_base()


class Channel(Base):
    __tablename__ = 'channel'

    id = Column(Integer, nullable=False, primary_key=True)


class Post(Base):
    __tablename__ = 'post'

    id = Column(Integer, nullable=False, primary_key=True)

    channel_id = Column(
        Integer,
        ForeignKey('channel.id'),
        nullable=False, primary_key=True
    )

    timestamp = Column(Integer, nullable=False)


class Subs(Base):
    __tablename__ = 'subs'

    subs = Column(Integer, nullable=False)

    channel_id = Column(
        Integer,
        ForeignKey('channel.id'),
        nullable=False, primary_key=True
    )

    timestamp = Column(Integer, nullable=False, primary_key=True)


class Views(Base):
    __tablename__ = 'views'

    views = Column(Integer, nullable=False)

    post_id = Column(
        Integer,
        ForeignKey('post.id'),
        nullable=False, primary_key=True
    )

    channel_id = Column(
        Integer,
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

        self.log = get_logger(
            name,
            file=self.config.log_file,
            level=self.config.log_level,
            stderr=not self.config.log_no_stderr,
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

            posts = list(map(
                attrget('id'),
                map(
                    compose(next, iter),
                    await session.execute(
                        select(Post)
                        .filter(Post.channel_id == channel.id)
                        .order_by(Post.id.desc())
                        .limit(self.config.n_posts)
                    )
                )
            ))

            async for i, post in aenumerate(self.user.iter_messages(channel)):
                if i >= self.config.n_posts:
                    break

                if post.id not in posts:
                    session.add(Post(
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

    async def try_join(self, hash: str) -> bool:
        try:
            await self.user(functions.messages.ImportChatInviteRequest(hash))

        except UserAlreadyParticipantError:
            pass

        except RPCError:
            return False

        return True

    async def run(self) -> None:
        await self.user.start()
        self.log.info('%s started', self.user.__class__.__name__)

        async with self.sql.begin() as conn:
            await conn.run_sync(Base.metadata.create_all)

        while not False:  # TODO: optimize
            with self.config.channels.open() as fptr:
                for channel in map(no_nl, fptr):
                    if match := PLUS_RE.match(channel):
                        channel = f'https://t.me/joinchat/{match.group(1)}'

                    hash, is_invite = parse_username(channel)
                    if is_invite:
                        if not await self.try_join(hash):
                            self.log.warning('failed to join to %r', channel)
                            continue

                    chan = await self.user.get_entity(channel)

                    if not isinstance(chan, TlChannel):
                        self.log.warning('%r is not channel', channel)
                        continue

                    self.log.info('collecting posts from %r', channel)
                    await self.collect_channel(chan)

            self.log.info('done, waiting %.2fs', self.config.interval)
            await asyncio.sleep(self.config.interval)


def main() -> None:
    try:
        asyncio.get_event_loop().run_until_complete(
            Bot(Args(Config).parse_args()).run()
        )

    except KeyboardInterrupt:
        pass
