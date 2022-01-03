# -*- coding: utf-8 -*-

import logging
from pathlib import Path
from functools import partial
from dataclasses import dataclass, field

from typing import Annotated, Optional

import xdg

from .log import LogLevel
from .args import ArgOpts
from . import __pkg_name__ as name
from .utils import apply_attr, compose, notf, instanceof, attrget, collect


@dataclass
class Config:
    n_posts: Annotated[
        int, 'collect <N> posts from each channel', ArgOpts.no_long
    ] = 100

    interval: Annotated[
        float, 'delay in seconds between stat collections'
    ] = 60 * 60

    log_level: Annotated[
        LogLevel, 'log level'
    ] = logging.getLevelName(logging.INFO)

    log_file: Annotated[
        Optional[Path], 'also write logs to file', ArgOpts.no_short
    ] = None

    db: Annotated[
        Path, 'database path'
    ] = field(
        default_factory=lambda: xdg.xdg_data_home() / name / 'database.sql'
    )

    session: Annotated[
        Path, 'path to telegram session file'
    ] = field(
        default_factory=lambda: xdg.xdg_data_home() / name / 'bot.session'
    )

    channels: Annotated[
        Path, 'path to channels list'
    ] = field(
        default_factory=lambda: xdg.xdg_config_home() / name / 'channels.list'
    )

    def check_paths(self) -> None:
        collect(
            map(
                apply_attr('mkdir', parents=True, exist_ok=True),
                map(
                    attrget('parent'),
                    filter(
                        instanceof(Path),
                        map(
                            partial(getattr, self),
                            filter(
                                compose(notf, apply_attr('startswith', '_')),
                                dir(self)
                            )
                        )
                    )
                )
            )
        )
