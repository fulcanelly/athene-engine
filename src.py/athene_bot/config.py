# -*- coding: utf-8 -*-

import logging
from pathlib import Path
from operator import not_
from functools import partial
from typing import Annotated, Optional
from dataclasses import dataclass, field

import xdg

from .log import LogLevel
from .args import ArgOpts
from . import __pkg_name__ as name
from .utils import apply_attr, compose, instanceof, attrget, collect


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

    log_no_stderr: Annotated[
        bool, "don't log to stderr", ArgOpts.store_true, ArgOpts.no_short
    ] = False

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
                                compose(not_, apply_attr('startswith', '_')),
                                dir(self)
                            )
                        )
                    )
                )
            )
        )
