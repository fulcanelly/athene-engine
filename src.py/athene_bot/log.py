# -*- coding: utf-8 -*-

import logging
from pathlib import Path
from typing import Optional, List, Union

from .type_helpers import to

TIMEFMT = '%d.%m.%Y %H:%M:%S'
LOGFMT = '%(asctime)s::%(levelname)s::%(name)s -> %(message)s'

LogLevelT = Union[int, str]


class LogLevel:
    def __init__(self, value: LogLevelT):
        self.value = value

    @property
    def value(self) -> LogLevelT:
        return self._value

    @value.setter
    def value(self, value: LogLevelT) -> None:
        self._value = to(LogLevelT, value)


def get_logger(
    name: str, level: LogLevel = LogLevel(logging.INFO),
    stderr: bool = True, file: Optional[Path] = None
) -> logging.Logger:
    logger = logging.getLogger(name)
    logger.setLevel(level.value)

    handlers: List[logging.Handler] = list()

    if stderr:
        handlers.append(logging.StreamHandler())

    if file is not None:
        handlers.append(logging.FileHandler(file))

    for handler in handlers:
        handler.setLevel(level.value)
        handler.setFormatter(
            logging.Formatter(LOGFMT, TIMEFMT)
        )

        logger.addHandler(handler)

    return logger
