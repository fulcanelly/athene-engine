# -*- coding: utf-8 -*-

import enum
import argparse
from typing import (
    Annotated, Type, Union,
    get_type_hints, get_origin,
)

from .type_helpers import T


class ArgOpts(enum.Enum):
    no_long = enum.auto()
    no_short = enum.auto()


class Args:
    def __init__(self, from_type: Type[T]):
        self.type = from_type
        self.parser = argparse.ArgumentParser(
            formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        )

        default = self.type()
        hints = get_type_hints(self.type, include_extras=True)
        for name, t in hints.items():
            assert get_origin(t) is Annotated, \
                f'{self.type.__name__}.{name} should have Annotated type hint'

            typ, *_ = t.__args__
            hlp, *metadata = t.__metadata__

            args = list()
            if ArgOpts.no_short not in metadata and len(name) > 2:
                args.append(f'-{next(iter(name))}')

            if ArgOpts.no_short in metadata or ArgOpts.no_long not in metadata:
                args.append(f'--{name.replace("_", "-")}')

            self.parser.add_argument(
                *args,
                type=typ,
                dest=name,
                help=f'{hlp} (type: {self.type_name(typ)})',
                default=getattr(default, name)
            )

    @staticmethod
    def type_name(typ: type) -> str:
        return str(typ) if get_origin(typ) is Union else typ.__name__

    def parse_args(self) -> T:
        return self.type(**vars(self.parser.parse_args()))
