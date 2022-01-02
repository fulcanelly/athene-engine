#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
from typing import get_type_hints, Type, TypeVar

T = TypeVar('T')


class Args:
    def __init__(self, from_type: Type[T]):
        self.type = from_type
        self.parser = argparse.ArgumentParser(
            formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        )

        default = self.type()
        hints = get_type_hints(self.type, include_extras=True)
        for name, t in hints.items():
            assert hasattr(t, '__args__') and hasattr(t, '__metadata__'), \
                f'{self.type.__name__}.{name} should have Annotated type hint'

            args = list()
            if len(name) > 2:
                args.append(f'-{next(iter(name))}')
            args.append(f'--{name.replace("_", "-")}')

            typ, = t.__args__
            hlp, = t.__metadata__

            self.parser.add_argument(
                *args,
                type=typ,
                help=f'{hlp} (type: {typ.__name__})',
                default=getattr(default, name)
            )

    def parse_args(self) -> T:
        return self.type(**vars(self.parser.parse_args()))
