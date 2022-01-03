# -*- coding: utf-8 -*-

from typing import Any, TypeVar, Union, get_args

T = TypeVar('T')


def to(typ: Any, value: Union[T]) -> Union[T]:
    types = get_args(typ)

    for t in types:
        try:
            return t(value)  # type: ignore

        except ValueError:
            pass

    else:
        raise ValueError(f'invalid value for {typ}: {value!r}')
