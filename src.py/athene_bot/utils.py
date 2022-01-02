# -*- coding: utf-8 -*-

import functools
from typing import (
    Any, AsyncIterator,
    AsyncIterable, Tuple,
    Callable, Union, Iterable, TypeVar,
)


async def aenumerate(
    aiter: AsyncIterable[Any], start: int = 0
) -> AsyncIterator[Tuple[int, Any]]:
    async for i in aiter:
        yield start, i
        start += 1


def is_some(x: Any) -> bool:
    return x is not None


def notf(x: Any) -> bool:
    return not x


def compose(*functions: Callable[[Any], Any]) -> Any:
    T1 = TypeVar('T1')
    T2 = TypeVar('T2')
    T3 = TypeVar('T3')

    def compose2(
        f: Callable[[T2], T3], g: Callable[[T1], T2]
    ) -> Callable[[T1], T3]:
        return lambda x: f(g(x))

    return functools.reduce(compose2, functions, lambda x: x)


def apply_attr(func: str, *args: Any, **kwargs: Any) -> Callable[[Any], Any]:
    return lambda x: getattr(x, func)(*args, **kwargs)


def instanceof(t: Union[type, Tuple[type]]) -> Callable[[Any], bool]:
    return lambda x: isinstance(x, t)


def attrget(attr: str) -> Any:
    return lambda x: getattr(x, attr)


def collect(i: Iterable[Any]) -> None:
    for _ in i:
        pass
