# -*- coding: utf-8 -*-

from typing import (
    Any, AsyncIterator, AsyncIterable, Tuple, Callable, Union, Iterable,
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


def compose(*functions):
    def compose2(f, g):
        return lambda x: f(g(x))
    return __import__('functools').reduce(compose2, functions, lambda x: x)


def apply_attr(func: str, *args, **kwargs) -> Callable[[Any], Any]:
    return lambda x: getattr(x, func)(*args, **kwargs)


def instanceof(t: Union[type, Tuple[type]]) -> Callable[[Any], bool]:
    return lambda x: isinstance(x, t)


def attrget(attr: str) -> Any:
    return lambda x: getattr(x, attr)


def collect(i: Iterable) -> None:
    for _ in i:
        pass
