"""
Python test file

"""

import math

a_long_word = "..."

a_dict = {
    'a': 1,
    'b': 2,
    'c': 3,
}

def foo(toto: int) -> None:
    """Foo"""
    match toto:
        case 1:
            print('1')
        case _:
            print(' ')

def bar(
    x: int,
    y: int,
) -> None:
    """bar"""
    pass

"...".replace()
