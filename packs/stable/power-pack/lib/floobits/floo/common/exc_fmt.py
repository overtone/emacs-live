#!/usr/local/bin/python
# coding: utf-8
import sys
import warnings
import traceback

try:
    unicode()
except NameError:
    unicode = None


def str_e(e):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        message = getattr(e, "message", None)
        if not (message and unicode):
            return str(e)
        try:
            return unicode(message, "utf8").encode("utf8")
        except Exception:
            return message.encode("utf8")


def pp_e(e):
    """returns a str in all pythons everywher"""
    # py3k has __traceback__
    tb = getattr(e, "__traceback__", None)
    if tb is not None:
        return "\n".join(traceback.format_tb(tb)) + str_e(e)

    # in case of sys.exc_clear()
    _, _, tb = sys.exc_info()
    if tb is not None:
        return "\n".join(traceback.format_tb(tb)) + str_e(e)

    return str_e(e)


if __name__ == "__main__":
    def test(excp):
        try:
            raise excp
        except Exception as e:
            stre = str_e(e)
            assert isinstance(stre, str)
            print(stre)

    def test2(excp):
        try:
            raise excp
        except Exception as e:
            sys.exc_clear()
            stre = str_e(e)
            assert isinstance(stre, str)
            print(stre)

    tests = [Exception("asdf"), Exception(u"aß∂ƒ"), Exception(u"asdf"), Exception(b"asdf1234")]
    for t in tests:
        test(t)
        if getattr(sys, "exc_clear", None):
            test2(t)
