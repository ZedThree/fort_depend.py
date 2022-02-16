import sys
import contextlib


@contextlib.contextmanager
def smart_open(filename, mode="Ur"):
    """Open stdin or stdout using a contextmanager

    From: http://stackoverflow.com/a/29824059/2043465

    Args:
        filename (str): name of file to open. Can be '-' for stdin/stdout
        mode (str): usual mode string for :py:func:`open`

    """
    if filename == "-":
        if mode is None or mode == "" or "r" in mode:
            fh = sys.stdin
        else:
            fh = sys.stdout
    else:
        fh = open(filename, mode)
    try:
        yield fh
    finally:
        if filename != "-":
            fh.close()
