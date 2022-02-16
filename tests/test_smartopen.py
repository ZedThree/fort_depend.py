from fortdepend.smartopen import smart_open
import sys
import io


def test_smart_open_file_read(tmpdir):
    text = "testing smart_open with file"
    testfile = tmpdir.join("testfile.txt")
    testfile.write(text)

    with smart_open(str(testfile), "r") as f:
        output = f.read()

    assert output == text


def test_smart_open_file_write(tmpdir):
    testfile = tmpdir.join("testfile.txt")
    text = "testing smart_open with file"

    with smart_open(str(testfile), "w") as f:
        f.write(text)

    assert testfile.read() == text


def test_smart_open_stdin():
    text = "testing smart_open with stdin"

    oldstdin = sys.stdin
    try:
        sys.stdin = io.StringIO(text)
    except TypeError:
        # Python 2 needs unicode here
        sys.stdin = io.StringIO(unicode(text))

    with smart_open("-", "r") as f:
        output = f.read()

    sys.stdin = oldstdin
    assert output == text


def test_smart_open_stdout(capsys):
    text = "testing smart_open with stdout"

    with smart_open("-", "w") as f:
        f.write(text)

    output, _ = capsys.readouterr()
    assert output == text
