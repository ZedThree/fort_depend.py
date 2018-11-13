from distutils import dir_util
import pytest
import os


@pytest.fixture(scope="function")
def datadir(tmpdir_factory, request):
    """
    Fixture responsible for searching a folder with the same name of test
    module and, if available, moving all contents to a temporary directory so
    tests can use them freely.

    Adapted from http://stackoverflow.com/a/29631801/2043465
    """
    filename = request.module.__file__
    test_dir, _ = os.path.splitext(filename)

    if os.path.isdir(test_dir):
        tmpdir = tmpdir_factory.mktemp("data")
        dir_util.copy_tree(test_dir, str(tmpdir))

    tmpdir.chdir()
    return tmpdir
