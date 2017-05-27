from fortdepend.units import FortranFile
from distutils import dir_util
import pytest
import os


@pytest.fixture(scope="class")
def datadir(tmpdir_factory, request):
    filename = request.module.__file__
    test_dir, _ = os.path.splitext(filename)

    if os.path.isdir(test_dir):
        tmpdir = tmpdir_factory.mktemp("data")
        dir_util.copy_tree(test_dir, str(tmpdir))

    tmpdir.chdir()
    return tmpdir


class TestEmptyFortranFile():
    def test_empty(self):
        empty = FortranFile(filename='empty', readfile=False)
        assert empty.filename == "empty"
        assert empty.uses is None
        assert empty.modules is None
        assert empty.depends_on is None


class TestSimpleFortranFile():
    @classmethod
    def setup_class(cls):
        cls.testfile = FortranFile(filename="moduleA.f90", readfile=False)

    def test_init(self):
        assert self.testfile.filename == "moduleA.f90"

    def test_file_str(self):
        assert str(self.testfile) == "moduleA.f90"

    def test_file_repr(self):
        assert repr(self.testfile) == "FortranFile('moduleA.f90')"

    def test_get_uses(self):
        assert self.testfile.uses is None
        assert self.testfile.get_uses() == []

    def test_get_modules(self):
        contents = ['module modA\n', 'end module modA\n']
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert self.testfile.modules is None
        assert str(self.testfile.get_modules(contents)) == result


@pytest.mark.usefixtures("datadir")
class TestReadFortranFile():
    def test_uses(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        assert testfile.uses == []

    def test_modules(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert str(testfile.modules) == result
