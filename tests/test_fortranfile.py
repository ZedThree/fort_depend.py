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
        cls.testfile = FortranFile(filename="file.f90", readfile=False)

    def test_init(self):
        assert self.testfile.filename == "file.f90"

    def test_file_str(self):
        assert str(self.testfile) == "file.f90"

    def test_file_repr(self):
        assert repr(self.testfile) == "FortranFile('file.f90')"

    def test_get_uses(self):
        assert self.testfile.uses is None
        assert self.testfile.get_uses() == []

    def test_get_single_module(self):
        contents = ['module modA',
                    'end module modA']
        expected = {"modA": "FortranModule(module, 'moda', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_get_multiple_modules(self):
        contents = ['module modA',
                    'end module modA',
                    'module modB',
                    'end module modB']
        expected = {"modA": "FortranModule(module, 'moda', 'file.f90')",
                    "modB": "FortranModule(module, 'modb', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_get_program(self):
        contents = ['program progA', 'end program progA']
        expected = {"progA": "FortranModule(program, 'proga', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_get_program_and_multiple_modules(self):
        contents = ['program progA',
                    'end program progA',
                    'module modA',
                    'end module modA',
                    'module modB',
                    'end module modB']
        expected = {"modA": "FortranModule(module, 'moda', 'file.f90')",
                    "modB": "FortranModule(module, 'modb', 'file.f90')",
                    "progA": "FortranModule(program, 'proga', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value


@pytest.mark.usefixtures("datadir")
class TestReadFortranFile():
    def test_empty_uses(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        assert testfile.uses == []

    def test_single_modules(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert str(testfile.modules) == result

    def test_single_uses(self):
        testfile = FortranFile(filename="moduleB.f90", readfile=True)
        assert testfile.uses == ['modA']

    def test_multiple_uses(self):
        testfile = FortranFile(filename="moduleC.f90", readfile=True)
        assert testfile.uses == ['modA', 'modB']
