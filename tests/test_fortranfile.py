from fortdepend.units import FortranFile


class TestFortranFile(object):
    @classmethod
    def setup_class(cls):
        pass

    def test_init(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert testfile.filename == "moduleA.f90"

    def test_print_name(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert str(testfile) == "moduleA.f90"

    def test_repr(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert repr(testfile) == "FortranFile('moduleA.f90')"

    def test_uses(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        assert testfile.uses == []

    def test_get_uses(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert testfile.uses is not None
        assert testfile.get_uses() == []

    def test_modules(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert str(testfile.modules) == result

    def test_get_modules(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        contents = ['module modA\n', 'end module modA\n']
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert testfile.modules is not None
        assert str(testfile.get_modules(contents)) == result
