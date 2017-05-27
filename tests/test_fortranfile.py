from fortdepend.units import FortranFile, FortranModule


class TestFortranFile(object):
    @classmethod
    def setup_class(cls):
        pass

    def test_init(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert testfile.filename == "moduleA.f90"
        assert testfile.filename != "moduleB.f90"

    def test_print_name(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert str(testfile) == "moduleA.f90"
        assert str(testfile) != "moduleB.f90"

    def test_repr(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert repr(testfile) == "FortranFile('moduleA.f90')"
        assert repr(testfile) != "FortranFile('moduleB.f90')"
        assert repr(testfile) != "FortranModule('moduleA.f90')"

    def test_uses(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        assert testfile.uses == []
        assert testfile.uses != None

    def test_get_uses(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert testfile.uses == None
        assert testfile.get_uses() == []

    def test_modules(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert str(testfile.modules) == result
        assert testfile.modules != None

    def test_get_modules(self, datadir):
        datadir.chdir()
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        contents = ['module modA\n', 'end module modA\n']
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert testfile.modules == None
        assert str(testfile.get_modules(contents)) == result
        
