from fortdepend.units import FortranFile, FortranModule
from nose.tools import assert_equal, assert_not_equal


class TestFortranFile(object):
    @classmethod
    def setup_class(cls):
        pass

    def test_init(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert_equal(testfile.filename, "moduleA.f90")
        assert_not_equal(testfile.filename, "moduleB.f90")

    def test_print_name(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert_equal(str(testfile), "moduleA.f90")
        assert_not_equal(str(testfile), "moduleB.f90")

    def test_repr(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert_equal(repr(testfile), "FortranFile('moduleA.f90')")
        assert_not_equal(repr(testfile), "FortranFile('moduleB.f90')")
        assert_not_equal(repr(testfile), "FortranModule('moduleA.f90')")

    def test_uses(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        assert_equal(testfile.uses, [])
        assert_not_equal(testfile.uses, None)

    def test_get_uses(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        assert_equal(testfile.uses, None)
        assert_equal(testfile.get_uses(), [])

    def test_modules(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert_equal(str(testfile.modules), result)
        assert_not_equal(testfile.modules, None)

    def test_get_modules(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=False)
        contents = ['module modA\n', 'end module modA\n']
        result = "{'modA': FortranModule(module, 'moda', 'moduleA.f90')}"
        assert_equal(testfile.modules, None)
        assert_equal(str(testfile.get_modules(contents)), result)
        
