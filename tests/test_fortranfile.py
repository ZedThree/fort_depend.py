from fortdepend.units import FortranFile
from distutils import dir_util
import pytest
import os


class TestEmptyFortranFile:
    def test_empty(self):
        empty = FortranFile(filename="empty", readfile=False)
        assert empty.filename == "empty"
        assert empty.uses is None
        assert empty.modules is None
        assert empty.depends_on is None


class TestSimpleFortranFile:
    @classmethod
    def setup_class(cls):
        cls.testfile = FortranFile(filename="file.f90", readfile=False)

    def test_init(self):
        assert self.testfile.filename == "file.f90"

    def test_file_str(self):
        assert str(self.testfile) == "file.f90"

    def test_file_repr(self):
        assert repr(self.testfile) == "FortranFile('file.f90')"

    def test_get_empty_uses(self):
        assert self.testfile.uses is None
        assert self.testfile.get_uses() == []

    def test_get_single_module(self):
        contents = ["module modA", "end module modA"]
        expected = {"modA": "FortranModule(module, 'moda', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_get_multiple_modules(self):
        contents = ["module modA", "end module modA", "module modB", "end module modB"]
        expected = {
            "modA": "FortranModule(module, 'moda', 'file.f90')",
            "modB": "FortranModule(module, 'modb', 'file.f90')",
        }

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_module_with_module_procedure(self):
        contents = ["module modA", "module procedure foo", "end module modA"]
        expected = {"modA": "FortranModule(module, 'moda', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_get_program(self):
        contents = ["program progA", "end program progA"]
        expected = {"progA": "FortranModule(program, 'proga', 'file.f90')"}

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_get_program_and_multiple_modules(self):
        contents = [
            "program progA",
            "end program progA",
            "module modA",
            "end module modA",
            "module modB",
            "end module modB",
        ]
        expected = {
            "modA": "FortranModule(module, 'moda', 'file.f90')",
            "modB": "FortranModule(module, 'modb', 'file.f90')",
            "progA": "FortranModule(program, 'proga', 'file.f90')",
        }

        assert self.testfile.modules is None
        module_list = self.testfile.get_modules(contents)

        for key, value in expected.items():
            assert key in module_list
            assert repr(module_list[key]) == value

    def test_catch_unmatched_begin_end(self):
        contents = ["module modA"]
        with pytest.raises(ValueError):
            self.testfile.get_modules(contents)

    def test_catch_unmatched_begin_end_2(self):
        contents = ["module modA", "end module", "end module"]
        with pytest.raises(ValueError):
            self.testfile.get_modules(contents)


@pytest.mark.usefixtures("datadir")
class TestReadFortranFile:
    def test_empty_uses(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        assert testfile.uses == []

    def test_get_single_module(self):
        testfile = FortranFile(filename="moduleA.f90", readfile=True)
        expected = {"modA": "FortranModule(module, 'moda', 'moduleA.f90')"}

        for key, value in expected.items():
            assert key in testfile.modules
            assert repr(testfile.modules[key]) == value

    def test_get_program_and_multiple_modules(self):
        testfile = FortranFile(filename="multiple_modules.f90", readfile=True)
        expected = {
            "modA": "FortranModule(module, 'moda', 'multiple_modules.f90')",
            "modB": "FortranModule(module, 'modb', 'multiple_modules.f90')",
            "modC": "FortranModule(module, 'modc', 'multiple_modules.f90')",
            "modD": "FortranModule(module, 'modd', 'multiple_modules.f90')",
            "progA": "FortranModule(program, 'proga', 'multiple_modules.f90')",
        }

        for key, value in expected.items():
            assert key in testfile.modules
            assert repr(testfile.modules[key]) == value

    def test_single_uses(self):
        testfile = FortranFile(filename="moduleB.f90", readfile=True)
        assert testfile.uses == ["modA"]

    def test_multiple_uses(self):
        testfile = FortranFile(filename="moduleC.f90", readfile=True)
        assert set(testfile.uses) == set(["modA", "modB"])

    def test_multiple_uses_in_multiple_units(self):
        testfile = FortranFile(filename="multiple_modules.f90", readfile=True)
        assert set(testfile.uses) == set(
            ["modA", "modB", "modC", "modD", "iso_c_binding"]
        )

    def test_macro_replacement_dict(self):
        testfile = FortranFile(
            filename="moduleC.f90",
            readfile=True,
            macros={"modA": "module_A", "modB": "module_B"},
        )
        assert sorted(testfile.uses) == sorted(["module_A", "module_B"])

    def test_macro_replacement_list(self):
        testfile = FortranFile(
            filename="moduleC.f90",
            readfile=True,
            macros=["modA=module_A", "modB=module_B"],
        )
        assert sorted(testfile.uses) == sorted(["module_A", "module_B"])

    def test_macro_replacement_single_value(self):
        testfile = FortranFile(
            filename="moduleC.f90", readfile=True, macros="modA=module_A"
        )
        assert sorted(testfile.uses) == sorted(["module_A", "modB"])

    def test_conditional_include(self):
        testfile = FortranFile(
            filename="preprocessor.f90", readfile=True, macros=["FOO"]
        )
        assert testfile.uses == ["foo"]

        testfile2 = FortranFile(filename="preprocessor.f90", readfile=True)
        assert sorted(testfile2.uses) == sorted(["bar", "rawr"])

    def test_no_preprocessor(self):
        testfile = FortranFile(
            filename="preprocessor.f90",
            readfile=True,
            macros=["FOO"],
            use_preprocessor=False,
        )
        assert sorted(testfile.uses) == sorted(["bar", "foo", "rawr"])

        testfile2 = FortranFile(
            filename="preprocessor.f90", readfile=True, use_preprocessor=False
        )
        assert sorted(testfile2.uses) == sorted(["bar", "foo", "rawr"])

    def test_include_file(self):
        testfile = FortranFile(
            filename="preprocessor_include_file.F90",
            readfile=True,
            macros=["CAT"],
            cpp_includes="some_include_dir",
        )
        assert testfile.uses == ["cat"]

        testfile2 = FortranFile(filename="preprocessor_include_file.F90", readfile=True)
        assert sorted(testfile2.uses) == sorted(["dog", "goat"])
