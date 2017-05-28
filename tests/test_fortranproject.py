from fortdepend import FortranProject
import pytest


@pytest.mark.usefixtures("datadir")
class TestFortranProject:
    def test_basic_creation(self):
        testproject = FortranProject()
        assert testproject.name[:4] == "data"

    def test_get_single_file(self):
        testproject = FortranProject(files="moduleA.f90")
        expected_files = ["moduleA.f90"]

        assert set(testproject.files) == set(expected_files)

    def test_get_multiple_files(self):
        testproject = FortranProject(files=["moduleA.f90",
                                            "moduleB.f90"])
        expected_files = ["moduleA.f90",
                          "moduleB.f90"]

        assert set(testproject.files) == set(expected_files)

    def test_get_all_files(self):
        testproject = FortranProject()
        expected_files = ["moduleA.f90",
                          "moduleB.f90",
                          "moduleC.f90",
                          "moduleD.f90",
                          "moduleE.f90",
                          "multiple_modules.f90",
                          "programTest.f90"]

        assert set(testproject.files) == set(expected_files)

    def test_get_files_with_different_extension(self):
        testproject = FortranProject()
        files = testproject.get_source(extensions=".f08")
        expected_files = ["different_ext.f08"]

        assert set(files) == set(expected_files)

    def test_exclude_files(self):
        testproject = FortranProject(excludes="multiple_modules.f90")
        expected_files = ["moduleA.f90",
                          "moduleB.f90",
                          "moduleC.f90",
                          "moduleD.f90",
                          "moduleE.f90",
                          "programTest.f90"]

        assert set(testproject.files) == set(expected_files)

    def test_depends_by_module(self):
        """This one is a little complicated...

        The dictionary depends_by_module uses FortranModule objects as
        keys. To save having to actually construct these, we instead
        need to reconstruct the dictionary with the repr strings as
        keys. Similarly for the values. We can then do comparisons on
        sets of strings.
        """
        testproject = FortranProject(excludes="multiple_modules.f90")
        reprs = {"modA": "FortranModule(module, 'moda', 'moduleA.f90')",
                 "modB": "FortranModule(module, 'modb', 'moduleB.f90')",
                 "modC": "FortranModule(module, 'modc', 'moduleC.f90')",
                 "modD": "FortranModule(module, 'modd', 'moduleD.f90')",
                 "modE": "FortranModule(module, 'mode', 'moduleE.f90')",
                 "test": "FortranModule(program, 'test', 'programTest.f90')",
                 "iso_fortran_env": "FortranModule(module, 'iso_fortran_env', 'empty')"}

        expected = {reprs["modA"]: [],
                    reprs["modB"]: [reprs["modA"]],
                    reprs["modC"]: [reprs["modA"], reprs["modB"]],
                    reprs["modD"]: [],
                    reprs["modE"]: [],
                    reprs["test"]: [reprs["modC"], reprs["modD"],
                                    reprs["iso_fortran_env"]],
                    }

        depends_by_module_repr = {}
        for key, value in testproject.depends_by_module.items():
            depends_by_module_repr[repr(key)] = value

        for key, value in expected.items():
            assert key in depends_by_module_repr
            reprs = set([repr(foo) for foo in depends_by_module_repr[key]])
            assert reprs == set(value)

    def test_depends_by_file(self):
        """This one is a little complicated...

        The dictionary depends_by_file uses FortranFile objects as
        keys. To save having to actually construct these, we instead
        need to reconstruct the dictionary with the repr strings as
        keys. Similarly for the values. We can then do comparisons on
        sets of strings.
        """
        testproject = FortranProject(excludes="multiple_modules.f90")
        reprs = {"modA": "FortranFile('moduleA.f90')",
                 "modB": "FortranFile('moduleB.f90')",
                 "modC": "FortranFile('moduleC.f90')",
                 "modD": "FortranFile('moduleD.f90')",
                 "modE": "FortranFile('moduleE.f90')",
                 "test": "FortranFile('programTest.f90')",
                 }

        expected = {reprs["modA"]: [],
                    reprs["modB"]: [reprs["modA"]],
                    reprs["modC"]: [reprs["modA"], reprs["modB"]],
                    reprs["modD"]: [],
                    reprs["modE"]: [],
                    reprs["test"]: [reprs["modC"], reprs["modD"]],
                    }

        depends_by_file_repr = {}
        for key, value in testproject.depends_by_file.items():
            depends_by_file_repr[repr(key)] = value

        for key, value in expected.items():
            assert key in depends_by_file_repr
            reprs = set([repr(foo) for foo in depends_by_file_repr[key]])
            assert reprs == set(value)
