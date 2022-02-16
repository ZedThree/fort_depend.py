import builtins
from fortdepend import FortranProject
from fortdepend.fort_depend import DEPFILE_HEADER
import pytest
import re
from unittest import mock


@pytest.mark.usefixtures("datadir")
class TestFortranProject:
    def test_basic_creation(self):
        testproject = FortranProject()
        assert testproject.name[:4] == "data"

    def test_get_single_file(self):
        testproject = FortranProject(files="moduleA.f90")
        expected_files = ["moduleA.f90"]

        assert sorted(testproject.files) == sorted(expected_files)

    def test_get_multiple_files(self):
        testproject = FortranProject(files=["moduleA.f90", "moduleB.f90"])
        expected_files = ["moduleA.f90", "moduleB.f90"]

        assert sorted(testproject.files) == sorted(expected_files)

    def test_get_all_files(self):
        testproject = FortranProject()
        expected_files = [
            "moduleA.f90",
            "moduleB.f90",
            "moduleC.f90",
            "moduleD.f90",
            "moduleE.f90",
            "multiple_modules.f90",
            "programTest.f90",
        ]

        assert sorted(testproject.files) == sorted(expected_files)

    def test_get_all_modules(self):
        testproject = FortranProject()
        expected_modules = [
            "modA",
            "modB",
            "modC",
            "modD",
            "modE",
            "modF",
            "modG",
            "modH",
            "progA",
            "test",
        ]

        assert sorted(testproject.modules) == sorted(expected_modules)

    def test_get_all_programs(self):
        testproject = FortranProject()
        expected_programs = [
            "progA",
            "test",
        ]

        assert sorted(testproject.programs) == sorted(expected_programs)

    def test_get_files_with_different_extension(self):
        testproject = FortranProject()
        files = testproject.get_source(extensions=".f08")
        expected_files = ["different_ext.f08"]

        assert sorted(files) == sorted(expected_files)

    def test_exclude_files(self):
        testproject = FortranProject(exclude_files="multiple_modules.f90")
        expected_files = [
            "moduleA.f90",
            "moduleB.f90",
            "moduleC.f90",
            "moduleD.f90",
            "moduleE.f90",
            "programTest.f90",
        ]

        assert sorted(testproject.files) == sorted(expected_files)

    def test_ignore_modules(self):
        testproject = FortranProject(
            files="multiple_modules.f90", ignore_modules="modF"
        )
        assert sorted(["modG", "modH", "progA"]) == sorted(testproject.modules.keys())
        assert [] == testproject.modules["modG"].uses
        assert sorted(["modG", "modH"]) == sorted(
            testproject.files["multiple_modules.f90"].uses
        )

    def test_depends_by_module(self):
        """This one is a little complicated...

        The dictionary depends_by_module uses FortranModule objects as
        keys. To save having to actually construct these, we instead
        need to reconstruct the dictionary with the repr strings as
        keys. Similarly for the values. We can then do comparisons on
        sets of strings.
        """
        testproject = FortranProject(exclude_files="multiple_modules.f90")
        reprs = {
            "modA": "FortranModule(module, 'moda', 'moduleA.f90')",
            "modB": "FortranModule(module, 'modb', 'moduleB.f90')",
            "modC": "FortranModule(module, 'modc', 'moduleC.f90')",
            "modD": "FortranModule(module, 'modd', 'moduleD.f90')",
            "modE": "FortranModule(module, 'mode', 'moduleE.f90')",
            "mpi": "FortranModule(module, 'mpi', 'empty')",
            "test": "FortranModule(program, 'test', 'programTest.f90')",
        }

        expected = {
            reprs["modA"]: [],
            reprs["modB"]: [reprs["modA"]],
            reprs["modC"]: [reprs["modA"], reprs["modB"]],
            reprs["modD"]: [reprs["modC"]],
            reprs["modE"]: [reprs["mpi"]],
            reprs["test"]: [reprs["modC"], reprs["modD"]],
        }

        depends_by_module_repr = {}
        for key, value in testproject.depends_by_module.items():
            depends_by_module_repr[repr(key)] = value

        for key, value in expected.items():
            assert key in depends_by_module_repr
            reprs = sorted([repr(foo) for foo in depends_by_module_repr[key]])
            assert reprs == sorted(value)

    def test_depends_by_file(self):
        """This one is a little complicated...

        The dictionary depends_by_file uses FortranFile objects as
        keys. To save having to actually construct these, we instead
        need to reconstruct the dictionary with the repr strings as
        keys. Similarly for the values. We can then do comparisons on
        sets of strings.
        """
        testproject = FortranProject(exclude_files="multiple_modules.f90")
        reprs = {
            "modA": "FortranFile('moduleA.f90')",
            "modB": "FortranFile('moduleB.f90')",
            "modC": "FortranFile('moduleC.f90')",
            "modD": "FortranFile('moduleD.f90')",
            "modE": "FortranFile('moduleE.f90')",
            "test": "FortranFile('programTest.f90')",
        }

        expected = {
            reprs["modA"]: [],
            reprs["modB"]: [reprs["modA"]],
            reprs["modC"]: [reprs["modA"], reprs["modB"]],
            reprs["modD"]: [reprs["modC"]],
            reprs["modE"]: [],
            reprs["test"]: [reprs["modC"], reprs["modD"]],
        }

        depends_by_file_repr = {}
        for key, value in testproject.depends_by_file.items():
            depends_by_file_repr[repr(key)] = value

        for key, value in expected.items():
            assert key in depends_by_file_repr
            reprs = sorted([repr(foo) for foo in depends_by_file_repr[key]])
            assert reprs == sorted(value)

    def test_write_depends(self, datadir):
        expected_contents = [
            DEPFILE_HEADER,
            "moduleA.o :",
            "moduleB.o : moduleA.o",
            "moduleC.o : moduleA.o moduleB.o",
            "moduleD.o : moduleC.o",
            "moduleE.o :",
            "multiple_modules.o :",
            "programTest.o : moduleC.o moduleD.o",
            "progA : multiple_modules.o",
            "test : moduleA.o moduleB.o moduleC.o moduleD.o programTest.o",
        ]

        testproject = FortranProject()
        testproject.write_depends()

        with open(str(datadir.join("makefile.dep")), "r") as f:
            contents = f.read()

        # A little manipulation to remove extraneous whitespace is
        # required in order for a clean comparison
        contents = contents.replace("\\\n\t", " ")
        contents = re.sub(r" +", " ", contents)
        contents = [
            line.lstrip().rstrip(" \t\n")
            for line in contents.splitlines()
            if line != ""
        ]

        assert sorted(expected_contents) == sorted(contents)

    def test_write_depends_overwrite(self, datadir):
        expected_contents = [
            DEPFILE_HEADER,
            "moduleA.o :",
            "moduleB.o : moduleA.o",
            "moduleC.o : moduleA.o moduleB.o",
            "moduleD.o : moduleC.o",
            "moduleE.o :",
            "programTest.o : moduleC.o moduleD.o",
            "test : moduleA.o moduleB.o moduleC.o moduleD.o programTest.o",
        ]

        FortranProject().write_depends()
        testproject = FortranProject(exclude_files="multiple_modules.f90")
        testproject.write_depends(overwrite=True)

        with open(str(datadir.join("makefile.dep")), "r") as f:
            contents = f.read()

        # A little manipulation to remove extraneous whitespace is
        # required in order for a clean comparison
        contents = contents.replace("\\\n\t", " ")
        contents = re.sub(r" +", " ", contents)
        contents = [
            line.lstrip().rstrip(" \t\n")
            for line in contents.splitlines()
            if line != ""
        ]

        assert sorted(expected_contents) == sorted(contents)

    def test_write_depends_no_overwrite(self, datadir):
        expected_contents = [
            DEPFILE_HEADER,
            "moduleA.o :",
            "moduleB.o : moduleA.o",
            "moduleC.o : moduleA.o moduleB.o",
            "moduleD.o : moduleC.o",
            "moduleE.o :",
            "multiple_modules.o :",
            "programTest.o : moduleC.o moduleD.o",
            "progA : multiple_modules.o",
            "test : moduleA.o moduleB.o moduleC.o moduleD.o programTest.o",
        ]

        FortranProject().write_depends()
        testproject = FortranProject(exclude_files="multiple_modules.f90")

        with mock.patch.object(builtins, "input", lambda x: "N"):
            testproject.write_depends(overwrite=False)

        with open(str(datadir.join("makefile.dep")), "r") as f:
            contents = f.read()

        # A little manipulation to remove extraneous whitespace is
        # required in order for a clean comparison
        contents = contents.replace("\\\n\t", " ")
        contents = re.sub(r" +", " ", contents)
        contents = [
            line.lstrip().rstrip(" \t\n")
            for line in contents.splitlines()
            if line != ""
        ]

        assert sorted(expected_contents) == sorted(contents)

    def test_write_depends_build(self, datadir):
        expected_contents = [
            DEPFILE_HEADER,
            "testdir/moduleA.o :",
            "testdir/moduleB.o : testdir/moduleA.o",
            "testdir/moduleC.o : testdir/moduleA.o testdir/moduleB.o",
            "testdir/moduleD.o : testdir/moduleC.o",
            "testdir/moduleE.o :",
            "testdir/multiple_modules.o :",
            "testdir/programTest.o : testdir/moduleC.o testdir/moduleD.o",
            "testdir/progA : testdir/multiple_modules.o",
            (
                "testdir/test : testdir/moduleA.o testdir/moduleB.o "  # long line
                + "testdir/moduleC.o testdir/moduleD.o testdir/programTest.o"
            ),
        ]

        testproject = FortranProject()
        testproject.write_depends(build="testdir")

        with open(str(datadir.join("makefile.dep")), "r") as f:
            contents = f.read()

        # A little manipulation to remove extraneous whitespace is
        # required in order for a clean comparison
        contents = contents.replace("\\\n\t", " ")
        contents = re.sub(r" +", " ", contents)
        contents = [
            line.lstrip().rstrip(" \t\n")
            for line in contents.splitlines()
            if line != ""
        ]

        assert sorted(expected_contents) == sorted(contents)

    def test_write_depends_skip_programs(self, datadir):
        expected_contents = [
            DEPFILE_HEADER,
            "moduleA.o :",
            "moduleB.o : moduleA.o",
            "moduleC.o : moduleA.o moduleB.o",
            "moduleD.o : moduleC.o",
            "moduleE.o :",
            "multiple_modules.o :",
            "programTest.o : moduleC.o moduleD.o",
        ]

        testproject = FortranProject()
        testproject.write_depends(skip_programs=True)

        with open(str(datadir.join("makefile.dep")), "r") as f:
            contents = f.read()

        # A little manipulation to remove extraneous whitespace is
        # required in order for a clean comparison
        contents = contents.replace("\\\n\t", " ")
        contents = re.sub(r" +", " ", contents)
        contents = [
            line.lstrip().rstrip(" \t\n")
            for line in contents.splitlines()
            if line != ""
        ]

        assert sorted(expected_contents) == sorted(contents)

    def test_get_all_used_modules(self):
        expected_used = {
            "progA": sorted(["modF", "modG", "modH"]),
            "test": sorted(["modA", "modB", "modC", "modD"]),
        }

        testproject = FortranProject()
        used_by_program = {}
        for program in testproject.programs.keys():
            used_by_program[program] = testproject.get_all_used_modules(program)

        assert expected_used == used_by_program

    def test_get_all_used_files(self):
        expected_used = {
            "progA": sorted(["multiple_modules.f90"]),
            "test": sorted(
                [
                    "moduleA.f90",
                    "moduleB.f90",
                    "moduleC.f90",
                    "moduleD.f90",
                    "programTest.f90",
                ]
            ),
        }

        testproject = FortranProject()
        used_by_program = {}
        for program in testproject.programs.keys():
            used_by_program[program] = testproject.get_all_used_files(program)

        assert expected_used == used_by_program
