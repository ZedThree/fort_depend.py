from fortdepend.units import FortranModule, FortranFile

class TestFortranModule():
    def test_canonical_name(self):
        module = FortranModule(' MODULE ', ' NAME ')
        assert module.name == 'name'

    def test_canonical_unit_type_module(self):
        module = FortranModule(' MODULE ', ' NAME ')
        assert module.unit_type == 'module'

    def test_canonical_unit_type_program(self):
        program = FortranModule(' PROGRAM ', ' NAME ')
        assert program.unit_type == 'program'

    def test_get_use_of_single_module(self):
        file_contents = [
            "module name",
            "use module1",
            "end module"]
        start = 1
        end = len(file_contents)
        source_file = FortranFile(filename='empty',
                                  readfile=False)

        module = FortranModule(unit_type='module',
                               name='name',
                               source_file=source_file,
                               text=(file_contents, start, end),
                               macros=None)

        assert module.uses == ['module1']

    def test_get_use_of_multiple_modules(self):
        file_contents = [
            "module name",
            "use module1",
            "use module2",
            "use module3",
            "end module"]
        start = 1
        end = len(file_contents)
        source_file = FortranFile(filename='empty',
                                  readfile=False)

        module = FortranModule(unit_type='module',
                               name='name',
                               source_file=source_file,
                               text=(file_contents, start, end),
                               macros=None)

        assert set(module.uses) == set(['module1', 'module2', 'module3'])

    def test_get_use_of_repeated_modules(self):
        file_contents = [
            "module name",
            "use module1",
            "use module2",
            "use module3",
            "use module1",
            "end module"]
        start = 1
        end = len(file_contents)
        source_file = FortranFile(filename='empty',
                                  readfile=False)

        module = FortranModule(unit_type='module',
                               name='name',
                               source_file=source_file,
                               text=(file_contents, start, end),
                               macros=None)

        assert set(module.uses) == set(['module1', 'module2', 'module3'])
