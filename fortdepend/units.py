import re

from .smartopen import smart_open

UNIT_REGEX = re.compile("^\s*(?P<unit_type>module|program)\s*(?P<modname>\w*)",
                        re.IGNORECASE)
END_REGEX = re.compile("^\s*end\s*(?P<unit_type>module|program)\s*(?P<modname>\w*)?",
                       re.IGNORECASE)
USE_REGEX = re.compile("^\s*use(\s*,\s*intrinsic\s*::)?\s*(?P<moduse>\w*)\s*(, )?\s*(only)?\s*(:)?.*?$",
                       re.IGNORECASE)


class FortranFile(object):
    """The modules and dependencies of a Fortran source file

    Args:
        filename: Source file
        macros: Dict of preprocessor macros to be expanded
        readfile: Read and process the file [True]
    """
    def __init__(self, filename=None, macros=None, readfile=True):
        self.filename = filename
        self.uses = None
        self.modules = None
        self.depends_on = None

        if readfile:
            with smart_open(self.filename, 'r') as f:
                contents = f.readlines()

            self.modules = self.get_modules(contents, macros)
            self.uses = self.get_uses()

    def __str__(self):
        return self.filename

    def __repr__(self):
        return "FortranFile('{}')".format(self.filename)

    def get_modules(self, contents, macros={}):
        """Return all the modules or programs that are in the file

        Args:
            contents: Contents of the source file
        """

        contains = {}
        found_units = []
        starts = []
        ends = []

        for num, line in enumerate(contents):
            unit = re.match(UNIT_REGEX, line)
            end = re.match(END_REGEX, line)
            if unit:
                found_units.append(unit)
                starts.append(num)
            if end:
                ends.append(num)

        if found_units:
            if len(found_units) != len(starts) != len(ends):
                error_string = "Unmatched start/end of modules in {} ({} begins/{} ends)".format(
                    self.filename, len(starts), len(ends))
                raise ValueError(error_string)
            for unit, start, end in zip(found_units, starts, ends):
                name = unit.group('modname')
                contains[name] = FortranModule(unit_type=unit.group('unit_type'),
                                               name=name,
                                               source_file=self,
                                               text=(contents, start, end),
                                               macros=macros)

        # Remove duplicates before returning
        return contains

    def get_uses(self):
        """Return a sorted list of the modules this file USEs
        """

        if self.modules is None:
            return []

        # flatten list of lists
        return sorted(set([mod for module in self.modules.values()
                           for mod in module.uses]))


class FortranModule(object):
    """A Fortran Module or Program

    unit_type: 'module' or 'program'
    name: Name of the module/program
    source_file: Name of the file containing the module/program
    text: Tuple containing source_file contents, and start and end lines of the module
    macros: Any defined macros
    """
    def __init__(self, unit_type, name, source_file=None, text=None, macros=None):
        self.unit_type = unit_type.strip().lower()
        self.name = name.strip().lower()

        if source_file is not None:
            self.source_file = source_file
            self.defined_at = text[1]
            self.end = text[2]

            self.uses = self.get_uses(text[0], macros)
        else:
            self.source_file = FortranFile(filename='empty',
                                           readfile=False)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "FortranModule({}, '{}', '{}')".format(self.unit_type, self.name, self.source_file.filename)

    def get_uses(self, contents, macros=None):
        """Return which modules are used in the file after expanding macros

        Args:
            contents: Contents of the source file
            macros: Dict of preprocessor macros to be expanded
        """

        uses = []

        for line in contents[self.defined_at:self.end]:
            found = re.match(USE_REGEX, line)
            if found:
                uses.append(found.group('moduse').strip())

        # Remove duplicates
        uniq_mods = list(set(uses))

        for i, mod in enumerate(uniq_mods):
            for k, v in macros.items():
                if re.match(k, mod, re.IGNORECASE):
                    uniq_mods[i] = mod.replace(k, v)

        return uniq_mods
