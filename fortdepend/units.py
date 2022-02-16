from .preprocessor import FortranPreprocessor
import re

from .smartopen import smart_open

UNIT_REGEX = re.compile(
    r"^\s*(?P<unit_type>module(?!\s+procedure)|program)\s+(?P<modname>\w*)",
    re.IGNORECASE,
)
END_REGEX = re.compile(
    r"^\s*end\s*(?P<unit_type>module|program)\s+(?P<modname>\w*)?", re.IGNORECASE
)
USE_REGEX = re.compile(
    r"""^\s*use
(\s*,\s*intrinsic\s*)?(\s*::\s*|\s+)  # Valid separators between "use" and module name
(?P<moduse>\w*)                       # The module name
\s*(, )?\s*(only)?\s*(:)?.*?$         # Stuff that might follow the name
""",
    re.IGNORECASE | re.VERBOSE,
)


class FortranFile:
    """The modules and dependencies of a Fortran source file

    Args:
        filename (str): Source file
        macros (iterable): Dict of preprocessor macros to be expanded
        readfile (bool): Read and process the file [True]
        cpp_includes (list of str): List of directories to add to preprocessor search path
        use_preprocessor (bool): Preprocess the source file [True]

    """

    def __init__(
        self,
        filename=None,
        macros=None,
        readfile=True,
        cpp_includes=None,
        use_preprocessor=True,
    ):
        self.filename = filename
        self.uses = None
        self.modules = None
        self.depends_on = None

        if readfile:
            with smart_open(self.filename, "r") as f:
                contents = f.read()

            preprocessor = FortranPreprocessor()

            if macros:
                if isinstance(macros, dict):
                    for k, v in macros.items():
                        preprocessor.define("{} {}".format(k, v))
                else:
                    if not isinstance(macros, list):
                        macros = [macros]
                    for macro in macros:
                        if "=" in macro:
                            temp = macro.split("=")
                            preprocessor.define("{} {}".format(*temp))
                        else:
                            preprocessor.define(macro)

            if cpp_includes:
                if not isinstance(cpp_includes, list):
                    cpp_includes = [cpp_includes]
                for include_dir in cpp_includes:
                    preprocessor.add_path(include_dir)

            if use_preprocessor:
                contents = preprocessor.parse_to_string(contents, source=self.filename)

            self.modules = self.get_modules(contents.splitlines())
            self.uses = self.get_uses()

    def __str__(self):
        return self.filename

    def __repr__(self):
        return "FortranFile('{}')".format(self.filename)

    def get_modules(self, contents, macros=None):
        """Return all the modules or programs that are in the file

        Args:
            contents (str): Contents of the source file
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
            if (len(found_units) != len(starts)) or (len(starts) != len(ends)):
                error_string = (
                    "Unmatched start/end of modules in {} ({} begins/{} ends)".format(
                        self.filename, len(starts), len(ends)
                    )
                )
                raise ValueError(error_string)
            for unit, start, end in zip(found_units, starts, ends):
                name = unit.group("modname")
                contains[name] = FortranModule(
                    unit_type=unit.group("unit_type"),
                    name=name,
                    source_file=self,
                    text=(contents, start, end),
                    macros=macros,
                )

        # Remove duplicates before returning
        return contains

    def get_uses(self):
        """Return a sorted list of the modules this file USEs"""

        if self.modules is None:
            return []

        # flatten list of lists
        return sorted(
            set([mod for module in self.modules.values() for mod in module.uses])
        )


class FortranModule:
    """A Fortran Module or Program

    Args:
        unit_type (str): 'module' or 'program'
        name (str): Name of the module/program
        source_file (str): Name of the file containing the module/program
        text (tuple): Tuple containing source_file contents, and start and end lines of the module
        macros (dict): Any defined macros

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
            self.source_file = FortranFile(filename="empty", readfile=False)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "FortranModule({}, '{}', '{}')".format(
            self.unit_type, self.name, self.source_file.filename
        )

    def get_uses(self, contents, macros=None):
        """Return which modules are used in the file after expanding macros

        Args:
            contents (str): Contents of the source file
            macros (dict): Dict of preprocessor macros to be expanded

        """

        uses = []

        for line in contents[self.defined_at : self.end]:
            found = re.match(USE_REGEX, line)
            if found:
                uses.append(found.group("moduse").strip())

        # Remove duplicates
        uniq_mods = list(set(uses))

        if macros is not None:
            for i, mod in enumerate(uniq_mods):
                for k, v in macros.items():
                    if re.match(k, mod, re.IGNORECASE):
                        uniq_mods[i] = mod.replace(k, v)

        return uniq_mods
