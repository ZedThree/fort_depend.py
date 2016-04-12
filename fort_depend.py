#!/usr/bin/env python
from itertools import chain
import os
import re


class FortranFile(object):
    """The modules and dependencies of a Fortran source file

    Args:
        filename: Source file
        macros: Dict of preprocessor macros to be expanded
    """
    def __init__(self, filename=None, macros={}):
        self.filename = filename
        self.uses = None
        self.modules = None
        self.depends_on = None

        with open(self.filename, 'r') as f:
            contents = f.readlines()

        self.modules = self.get_modules(contents)
        self.uses = self.get_uses(contents, macros)

    def __repr__(self):
        return "FortranFile('{}')".format(self.filename)

    def get_modules(self, contents):
        """Return all the modules or programs that are in the file

        Args:
            contents: Contents of the source file
        """

        p = re.compile("^\s*(?P<unit_type>module|program)\s*(?P<modname>\w*)",
                       re.IGNORECASE).match

        contains = {}

        for line in contents:
            found = p(line)
            if found:
                name = found.group('modname')
                contains[name] = FortranModule(unit_type=found.group('unit_type'),
                                               name=name,
                                               filename=self.filename)

        # Remove duplicates before returning
        return contains

    def get_uses(self, contents, macros={}):
        """Return which modules are used in the file after expanding macros

        Args:
            contents: Contents of the source file
            macros: Dict of preprocessor macros to be expanded
        """
        p = re.compile("^\s*use\s*(?P<moduse>\w*)\s*(, )?\s*(only)?\s*(:)?.*?$",
                       re.IGNORECASE).match

        uses = []

        for line in contents:
            found = p(line)
            if found:
                uses.append(found.group('moduse').strip())

        # Remove duplicates
        uniq_mods = list(set(uses))

        for i, mod in enumerate(uniq_mods):
            for k, v in macros.items():
                if re.match(k, mod, re.IGNORECASE):
                    uniq_mods[i] = mod.replace(k, v)

        return uniq_mods


class FortranModule(object):
    """A Fortran Module or Program

    unit_type: 'module' or 'program'
    name: Name of the module/program
    filename: Name of the file containing the module/program
    """
    def __init__(self, unit_type, name, filename):
        self.unit_type = unit_type.strip().lower()
        self.name = name.strip().lower()
        self.filename = filename

    def __repr__(self):
        return "FortranModule({}, '{}', '{}')".format(self.unit_type, self.name, self.filename)


def make_module_dict(file_list):
    """Merge dicts of FortranModules from list of FortranFiles

    Args:
        file_list: List of FortranFiles
    """

    result = {}
    for source_file in file_list:
        result.update(source_file.modules)
    return result


# Definitions
def run(files=None, verbose=True, overwrite=None, output=None, macros={}, build=''):

    file_list = create_file_list(files, macros)
    mod_list = make_module_dict(file_list)
    depends = get_depends(file_list, mod_dict=mod_list)

    if verbose:
        for i in depends.keys():
            print("\033[032m"+i+"\033[039m depends on :\033[034m")
            for j in depends[i]:
                print("\t"+j)
            print("\033[039m")

    if output is None:
        output = "makefile.dep"

    tmp = write_depend(outfile=output, dep=depends, overwrite=overwrite, build=build)

    return depends

def write_depend(outfile="makefile.dep", dep=[], overwrite=False, build=''):
    "Write the dependencies to outfile"
    # Test file doesn't exist
    if os.path.exists(outfile):
        if not(overwrite):
            print("\033[031mWarning file exists.\033[039m")
            opt = raw_input("Overwrite? Y... for yes.")
        else:
            opt = "y"
        if opt.lower().startswith("y"):
            pass
        else:
            return

    # Open file
    with open(outfile, 'w') as f:
        f.write('# This file is generated automatically. DO NOT EDIT!\n')
        for i in dep.keys():
            tmp, fil = os.path.split(i)
            stri = "\n"+os.path.join(build, fil.split(".")[0]+".o"+" : ")
            for j in dep[i]:
                tmp, fil = os.path.split(j)
                stri = stri+" \\\n\t"+os.path.join(build, fil.split(".")[0]+".o")
            stri = stri+"\n"
            f.write(stri)

    return


def get_source(ext=[".f90", ".F90"]):
    "Return all files ending with any of ext"
    tmp = os.listdir(".")
    fil = []
    for i in ext:
        fil.extend([x for x in tmp if x.endswith(i)])
    return fil


def create_file_list(files=None, macros={}):
    """Create a list of FortranFile objects
    """

    if files is None:
        files = get_source()

    return [FortranFile(filename, macros) for filename in files]


def get_depends(file_list=[], mod_dict={}):
    depends = {}
    for source_file in file_list:
        graph = []
        for mod in source_file.uses:
            try:
                graph.append(mod_dict[mod].filename)
            except KeyError:
                print("\033[031mError\033[039m module \033[032m"+mod+"\033[039m not defined in any files. Skipping...")

        depends[source_file.filename] = graph

    return depends

# Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='Generate Fortran dependencies')
    parser.add_argument('-f', '--files', nargs='+', help='Files to process')
    parser.add_argument('-D', nargs='+', action='append', metavar='NAME=DESCRIPTION',
                        help="""The macro NAME is replaced by DEFINITION in 'use' statements""")
    parser.add_argument('-b', '--build', nargs=1, default='',
                        help='Build Directory (prepended to all files in output)')
    parser.add_argument('-o', '--output', nargs=1, help='Output file')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='explain what is done')
    parser.add_argument('-w', '--overwrite', action='store_true',
                        help='Overwrite output file without warning')

    # Parse the command line arguments
    args = parser.parse_args()

    # Assemble a dictionary out of the macro definitions
    macros = {}
    if args.D:
        for arg in args.D:
            for var in arg:
                temp = var.split('=')
            macros[temp[0]] = temp[1]

    output = args.output[0] if args.output else None
    build = args.build[0] if args.build else ''

    run(files=args.files, verbose=args.verbose, overwrite=args.overwrite,
        macros=macros, output=output, build=build)
