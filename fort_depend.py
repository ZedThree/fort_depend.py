#!/usr/bin/python
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
        self.uses = []
        self.contains = None
        self.depends_on = None

        with open(self.filename, 'r') as f:
            contents = f.readlines()

        self.uses = self.get_uses(contents, macros)
        self.contains = self.get_contains(contents)

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

    def get_contains(self, contents):
        "Return all the modules that are in infile"

        p = re.compile("^\s*module\s*(?P<modname>\w*)", re.IGNORECASE).match

        contains = []

        for line in contents:
            found = p(line)
            if found:
                contains.append(found.group('modname').strip())

        # Remove duplicates before returning
        return list(set(contains))


# Definitions
def run(files=None, verbose=True, overwrite=None, output=None, macros={}, build=''):

    l = create_file_objs(files, macros)
    mod2fil = file_objs_to_mod_dict(file_objs=l)
    depends = get_depends(fob=l, m2f=mod2fil)

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
            opt = input("Overwrite? Y... for yes.")
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


def create_file_objs(files=None, macros={}):
    """Create a list of FortranFile objects
    """

    if files is None:
        files = get_source()

    return [FortranFile(filename, macros) for filename in files]


def file_objs_to_mod_dict(file_objs=[]):
    "Turn a list of file_objs in a dictionary, containing which modules depend on which files"
    dic = {}
    for i in file_objs:
        for j in i.contains:
            dic[j.lower()] = i.filename
    return dic


def get_depends(fob=[], m2f=[]):
    deps = {}
    for i in fob:
        tmp = []
        for j in i.uses:
            try:
                tmp.append(m2f[j.lower()])
            except:
                print("\033[031mError\033[039m module \033[032m"+j+"\033[039m not defined in any files. Skipping...")

        deps[i.filename] = tmp

    return deps

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
