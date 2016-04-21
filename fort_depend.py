#!/usr/bin/env python
import os
import re
from collections import defaultdict

try:
    input = raw_input
except NameError:
    pass

UNIT_REGEX = re.compile("^\s*(?P<unit_type>module|program)\s*(?P<modname>\w*)",
                        re.IGNORECASE)
END_REGEX = re.compile("^\s*end\s*(?P<unit_type>module|program)\s*(?P<modname>\w*)?",
                       re.IGNORECASE)
USE_REGEX = re.compile("^\s*use\s*(?P<moduse>\w*)\s*(, )?\s*(only)?\s*(:)?.*?$",
                       re.IGNORECASE)


class FortranProject(object):
    def __init__(self, files=None, macros={}, verbose=False):
        """Create a list of FortranFile objects
        """

        if files is None:
            files = self.get_source()

        self.files = {filename: FortranFile(filename, macros)
                      for filename in files}
        self.modules = self.get_modules()
        self.depends_by_module = self.get_depends_by_module(verbose)
        self.depends_by_file = self.get_depends_by_file(verbose)

    def get_source(self, extensions=[".f90", ".F90"]):
        "Return all files ending with any of ext"
        tmp = os.listdir(".")
        files = []
        for ext in extensions:
            files.extend([x for x in tmp if x.endswith(ext)])

        return files

    def get_modules(self):
        """Merge dicts of FortranModules from list of FortranFiles
        """

        mod_dict = {}
        for source_file in self.files.values():
            mod_dict.update(source_file.modules)
        return mod_dict

    def get_depends_by_module(self, verbose=False):
        """Get the dependencies of each file in file_list
        """
        depends = {}
        for module in self.modules.values():
            graph = []
            for used_mod in module.uses:
                try:
                    graph.append(self.modules[used_mod])
                except KeyError:
                    print("\033[031mError\033[039m module \033[032m"+
                          used_mod+"\033[039m not defined in any files. Skipping...")

            depends[module] = sorted(graph,
                                     key=lambda f: f.source_file.filename)
        if verbose:
            for file_ in depends.keys():
                print("\033[032m"+file_+"\033[039m depends on :\033[034m")
                for dep in depends[file_.filename]:
                    print("\t"+dep.filename)
                print("\033[039m")

        return depends

    def get_depends_by_file(self, verbose=False):
        """Get the dependencies of each file in file_list
        """
        # depends = defaultdict(list)
        # for module, dependencies in self.depends_by_module.items():
        #     for dependency in dependencies:
        #         depends[module.source_file].append(
        #             dependency.source_file)
        #     depends[module.source_file].sort(
        #         key=lambda f: f.filename)
        depends = {}
        for source_file in self.files.values():
            graph = []
            for mod in source_file.uses:
                try:
                    graph.append(self.modules[mod].source_file)
                except KeyError:
                    print("\033[031mError\033[039m module \033[032m"+
                          mod+"\033[039m not defined in any files. Skipping...")
            depends[source_file] = sorted(graph,
                                          key=lambda f: f.filename)

        return depends

    def write_depends(self, outfile="makefile.dep", overwrite=False, build=''):
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

        with open(outfile, 'w') as f:
            f.write('# This file is generated automatically. DO NOT EDIT!\n')
            alpha_list = sorted(self.depends_by_file.keys(),
                                key=lambda f: f.filename)
            for file_ in alpha_list:
                _, filename = os.path.split(file_.filename)
                listing = "\n"+os.path.join(build, filename.split(".")[0]+".o"+" : ")
                for dep in self.depends_by_file[file_]:
                    _, filename = os.path.split(dep.filename)
                    listing += " \\\n\t"+os.path.join(build, filename.split(".")[0]+".o")
                listing += "\n"
                f.write(listing)

    def get_graph(self, exclude=['hdf5', 'h5lt', 'mpi_f08']):
        # Start the graph
        graph = "digraph G {\n"

        for source_file in self.depends:
            for module in self.depends[source_file]:
                # Add the edges to the graph
                edge = '"' + source_file.filename + '" -> "' + module.filename + '";\n'
                graph += edge

        # Close the graph and return it
        graph = graph + "}"
        return graph


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

        self.modules = self.get_modules(contents, macros)
        self.uses = self.get_uses()

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

        # flatten list of lists
        return sorted(set([mod for module in self.modules.values()
                           for mod in module.uses]))


class FortranModule(object):
    """A Fortran Module or Program

    unit_type: 'module' or 'program'
    name: Name of the module/program
    filename: Name of the file containing the module/program
    """
    def __init__(self, unit_type, name, source_file, text, macros={}):
        self.unit_type = unit_type.strip().lower()
        self.name = name.strip().lower()
        self.source_file = source_file
        self.defined_at = text[1]
        self.end = text[2]

        self.uses = self.get_uses(text[0], macros)

    def __repr__(self):
        return "FortranModule({}, '{}', '{}')".format(self.unit_type, self.name, self.source_file.filename)

    def get_uses(self, contents, macros={}):
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


# Definitions
def run(files=None, verbose=False, overwrite=None, output=None, macros={}, build='',
        graph=False):

    project = FortranProject(files, macros, verbose)

    # if output is None:
    #     output = "makefile.dep"

    if output is not None:
        project.write_depends(outfile=output, overwrite=overwrite, build=build)

    if graph:
        print(project.get_graph())


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
    parser.add_argument('-g', '--graph', action='store_true',
                        help='Make a graph of the project')
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
        macros=macros, output=output, build=build, graph=args.graph)
