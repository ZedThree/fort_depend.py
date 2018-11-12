from __future__ import print_function

import os
import sys

# Terminal colours
from colorama import Fore

from .smartopen import smart_open
from .units import FortranFile, FortranModule
from .graph import Graph

# Python 2/3 compatibility
try:
    input = raw_input
except NameError:
    pass

DEPFILE_HEADER = "# This file is generated automatically. DO NOT EDIT!"


class FortranProject:
    def __init__(self, name=None, exclude_files=None, files=None, ignore_modules=None,
                 macros=None, verbose=False):
        """Create a list of FortranFile objects

        Args:
            name: Name of the project (default: name of current directory)
            exclude_files: List of files to exclude
            files: List of files to include (default: all in current directory)
            ignore_modules: List of module names to ignore_mod (default: iso_c_binding and iso_fortran_env)
            macros: Dictionary of module names and replacement values
            verbose: Print more messages (default: False)
        """

        if name is None:
            self.name = os.path.basename(os.getcwd())
        else:
            self.name = name

        if files is None:
            files = self.get_source()
        elif not isinstance(files, list):
            files = [files]

        if exclude_files is not None:
            if not isinstance(exclude_files, list):
                exclude_files = [exclude_files]
            files = set(files) - set(exclude_files)

        self.files = {filename: FortranFile(filename, macros)
                      for filename in files}
        self.modules = self.get_modules()
        self.programs = {k: v for k, v in self.modules.items()
                         if v.unit_type == "program"}

        if ignore_modules is None:
            ignore_modules = ["iso_c_binding", "iso_fortran_env"]
        self.remove_ignored_modules(ignore_modules)

        self.depends_by_module = self.get_depends_by_module(verbose)
        self.depends_by_file = self.get_depends_by_file(verbose)

    def get_source(self, extensions=None):
        """Return all files ending with any of extensions (defaults to
        [".f90", ".F90"])
        """

        if extensions is None:
            extensions = [".f90", ".F90"]
        elif not isinstance(extensions, list):
            extensions = [extensions]

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
                    new_module = FortranModule(unit_type='module',
                                               name=used_mod)
                    graph.append(new_module)

                    print(Fore.RED + "Error" + Fore.RESET + " module " +
                          Fore.GREEN + used_mod + Fore.RESET +
                          " not defined in any files. Creating empty ",
                          file=sys.stderr)

            depends[module] = sorted(graph,
                                     key=lambda f: f.source_file.filename)

        if verbose:
            for module_ in sorted(depends.keys(), key=lambda f: f.source_file.filename):
                print(Fore.GREEN + module_.name + Fore.RESET +
                      " depends on :" + Fore.BLUE)
                for dep in depends[module_]:
                    print("\t" + dep.name)
                print(Fore.RESET)

        return depends

    def get_depends_by_file(self, verbose=False):
        """Get the dependencies of each file in file_list
        """
        depends = {}
        for source_file in self.files.values():
            graph = []
            for mod in source_file.uses:
                try:
                    mod_file = self.modules[mod].source_file
                    # Don't add self as a dependency
                    if mod_file.filename.lower() == source_file.filename.lower():
                        continue
                    graph.append(mod_file)
                except KeyError:
                    print(Fore.RED + "Error" + Fore.RESET + " module " + Fore.GREEN +
                          mod + Fore.RESET + " not defined in any files. Skipping...",
                          file=sys.stderr)
            depends[source_file] = sorted(graph,
                                          key=lambda f: f.filename)

        if verbose:
            for file_ in sorted(depends.keys(), key=lambda f: f.filename):
                print(Fore.GREEN + file_.filename + Fore.RESET +
                      " depends on :" + Fore.BLUE)
                for dep in depends[file_]:
                    print("\t" + dep.filename)
                print(Fore.RESET)

        return depends

    def get_all_used_files(self, module_name):
        """Get the complete set of files that module_name requires, either
        directly or indirectly

        """
        used_modules = self._get_all_used_modules(module_name, state=[])
        used_files = [self.modules[module].source_file.filename for module in used_modules]

        module_filename = self.modules[module_name].source_file.filename

        return sorted(set(used_files + [module_filename]))

    def get_all_used_modules(self, module_name):
        """Get the complete set of modules that module_name requires, either
        directly or indirectly

        """
        return self._get_all_used_modules(module_name, state=[])

    def _get_all_used_modules(self, module_name, state):
        """Implementation for get_all_used_modules and
        get_all_used_files. Needs to keep track of some additional
        state which doesn't need to be exposed to users

        """
        for module in self.modules[module_name].uses:
            try:
                if module in state:
                    continue
                state.append(module)
                if self.modules[module].uses:
                    state.extend(self._get_all_used_modules(module, state))
            except KeyError:
                print(Fore.RED + "Error" + Fore.RESET + " module " + Fore.GREEN +
                      module + Fore.RESET + " not defined in any files. Skipping...",
                      file=sys.stderr)

        return sorted(set(state))

    def write_depends(self, filename="makefile.dep", overwrite=False, build=''):
        """Write the dependencies to file

        Args:
            filename: Name of the output file
            overwrite: Overwrite existing dependency file [False]
            build: Directory to prepend to filenames
        """
        # Test file doesn't exist
        if os.path.exists(filename):
            if not(overwrite):
                print(Fore.RED + "Warning: file '{}' exists.".format(filename) +
                      Fore.RESET)
                opt = input("Overwrite? Y... for yes.")
                if opt.lower().startswith("y"):
                    pass
                else:
                    return

        with smart_open(filename, 'w') as f:
            f.write(DEPFILE_HEADER + "\n")
            alpha_list = sorted(self.depends_by_file.keys(),
                                key=lambda f: f.filename)
            for file_ in alpha_list:
                _, filename = os.path.split(file_.filename)
                objectname = os.path.splitext(filename)[0] + ".o"
                listing = "\n{} : ".format(os.path.join(build, objectname))
                for dep in self.depends_by_file[file_]:
                    _, depfilename = os.path.split(dep.filename)
                    depobjectname = os.path.splitext(depfilename)[0] + ".o"
                    listing += " \\\n\t{}".format(os.path.join(build, depobjectname))
                listing += "\n"
                f.write(listing)

    def make_graph(self, filename=None, format='svg', view=True):
        """Draw a graph of the project using graphviz

        Args:
            filename: Name of the output file
            format: Image format
            view: Immediately display the graph [True]
        """

        if filename is None:
            filename = self.name + ".dot"

        graph = Graph(self.depends_by_module, filename=filename,
                      format=format, view=view)
        graph.draw()

    def remove_ignored_modules(self, ignore_modules=None):
        """Remove the modules in iterable ignore_modules from
        all dependencies
        """
        if ignore_modules is None:
            return
        elif not isinstance(ignore_modules, list):
            ignore_modules = [ignore_modules]

        # Remove from module dict
        for ignore_mod in ignore_modules:
            self.modules.pop(ignore_mod, None)
            # Remove from 'used' modules
            for module in self.modules.values():
                try:
                    module.uses.remove(ignore_mod)
                except ValueError:
                    pass
            # Remove from 'used' files
            for source_file in self.files.values():
                try:
                    source_file.uses.remove(ignore_mod)
                except ValueError:
                    pass
