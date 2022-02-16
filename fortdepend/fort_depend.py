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
DEFAULT_IGNORED_MODULES = ["iso_c_binding", "iso_fortran_env"]


class FortranProject:
    """Read a set of Fortran source files and produce a set of
    `FortranFile`, `FortranModule` and the dependencies between them

    Example:

        This is the main class for interacting with a Fortran project. The
        minimal "useful" thing is:

        >>> import fortdepend
        >>> my_project = fortdepend.FortranProject()
        >>> my_project.write_depends()

        This will read all the .f90 and .F90 files in the current
        directory and write the dependencies to "makefile.dep"

    Args:
        name (str): Name of the project (default: name of current directory)
        exclude_files (list of str): List of files to exclude
        files (list of str): List of files to include (default: all in current directory)
        ignore_modules (list of str): List of module names to ignore_mod
                                      (default: iso_c_binding and iso_fortran_env)
        macros (dict, list or str): Preprocessor macro definitions
        cpp_includes (list of str): List of directories to add to preprocessor search path
        use_preprocessor (bool): Use the preprocessor (default: True)
        verbose (bool): Print more messages (default: False)

    """

    def __init__(
        self,
        name=None,
        exclude_files=None,
        files=None,
        ignore_modules=None,
        macros=None,
        cpp_includes=None,
        use_preprocessor=True,
        verbose=False,
    ):

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

        self.files = {
            filename: FortranFile(
                filename=filename,
                macros=macros,
                readfile=True,
                cpp_includes=cpp_includes,
                use_preprocessor=use_preprocessor,
            )
            for filename in files
        }
        self.modules = self.get_modules()
        self.programs = {
            k: v for k, v in self.modules.items() if v.unit_type == "program"
        }

        self.remove_ignored_modules(ignore_modules)

        self.depends_by_module = self.get_depends_by_module(verbose)
        self.depends_by_file = self.get_depends_by_file(verbose)

    def get_source(self, extensions=None):
        """Return a list of filenames ending with any of extensions

        Args:
            extensions: List of file extensions (defaults to [".f90", ".F90"])
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
        """Return a dict of all the modules found in the project

        Works by iterating over the list of `FortranFile` and merging
        their dicts of `FortranModule`

        Returns:
            dict of module name (str) and `FortranModule` objects

        """

        mod_dict = {}
        for source_file in self.files.values():
            mod_dict.update(source_file.modules)
        return mod_dict

    def get_depends_by_module(self, verbose=False):
        """Return the set of which modules each module directly depends on

        Args:
            verbose: Print progress messages

        Returns:
            dict of `FortranModule` and a list of `FortranModule`

        """
        depends = {}
        for module in self.modules.values():
            graph = []
            for used_mod in module.uses:
                try:
                    graph.append(self.modules[used_mod])
                except KeyError:
                    new_module = FortranModule(unit_type="module", name=used_mod)
                    graph.append(new_module)

                    print(
                        Fore.RED
                        + "Error"
                        + Fore.RESET
                        + " module "
                        + Fore.GREEN
                        + used_mod
                        + Fore.RESET
                        + " not defined in any files. Creating empty ",
                        file=sys.stderr,
                    )

            depends[module] = sorted(graph, key=lambda f: f.source_file.filename)

        if verbose:
            for module_ in sorted(depends.keys(), key=lambda f: f.source_file.filename):
                print(
                    Fore.GREEN + module_.name + Fore.RESET + " depends on :" + Fore.BLUE
                )
                for dep in depends[module_]:
                    print("\t" + dep.name)
                print(Fore.RESET)

        return depends

    def get_depends_by_file(self, verbose=False):
        """Return the set of which files each file directly depends on

        Args:
            verbose: Print progress messages

        Returns:
            dict of `FortranFile` and a list of `FortranFile`

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
                    print(
                        Fore.RED
                        + "Error"
                        + Fore.RESET
                        + " module "
                        + Fore.GREEN
                        + mod
                        + Fore.RESET
                        + " not defined in any files. Skipping...",
                        file=sys.stderr,
                    )
            depends[source_file] = sorted(graph, key=lambda f: f.filename)

        if verbose:
            for file_ in sorted(depends.keys(), key=lambda f: f.filename):
                print(
                    Fore.GREEN
                    + file_.filename
                    + Fore.RESET
                    + " depends on :"
                    + Fore.BLUE
                )
                for dep in depends[file_]:
                    print("\t" + dep.filename)
                print(Fore.RESET)

        return depends

    def get_all_used_files(self, module_name):
        """Get the complete set of files that a module requires, either
        directly or indirectly

        Args:
            module_name (str): A module name

        Returns:
            list of filenames (str)

        """
        used_modules = self._get_all_used_modules(module_name, state=[])
        used_files = [
            self.modules[module].source_file.filename for module in used_modules
        ]

        module_filename = self.modules[module_name].source_file.filename

        return sorted(set(used_files + [module_filename]))

    def get_all_used_modules(self, module_name):
        """Get the complete set of modules that module_name requires, either
        directly or indirectly

        Args:
            module_name (str): A module name

        Returns:
            list of module names (str)

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
                print(
                    Fore.RED
                    + "Error"
                    + Fore.RESET
                    + " module "
                    + Fore.GREEN
                    + module
                    + Fore.RESET
                    + " not defined in any files. Skipping...",
                    file=sys.stderr,
                )

        return sorted(set(state))

    def write_depends(
        self, filename="makefile.dep", overwrite=False, build="", skip_programs=False
    ):
        """Write the dependencies to file

        Args:
            filename (str): Name of the output file
            overwrite (bool): Overwrite existing dependency file [False]
            build (str): Directory to prepend to filenames
            skip_programs (bool): Don't write dependencies for programs
        """

        def _format_dependencies(target, target_extension, dep_list):
            _, filename = os.path.split(target)
            target_name = os.path.splitext(filename)[0] + target_extension
            listing = "\n{} : ".format(os.path.join(build, target_name))
            for dep in dep_list:
                _, depfilename = os.path.split(dep)
                depobjectname = os.path.splitext(depfilename)[0] + ".o"
                listing += " \\\n\t{}".format(os.path.join(build, depobjectname))
            listing += "\n"
            return listing

        # Test file doesn't exist
        if os.path.exists(filename):
            if not (overwrite):
                print(
                    Fore.RED
                    + "Warning: file '{}' exists.".format(filename)
                    + Fore.RESET
                )
                opt = input("Overwrite? Y... for yes.")
                if opt.lower().startswith("y"):
                    pass
                else:
                    return

        with smart_open(filename, "w") as f:
            f.write(DEPFILE_HEADER + "\n")

            if not skip_programs:
                for program in self.programs.keys():
                    program_deps = self.get_all_used_files(program)
                    listing = _format_dependencies(program, "", program_deps)
                    f.write(listing)

            for file_ in sorted(self.depends_by_file.keys(), key=lambda f: f.filename):
                dep_list = [dep.filename for dep in self.depends_by_file[file_]]
                listing = _format_dependencies(file_.filename, ".o", dep_list)
                f.write(listing)

    def make_graph(self, filename=None, format="svg", view=True):
        """Draw a graph of the project using graphviz

        Args:
            filename (str): Name of the output file
            format (str): Image format (default: 'svg')
            view (bool): Immediately display the graph [True]

        """

        if filename is None:
            filename = self.name + ".dot"

        graph = Graph(
            self.depends_by_module, filename=filename, format=format, view=view
        )
        graph.draw()

    def remove_ignored_modules(self, ignore_modules=None):
        """Remove the modules in iterable ignore_modules from all dependencies

        Args:
            ignore_modules (iterable of str): module names to ignore
                                              (default: iso_c_binding and iso_fortran_env)

        """
        if ignore_modules is None:
            ignore_modules = []
        elif not isinstance(ignore_modules, list):
            ignore_modules = [ignore_modules]

        ignored_modules = ignore_modules + DEFAULT_IGNORED_MODULES

        # Remove from module dict
        for ignore_mod in ignored_modules:
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
