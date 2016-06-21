import os
import sys
import warnings

# Terminal colours
from colorama import Fore

from .smartopen import smart_open
from .units import FortranFile, FortranModule

# If graphviz is not installed, graphs can't be produced
try:
    import graphviz as gv
    has_graphviz = True
except ImportError:
    has_graphviz = False

# Python 2/3 compatibility
try:
    input = raw_input
except NameError:
    pass


class FortranProject(object):
    def __init__(self, name=None, excludes=None, files=None, macros={}, verbose=False):
        """Create a list of FortranFile objects
        """

        if name is None:
            self.name = os.path.basename(os.getcwd())
        else:
            self.name = name

        if files is None:
            files = self.get_source()

        if excludes is not None:
            files = set(files) - set(excludes)

        self.files = {filename: FortranFile(filename, macros)
                      for filename in files}
        self.modules = self.get_modules()

        self.remove_excluded_modules(excludes)

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
            f.write('# This file is generated automatically. DO NOT EDIT!\n')
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
        if not has_graphviz:
            warnings.warn("graphviz not installed: can't make graph",
                          RuntimeWarning)
            return

        if filename is None:
            filename = self.name + ".dot"

        # Start the graph
        graph = gv.Digraph(name=filename, format=format)

        for source_file in self.depends_by_file:
            graph.node(source_file.filename)
            for module in self.depends_by_file[source_file]:
                # Add the edges to the graph
                graph.edge(source_file.filename, module.filename)

        graph.render(filename, view=view, cleanup=False)

    def remove_excluded_modules(self, excludes=None):
        """Remove the modules in iterable excludes from
        all dependencies
        """
        if excludes is None:
            return
        # Remove from module dict
        for exclude in excludes:
            self.modules.pop(exclude, None)
        # Remove from 'used' modules
        for module in self.modules.values():
            for exclude in excludes:
                try:
                    module.uses.remove(exclude)
                except ValueError:
                    pass
        # Remove from 'used' files
        for source_file in self.files.values():
            for exclude in excludes:
                try:
                    source_file.uses.remove(exclude)
                except ValueError:
                    pass
