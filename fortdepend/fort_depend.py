import os
import warnings

# Terminal colours
from colorama import Fore

from .units import FortranFile

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
    def __init__(self, name=None, files=None, macros={}, verbose=False):
        """Create a list of FortranFile objects
        """

        if name is None:
            self.name = os.path.basename(os.getcwd())
        else:
            self.name = name

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
                    print(Fore.RED + "Error" + Fore.RESET + " module " +
                          Fore.GREEN + used_mod + Fore.RESET +
                          " not defined in any files. Skipping...")

            depends[module] = sorted(graph,
                                     key=lambda f: f.source_file.filename)
        if verbose:
            for file_ in depends.keys():
                print(Fore.GREEN + file_.source_file.filename + Fore.RESET +
                      " depends on :" + Fore.BLUE)
                for dep in depends[file_]:
                    print("\t" + dep.source_file.filename)
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
                          mod + Fore.RESET + " not defined in any files. Skipping...")
            depends[source_file] = sorted(graph,
                                          key=lambda f: f.filename)

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

        with open(filename, 'w') as f:
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
