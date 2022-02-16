import warnings

# If graphviz is not installed, graphs can't be produced
try:
    import graphviz as gv

    has_graphviz = True
except ImportError:
    has_graphviz = False


class Graph:
    """Draw a graph of the project using graphviz

    Args:
        filename (str): Name of the output file
        format (str): Image format
        view (bool): Immediately display the graph [True]

    """

    def __init__(self, tree, filename=None, format="svg", view=True):

        if not has_graphviz:
            warnings.warn("graphviz not installed: can't make graph", RuntimeWarning)
            return

        if filename is None:
            filename = "graph.dot"

        self.filename = filename
        self.view = view

        # Start the graph
        self.graph = gv.Digraph(name=self.filename, format=format)
        self.tree = tree

    def draw(self):
        """Render the graph to an image"""
        if not has_graphviz:
            warnings.warn("graphviz not installed: can't make graph", RuntimeWarning)
            return

        for parent in self.tree:
            self.graph.node(str(parent))
            for child in self.tree[parent]:
                # Add the edges to the graph
                self.graph.edge(str(parent), str(child))

        self.graph.render(self.filename, view=self.view, cleanup=False)
