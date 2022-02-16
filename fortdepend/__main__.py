#!/usr/bin/env python3
import argparse
import colorama
from fortdepend import FortranProject
from fortdepend import __version__


def create_argument_parser():
    """Create the parser for the command line arguments"""
    # Add command line arguments
    parser = argparse.ArgumentParser(description="Generate Fortran dependencies")
    parser.add_argument("-f", "--files", nargs="+", help="Files to process")
    parser.add_argument(
        "-D",
        nargs="+",
        action="append",
        metavar="NAME[=DESCRIPTION]",
        help="Preprocessor define statements",
    )
    parser.add_argument(
        "-I",
        action="append",
        metavar="dir",
        help="Add dir to the preprocessor search path",
    )
    parser.add_argument(
        "-b",
        "--build",
        nargs=1,
        default="",
        help="Build Directory (prepended to all files in output)",
    )
    parser.add_argument("-o", "--output", nargs=1, help="Output file")
    parser.add_argument(
        "-g", "--graph", action="store_true", help="Make a graph of the project"
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="explain what is done"
    )
    parser.add_argument(
        "-w",
        "--overwrite",
        action="store_true",
        help="Overwrite output file without warning",
    )
    parser.add_argument("-c", "--colour", action="store_true", help="Print in colour")
    parser.add_argument(
        "-e", "--exclude-files", nargs="+", default=None, help="Files to exclude"
    )
    parser.add_argument(
        "-i", "--ignore-modules", nargs="+", default=None, help="Modules to ignore"
    )
    parser.add_argument(
        "-s",
        "--skip-programs",
        action="store_true",
        help="Don't include programs in the output file",
    )
    parser.add_argument(
        "-n",
        "--no-preprocessor",
        action="store_true",
        help="Don't use the preprocessor",
    )
    parser.add_argument(
        "--version",
        action="version",
        version="%(prog)s {version}".format(version=__version__),
    )

    return parser


def main(args=None):
    """Run the module as a script"""

    parser = create_argument_parser()
    # Parse the command line arguments
    args = parser.parse_args()

    # Assemble a dictionary out of the macro definitions
    macros = {}
    if args.D:
        for arg in args.D:
            for var in arg:
                if "=" not in var:
                    macros[var] = ""
                else:
                    temp = var.split("=")
                    macros[temp[0]] = temp[1]

    output = args.output[0] if args.output else None
    build = args.build[0] if args.build else ""

    # Sorts out the terminal colours on Windows
    strip_colours = not args.colour
    colorama.init(strip=strip_colours)

    project = FortranProject(
        files=args.files,
        exclude_files=args.exclude_files,
        ignore_modules=args.ignore_modules,
        macros=macros,
        cpp_includes=args.I,
        verbose=args.verbose,
        use_preprocessor=not args.no_preprocessor,
    )

    if output is not None:
        project.write_depends(
            filename=output,
            overwrite=args.overwrite,
            build=build,
            skip_programs=args.skip_programs,
        )

    if args.graph:
        project.make_graph()


# Script
if __name__ == "__main__":
    main()
