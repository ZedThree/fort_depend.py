#!/usr/bin/env python3
import argparse
import colorama
from fortdepend import FortranProject


def main(args=None):
    """Run the module as a script

    """
    # Add command line arguments
    parser = argparse.ArgumentParser(description='Generate Fortran dependencies')
    parser.add_argument('-f', '--files', nargs='+', help='Files to process')
    parser.add_argument('-D', nargs='+', action='append', metavar='NAME=DESCRIPTION',
                        help="The macro NAME is replaced by DEFINITION in 'use' statements")
    parser.add_argument('-b', '--build', nargs=1, default='',
                        help='Build Directory (prepended to all files in output)')
    parser.add_argument('-o', '--output', nargs=1, help='Output file')
    parser.add_argument('-g', '--graph', action='store_true',
                        help='Make a graph of the project')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='explain what is done')
    parser.add_argument('-w', '--overwrite', action='store_true',
                        help='Overwrite output file without warning')
    parser.add_argument('-c', '--colour', action='store_true',
                        help='Print in colour')

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

    # Sorts out the terminal colours on Windows
    strip_colours = not args.colour
    colorama.init(strip=strip_colours)

    project = FortranProject(files=args.files, macros=macros, verbose=args.verbose)

    if output is not None:
        project.write_depends(filename=output, overwrite=args.overwrite, build=build)

    if args.graph:
        project.make_graph()

# Script
if __name__ == "__main__":
    main()
