#!/usr/bin/python
# 
#
#  this is a python script
#
import os
import sys
from subprocess import Popen, PIPE
import unicodedata
import ast

#Definitions

def run(boutfile,bedgefile, outfile):
#
#  execute 
#
    print_header()
#
    path = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()

    path = check_path(path=path)
    cwd = check_path(path=cwd)

    executable = path+"fll_forces.x"

    if not os.path.isfile(boutfile):
      print("  ")
      print("\033[031mERROR:\033[039m bout file \033[032m"+file+"\033[039m does not exist, terminating .... ") 
      sys.exit()
      
    if not os.path.isfile(bedgefile):
      print("  ")
      print("\033[031mERROR:\033[039m bedge file \033[032m"+file+"\033[039m does not exist, terminating .... ") 
      sys.exit()
      
    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([bedgefile, boutfile, outfile]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [bedgefile, boutfile, outfile]))

def print_header():
     print("  ")
     print ("\033[031m************************************************************************************ \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m               \033[039m              fll_convert   - v1.1       \033[031m                          \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m             \033[039m                 conversion utility  \033[031m                    \033[039m")
     print ("\033[031m                                                                                  \033[039m")
     print ("\033[031m************************************************************************************ \033[039m")


def check_path(path):
    if not(path.endswith("/")):
        path=path + "/"

    return path



#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='FLL configure script')
    parser.add_argument('-s','--bout_file',nargs=1,help='Solution file')
    parser.add_argument('-b','--bedge_file',nargs=1,help='bedge file')
    parser.add_argument('-o','--output_file',nargs=1,help='Output file')

    # Parse the command line arguments
    args = parser.parse_args()

    bout_file = args.bout_file[0]   if args.bout_file else None
    bedge_file = args.bedge_file[0]   if args.bedge_file else None
    output_file = args.output_file[0] if args.output_file else None
    
    if not len(sys.argv) > 1:
        print("\nfll_force - calculates forces from bedg and bout files\n")
        print("usage: fll_forces.py [-h] [-s BOUT_FILE] [-b BEDGE_FILE] [-o OUTPUT_FILE]\n")
        sys.exit()

    if not bout_file:
        print ("\033[031mError: \033[039m missing name of file, option \033[031m -s \033[039m")
        sys.exit()
        
    if not bedge_file:
        print ("\033[031mError: \033[039m missing name of file, option \033[031m -b \033[039m")
        sys.exit()
    
    if not output_file:
        print ("\033[031mError: \033[039m missing output file, option \033[031m -o \033[039m")
        sys.exit()


    run(boutfile=bout_file,bedgefile=bedge_file, outfile=output_file)
