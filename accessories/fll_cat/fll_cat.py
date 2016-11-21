#!/usr/bin/python
# 
#
#  this is a python script wj
#
import os
import sys
from subprocess import Popen, PIPE
import unicodedata
import ast

#Definitions

def run(file,fmt,efmt,scan):
#
#  execute 
#
    print_header()
#
    path = os.path.dirname(os.path.abspath(__file__))
    cwd = os.getcwd()

    path = check_path(path=path)
    cwd = check_path(path=cwd)

    executable = path+"fll_cat.x"

    if not os.path.isfile(file):
      print("  ")
      print("\033[031mERROR:\033[039m specified file \033[032m"+file+"\033[039m does not exist, terminating .... ") 
      sys.exit()

    print(" ")  
    print("\033[039m Specified file  is:       \033[032m"+file+"\033[039m")
    if fmt == 'b'  or fmt == 'B':
      print("\033[039m Specified file format is: \033[032mbinary\033[039m") 
    else:
      print("\033[039m Specified file format is: \033[032mASCII \033[039m")  

    if scan == 'Y':  
        print(" ")
        print("\033[035m ... running in scan only mode ... \033[039m")  
    print(" ")  
    
    if sys.version_info < (3,0):
      p = Popen([executable], stdin=PIPE) #NOTE: no shell=True here
      p.communicate(os.linesep.join([file, fmt, efmt, scan]))
    else:
      p = Popen([executable], stdin=PIPE,universal_newlines=True) #NOTE: no shell=True here
      p.communicate(os.linesep.join( [file, fmt, efmt, scan]))

def print_header():
     print("  ")
     print ("\033[031m************************************************************************************ \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m               \033[039m              fll_cat   - v1.1       \033[031m                          \033[039m")
     print ("\033[031m                                                                                   \033[039m")
     print ("\033[031m             \033[039m          prints content of file on screen  \033[031m                    \033[039m")
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
    parser.add_argument('-i','--file',nargs=1,help='Files to process')
    parser.add_argument('-f','--format',nargs=1,help='Format of the file')
    parser.add_argument('-s','--scan',action='store_true',help='Scan file only',required=False)
    parser.add_argument('-e','--external_format',nargs=1,help='External format of the file')

    # Parse the command line arguments
    args = parser.parse_args()

    file = args.file[0]   if args.file else None
    format = args.format[0] if args.format else None
    eformat = args.external_format[0] if args.external_format else None
    scan = args.scan
 
    if not scan:
       scan = 'n'
    else:
       scan = 'Y'

    if not file:
        print ("\033[031mError: \033[039m missing name of file\033[031m-c \033[032m")
        sys.exit()

    if not format:
        print ("\033[031mError: \033[039m missing file format\033[031m-c \033[032m")
        print ("\033[031m       \033[039m available options are: \033[032m a - ASCII\033[039m")
        print ("\033[031m       \033[039m                        \033[032m b - binary format\033[039m")
        sys.exit()

    if not eformat:
        eformat = 'fll'
    else:
     if not('fll') or not('ffa'):
        print ("\033[031mError: \033[039m wrong file format\033[031m-e \033[032m")
        print ("\033[031m       \033[039m available options are: \033[032m fll - fll native format\033[039m")
        print ("\033[031m       \033[039m                        \033[032m ffa - ffa format\033[039m")
        sys.exit()

    run(file=file,fmt=format, efmt = eformat, scan=scan)
