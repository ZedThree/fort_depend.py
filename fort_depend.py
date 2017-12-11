#!/usr/bin/python
#The MIT License (MIT)

#Copyright (c) 2014 David Dickinson, Peter Hill

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.
# 
#
#  This is a modification of the original script of D Dickinson @https://github.com/ZedThree/fort_depend.py
#  done by Adam Jirasek
#  The modified version can be found @https://github.com/libm3l/fort_depend.py
#
#  This is a script which makes fortran project dependecies
#  Fortran project can have source files located in diffrent subdirectories
#  where the common directory (project root directory)is specified as an input parameter with an option -r
#  The script is executed in each directory separately and creates a project.dep file with fortran dependencies
#  however, while searching modules it will loop over all subdirectories in project root directory
#
#  If user wants to specify the search for modules to selected set of subdirectories, 
#  he/she can use --dep_dir followed by list of selected directories
#
#  If -r not specified, the script will use the current working directory and it will search
#  for modules in this directory only.
#
#  List of all options is:
#   -f --files Files to process
#   -o --output Output file
#   -v -vv -vvv Different level of verbosity contained in standard output
#   -w --overwrite Overwrite output file without warning
#   -r --root_dir Project root directory
#   -d --dep_dir List of selected dependecy directories
#
#  For example of how the fortran dependency scritp can be used for larger project see
#  FLL linked list utility at https://github.com/libm3l/fll
#
# History:
# Version   Date       Author        Patch number  CLA  Comment
# -------   --------   --------      ------------  ---  -------
# 1.1       01/09/17   Adam Jirasek                     Rewrite original version
#
#
#
#
import os
import re
import glob
import fnmatch
import os
import sys
from time import gmtime, strftime
import getpass

#Definitions

def run(path,dep=None,ignore=None,files=None,verbose=None,overwrite=None,output=None,macros={},build=''):
   
    cwd = os.getcwd()
#
#  if project root path not specified, used current directory
#
    if(path == None):
       path = os.getcwd()
       
    path = check_path(path=path)
    cwd = check_path(path=cwd)

    if int(verbose) > 0:
      print("  ")
      print("\033[031m Making dependencies in \033[032m"+cwd+"\033[039m directory")
      print("  ")
#
#  get files where to look for modules
#  if list of preferred directories is specified in dep
#  list only these files, otherwise
#  list all file in path dir
#
#  files paths is relative to projet root directory path so that if the compillation is done in different directory then
#  where the source files are located, there are no any prolems with it
#
    ff=get_all_files(path=path, dep=dep) 
    
    if int(verbose) > 2:
      print(" ")
      print("\033[031m Searching for modules in files:\033[039m")
      print(ff)
    
    l=create_file_objs( (verbose) ,files,macros)
    mod2fil=file_objs_to_mod_dict(file_objs=l)
#
#  make dependencies
#
    depends=get_depends(ignore=ignore,verbose=verbose,cwd=cwd,fob=l,m2f=mod2fil,ffiles=ff)

    if int(verbose) ==  3 :
       for i in depends.keys():
           print ("\033[032m "+i+"\033[039m depends on:\033[034m")
           for j in depends[i]: print( "\t"+j)
           print ("\033[039m")

    if output is None:
        output = "makefile.dep"

    tmp=write_depend(verbose=verbose,path=path,cwd=cwd,outfile=output,dep=depends,overwrite=overwrite,build=build)

    return depends

def write_depend(verbose,path,cwd,outfile="makefile.dep",dep=[],overwrite=False,build=''):
    "Write the dependencies to outfile"
#
#Test file doesn't exist
#
    if os.path.exists(outfile):
        if not(overwrite):
            print ("\033[031mWarning file exists.\033[039m")
            opt=raw_input("Overwrite? Y... for yes.")
        else:
            opt="y"
        if opt.lower().startswith("y"):
            pass
        else:
            return
#
#Open file
#
    if int(verbose) > 1:
      print("  ")
      print("\033[031m Opening dependency file \033[032m"+outfile+"\033[039m ...")
      print("  ")
    f=open(outfile,'w')
    f.write('# This file is generated automatically by fort_depend.py. DO NOT EDIT!\n')
#
#  header 
#
    username = getpass.getuser()
    f.write("#\n")
    f.write("#  Created by: "+username+"\n")
    f.write("#  Date: "+strftime("%Y-%m-%d %H:%M:%S", gmtime()) + "\n")
    f.write("#\n")

    for i in dep.keys():
        tmp,fil=os.path.split(i)
#
#  get name of file for which you write dependencies with .o 
#
        stri="\n"+os.path.join(build, fil.split(".")[0]+".o"+" : ")
        if int(verbose) > 1:
          print("\033[031m Writing dependency info for \033[032m"+i+"\033[039m module")
#
#  now write down all files containing modules 
#  the list contains file names with .f or f90 suffix, replace by .o suffix
        if not(dep[i] == ""):
#
#  add module name to stri and separate by new line and tab
#
          for j in dep[i]:

            npathseg = j.count('/')
            if npathseg == 0:
#
#  module is in the file located in the same directory as the file for which the dependency is written
#
                tmp,fil=os.path.split(j)
#
#  replace suffix with .o
#
                fil= os.path.splitext(j)[0]+'.o'
            else:
#
#  module is in file located in different directory
#
                 fil = j

            if "../" in fil:
                stri = stri + " \\\n\t" + fil
            else:
                stri=stri+" \\\n\t"+os.path.join(build, fil.split(".")[0]+".o")
#
#  add the last new line and write to a file
#           
        stri=stri+"\n"
        f.write(stri)

    f.close()

    if int(verbose) > 1:
      print("\033[031m Finished ... \033[039m")
      print("  ")

    return

def get_source(ext=[".f90",".F90",".f",".F"]):
#
#      "Return all files ending with any of ext"
#
    tmp=os.listdir(".")
    
    fil=[]
    for i in ext:
        fil.extend(filter(lambda x: x.endswith(i),tmp))

    return fil

def get_all_files(path,dep):
#
#  list all fortran files where to look for possible module
#  once found add their relative path from this directory to project root directory
#  it is important to do so when the project is compiled in a different directory
#  the path of all modules should be relatie
#
    matches = []
#
#  get relative path of current directory
#
    currdirr = os.getcwd()
    currdirr = check_path(path=currdirr)
    relapth = currdirr
    relapth=relapth.replace(path,'')
    slsh_count  = relapth.count('/')
    relapth = ''
    for isl in range(0, slsh_count):
            relapth += "../"
#
#  list only files located in those
#
    if not(dep == None):
       dep.append(currdirr)
       for i in dep:
#
#   use basolute path to preferred directories ie.: os.path.abspath(i) 
#
          for root, dirnames, filenames in os.walk(os.path.abspath(i)):   
#
#  list all files and check if they end up with given suffix, if yes, add to the list
#
                for filename in filenames:
                    if filename.endswith(('.f', '.f90', '.F', '.F90')):
##                   matches.append(os.path.join(root, filename))
##
##    add specified dependency directory location (i) rather then aboslute path
##                    
                       if(root == currdirr):
#
#   file is in this directory add juts its name
#
                            matches.append(filename)
                       else:
#
#   file is different directory, 
#   substract project root parth from the file path
#   add trailing /   and then add ../ to get to project root path, ie..file will have relative path
#
                          cwurrdirr = root
                          cwurrdirr = cwurrdirr.replace(path,'')
#
#   if specified directory is a subdirectory in projet root path then add files
#   otherwise not (it is possible then some external library is specified)
                          if root != cwurrdirr:
                            cwurrdirr = relapth + cwurrdirr + "/"
                            matches.append(os.path.join(cwurrdirr, filename))
#
#  otherwise include all files from path dir
#                         
    else: 
#
#  loop over all file in project root path directory 
#
       for root, dirnames, filenames in os.walk(path):
       
         for filename in filenames:
             if filename.endswith(('.f', '.f90', '.F', '.F90')):
 #                matches.append(os.path.join(root, filename))
 
                 if(root == currdirr):
#
#   file is in this directory add juts its name
#
                      matches.append(filename)
                 else:
#
#   file is different directory, 
#   sybstract project root parth from the file path
#   add trailing /   and then add ../ to get to project root path, ie..file will have relative path
#
                   cwurrdirr = root
                   cwurrdirr=cwurrdirr.replace(path,'')
                   cwurrdirr = relapth + cwurrdirr + "/"
                   matches.append(os.path.join(cwurrdirr, filename))

    return matches

def check_if_there(use,file):
#
#   "return if you see module name"
#    make routine to consider version of python installation
#
    if sys.version_info < (3,0):
      with open(file) as f:
        for line in f:
            if "module" in line.lower():
                extrline = line.lower()
                extrline = extrline.replace("module", "",1)
                if use.lower().strip() == extrline.strip():
                    f.close()
                    return 1
    else:
       with open(file) as f:
         with open(file, errors='ignore') as f:
            for line in f:
              if "module" in line.lower():
                extrline = line.lower()
                extrline = extrline.replace("module", "",1)
                if use.lower().strip() == extrline.strip():
                    f.close()
                    return 1
                
    f.close()
    return 0


def create_file_objs(verbose, files=None,  macros={}):
    l=[]

#    if files is None:
#        files = get_source()

    files = get_source()

    if int(verbose) > 1 : 
      print(" ")
      print("\033[031m Searching modules for files:\033[039m")
      print(" ")


    for i in files:
        source_file = file_obj()

        if int(verbose) > 1 :
          print("\033[031m -- \033[039m" + i)
        source_file.file_name = i
        source_file.uses = get_uses(i,macros)
        source_file.includes = get_includes(i,macros)
        source_file.contains = get_contains(i)

        l.append(source_file)

    if int(verbose) > 1 :
      print(" ")

    return l

def get_uses(infile=None, macros={}):
    "Return which modules are used in infile after expanding macros"
    p=re.compile("^\s*use\s+(?P<moduse>\w*)\s*(,)?\s*(only)?\s*(:)?.*?$",re.IGNORECASE).match

    uses=[]

    if sys.version_info < (3,0):
      with open(infile,'r') as f:
        t=f.readlines()
    else:
      with open(infile,'r',errors='ignore') as f:
        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            uses.append(tmp.group('moduse').strip())

    # Remove duplicates
    uniq_mods = list(set(uses))

    for i, mod in enumerate(uniq_mods):
        for k, v in macros.items():
            if re.match(k, mod, re.IGNORECASE):
                uniq_mods[i] = mod.replace(k,v)

    return uniq_mods

def get_includes(infile=None, macros={}):
    "Return which modules are included in infile after expanding macros"
    p=re.compile("\#include\s*\"(?P<incfile>\w*).inc\"$",re.IGNORECASE).match

    includes=[]

    if sys.version_info < (3,0):
      with open(infile,'r') as f:
        t=f.readlines()
    else:
      with open(infile,'r',errors='ignore') as f:
        t=f.readlines()
#    with open(infile,'r',encoding = "ISO-8859-1") as f:
#        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            includes.append(tmp.group('incfile').strip()+".inc")

    # Remove duplicates
    uniq_includes = list(set(includes))

    for i, mod in enumerate(uniq_includes):
        for k, v in macros.items():
            if re.match(k, mod, re.IGNORECASE):
                uniq_includes[i] = mod.replace(k,v)

    return uniq_includes

def get_contains(infile=None):
    "Return all the modules that are in infile"
    p=re.compile("^\s*module\s*(?P<modname>\w*)",re.IGNORECASE).match

    contains=[]

    if sys.version_info < (3,0):
      with open(infile,'r') as f:
        t=f.readlines()
    else:
      with open(infile,'r', errors='ignore') as f:
        t=f.readlines()

    for i in t:
        tmp=p(i)
        if tmp:
            contains.append(tmp.group('modname').strip())

# Remove duplicates before returning
    return list(set(contains))

def file_objs_to_mod_dict(file_objs=[]):
    "Turn a list of file_objs in a dictionary, containing which modules depend on which files"
    dic={}
    for i in file_objs:
        for j in i.contains:
            dic[j.lower()]=i.file_name
    return dic

def get_depends(ignore,verbose,cwd,fob=[],m2f=[], ffiles=[]):
    deps={}
    istat = 0

    for i in fob:
        if int(verbose) > 1 :
          print("\033[031m Checking dependency for file: \033[032m"+i.file_name+"\033[039m")
          
        tmp=[]
        for j in i.uses:
            if ignore and (j in ignore): continue
            try:
#
#  module is in the same directory, include it
#
                tmp.append(m2f[j.lower()])
                istat = 1
            except KeyError:
#
#  module is not, loop through all other files specified in ffiles
#  these are files found in function get_all_files
#
                for k in ffiles:

                    dir,fil=os.path.split(k)
                    dir = dir+ "/"
                    retval = 0
                    
                    if not(cwd.strip() == dir):
                        
                        retval=check_if_there(use=j,file=k)
                       
                        if retval > 0:
                          istat = 1
                          name=os.path.splitext(k)[0]+'.o'
                          tmp.append(name.lower())

                          if int(verbose) > 2 :
                            print ("\033[031m   Note: \033[039m module \033[032m"+j+"\033[039m not defined in any file in this directory")
                            print ("\033[031m         \033[039m module found in \033[032m"+name+"\033[039m file")
                            print ("\033[031m         \033[039m adding the module to dependency file, not checking its dependency further \033[032m\033[039m")
                          break    #break loop, dependency declared
                
                if istat== 0 and (j != ""):
                         if int(verbose) > 2 :
                           print("")
                           print ("\033[031m   Note!!!!: \033[039m module \033[032m"+j+"\033[039m not defined in any file")
                           print ("\033[031m             \033[039m assuming intrinsic module, not adding to dependency tree ... \033[032m\033[039m")
#
#   once module found, break the loop
#
#                           break    #break loop, dependency declared 
        if (istat != 0):
#
#   if file containign module, add to the list 
#
             deps[i.file_name]=tmp
        else:
             deps[i.file_name]="" 

        if int(verbose) > 1 :
          print("\033[031m   Done ... \033[032m")

    return deps

def check_path(path):
    if path.endswith("/"):
        return path
    else:
        path=path + "/"

    return path

def get_relative_path_name(file,path,cwd):
    length = len(path)
    filetmp = file
    fil = filetmp.replace(filetmp[:length], '')
    
    loccwd = cwd
    loccwd = loccwd.replace(loccwd[:length], '')

    npathseg = loccwd.count('/')
    for x in range(0, npathseg):
        fil = "../"+fil

    return fil


class file_obj:
    def __init__(self):
        self.file_name=None
        self.uses=None
        self.includes=None
        self.contains=None
        self.depends_on=None


#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='Generate Fortran dependencies')
    parser.add_argument('-f','--files',nargs='+',help='Files to process')
    parser.add_argument('-i','--ignore',nargs='+',help='Modules to ignore')
    parser.add_argument('-D',nargs='+',action='append',metavar='NAME=DESCRIPTION',
                        help="""The macro NAME is replaced by DEFINITION in 'use' statements""")
    parser.add_argument('-b','--build',nargs=1,help='Build Directory (prepended to all files in output',
                        default='')
    parser.add_argument('-o','--output',nargs=1,help='Output file')
    parser.add_argument('-v','--verbose',action='store_true',help='explain what is done')
    parser.add_argument('-vv','--vverbose',action='store_true',help='explain what is done')
    parser.add_argument('-vvv','--vvverbose',action='store_true',help='explain what is done')
    parser.add_argument('-w','--overwrite',action='store_true',help='Overwrite output file without warning')
    parser.add_argument('-r','--root_dir',nargs=1,help='Project root directory')
    parser.add_argument('-d','--dep_dir',nargs='+',action='append',help='Preferred dependecy directory')

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
    root_dir = args.root_dir[0] if args.root_dir else None
    dep_dir = args.dep_dir[0] if args.dep_dir else None
    
    if not root_dir:
        print ("\033[031mError: \033[039m missing path to project root directory \033[032m")
        sys.exit()

    verbose = 0
    if(args.verbose):
        verbose = 1
    if(args.vverbose):
        verbose = 2
    if(args.vvverbose):
        verbose = 3
    

    run(path=root_dir, dep=dep_dir, ignore=args.ignore, files=args.files, verbose=verbose, overwrite=args.overwrite, macros=macros, output=output, build=build)
