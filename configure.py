#!/usr/bin/python
# 
#
#  this is a python script wj
#
import os
import re
import glob
import fnmatch
import os
import sys
import errno
import copy
import platform

#Definitions

def run(comp,files=None,verbose=True,overwrite=None,output=None,macros={},build=''):
#
#  definition of parameters
#
    print_header()
    linkfiles =(['src_dir_path.mk', 'Makefile', 'project.dep'])
    exclude =(['python_dep', 'config', '.git'])     
#
#  
    path = os.path.dirname(os.path.abspath(__file__))
#
    cwd = os.getcwd()

    path = check_path(path=path)
    if not os.path.isdir(path):
      print("  ")
      print("\033[031mERROR:\033[039m specified project source location \033[032m"+path+"\033[039m does not exist, terminating .... ") 
      sys.exit()

    cwd = check_path(path=cwd)

    print("  ")
    print("\033[031mDIAG:\033[039m project location is                  \033[032m"+path+"\033[039m")  	
    print("\033[031mDIAG:\033[039m Intended location of compillation is \033[032m"+cwd+"\033[039m")    

    if cwd == path:
        print(".....")
        print ("\033[031mError:\033[039m project location is the same as inteded location of compillation \033[032m"+path+"\033[039m")  	
        print ("\033[031m      \033[039m choose different location \033[032m"  "\033[039m")
        print("       terminating .... ") 	
        sys.exit()
#
#  creating config file
#
    print("  ")
    print("\033[031mDIAG:\033[039m creating configure file \033[032m \033[039m")  	
    print("  ")
    ok = mkconfigfile(path=path, cwd=cwd,version=comp, bin_dir=cwd)
#
#   create structure and link necessary files
#
    print("  ")
    print("\033[031mDIAG:\033[039m Recreating project tree structure and linking files \033[032m \033[039m .....")  	
    print("\033[031mDIAG:\033[039m Project root directory .... \033[032m \033[039m .....")  	
    ok=prepare_compiler(root_path=path, cwd=cwd, linkfiles=linkfiles)
    print("  ")
    print("\033[031mDIAG:\033[039m Subdirectories .... \033[032m \033[039m")  	
    ok=mkdir_structure(root_path=path, cwd=cwd, exclude=exclude, linkfiles=linkfiles)

    print("  ")
    print("\033[031mSUCCESS:\033[039m Setup was succesful \033[032m \033[039m")  	
    print("  ")

#   list directories and subdirectories

def get_all_dirs(path,exclude):

    matches = []
    matchesf = []

    for root, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(dirnames, '*'):
            matches.append(os.path.join(root, filename))

    length = len(path)
    for name in matches:
        dirtmp = name.replace(name[:length], '')
        matchesf.append(dirtmp)

    for d in list(matchesf):
        for word in exclude:
           if word in d:
              matchesf.remove(d) 
 
    print ("\033[031mDIAG:\033[039m Following list of subdirectories will be processed ... \033[032m \033[039m")      
    for name in matchesf:
        print ("\033[032m      "+name+"\033[039m")  

    return matchesf

def prepare_compiler(root_path,cwd, linkfiles):
#
#  list all dirs in project except directories specified in exclude
#
   linknew = copy.copy(linkfiles)
   linknew.append('rules.mk')
   linknew.remove('project.dep')

   for word in linknew:
      try:
        os.remove(word)
        print ("\033[031mDIAG: \033[039m file \033[032m"+word+"\033[039m already exists, removing ....")
      except OSError:
        pass

      source = root_path+'/'+word
      dest   = cwd+'/'+word
      print ("\033[031mDIAG: \033[039m linking file \033[032m"+source+"\033[039m ....")
      linkfile = os.symlink( source, dest)

   return 

def mkdir_structure(root_path,cwd, exclude, linkfiles):
#
#  list all dirs in project except directories specified in exclude
#
    dirs=get_all_dirs(path=root_path,exclude=exclude) 

    print(" ")
#
#  loop over diorectories and make them 
#
    for dir in dirs:
       try:
          os.makedirs(dir)
       except OSError as e:
         if e.errno != errno.EEXIST:
            raise  # raises the error again
         else:
          print ("\033[031mDIAG: \033[039m directory \033[032m"+dir+"\033[039m already exists, keeping it ....")
#
#  loop over new directories and link the linkfiles
#  from project directories
#
    print("  ")
    length = len(cwd)

    for subdir, dirs, files in os.walk(cwd):
       for dir in dirs:
          subdir = check_path(path=subdir)
          newdir = subdir+dir
          print ("\033[031mDIAG: \033[039m processing directory \033[032m"+newdir+"\033[039m ....")
          dirtmp = subdir.replace(subdir[:length], '')
          dirtmp = check_path(path=dirtmp)          
          dirtmp = root_path + dirtmp+dir

          os.chdir(newdir)

          for word in linkfiles:
             try:
               os.remove(word)
               print ("\033[031mDIAG: \033[039m file \033[032m"+word+"\033[039m already exists, removing ....")
             except OSError:
                pass

             source = dirtmp+'/'+word
             dest   = newdir+'/'+word
             if os.path.exists(source):
               print ("\033[031mDIAG: \033[039m linking file \033[032m"+source+"\033[039m ....")
               linkfile = os.symlink( source, dest)

#
# check if python script
#
          if os.path.isdir(dirtmp):
            for file in os.listdir(dirtmp):
              if file.endswith(".py"):
                source = dirtmp+'/'+file
                dest   = newdir+'/'+file
                try:
                  os.remove(file)
                  print ("\033[031mDIAG: \033[039m script file \033[032m"+file+"\033[039m already exists, removing ....")
                except OSError:
                  pass

                print ("\033[031mDIAG: \033[039m linking sript file \033[032m"+source+"\033[039m ....")
                linkfile = os.symlink( source, dest)

          os.chdir(cwd)
          print("  ")


    return 


def check_path(path):
    if not(path.endswith("/")):
        path=path + "/"

    return path

def mkconfigfile(path, cwd,version, bin_dir):
    filename = path+'/config/compset.'+version

    if not(os.path.exists(filename)):
        print ("\033[031mError: \033[039m \033[031m"+version+"\033[039m verion of compiler is not available")
        print ("\033[031m       \033[039m available options are: \033[032m gfortran\033[039m")
        print ("\033[031m       \033[039m                        \033[032m gfortran_debug\033[039m")
        print ("\033[031m       \033[039m                        \033[032m x86_64\033[039m")
        print ("\033[031m       \033[039m                        \033[032m x86_64_debug\033[039m") 
        sys.exit()
        

    exec_dir=bin_dir+"/bin/"
    
    confname =  'config.mk'
    
    try:
        os.remove(confname)
        print ("\033[031mDIAG: \033[039m config.mk file \033[032m \033[039m already exists, removing ....")
    except OSError:
        pass
    
    fconfig = open(confname, 'w')
    
    fconfig.write("#\n")
    fconfig.write("SHELL              = /bin/bash\n")
    fconfig.write("#\n")

    fconfig.write("#\n")
    fconfig.write("# Compiler settings\n")
    fconfig.write("#\n")

    with open(filename) as f:
        for line in f:
           if ( not(line.startswith('#')) or not line.strip()): 
                print(line)
                fconfig.write(line)

    fconfig.write("#\n")  
    fconfig.write("#  bin_dir is the base directory where executables will be installed\n")
    fconfig.write("#\n")
    fconfig.write('bin_dir='+exec_dir)
    fconfig.write("#\n")
    fconfig.write("#\n")
    fconfig.write("#  MACHINE identifies the host machine type")
    fconfig.write("#\n")
    fconfig.write("MACHINE="+platform.machine()+"\n")
    fconfig.write("#\n")
    fconfig.write("#  Install command (or cp -p if install is not found)\n")
    fconfig.write("#\n")
    fconfig.write("INSTALL = install -c\n")
    fconfig.write("#\n")
    fconfig.write("PROJ_ROOT_PATH="+path+"\n")
    fconfig.write("MAKEDEPEND="+path+"/python_def/fort_depend.py\n")
                
    f.close()
    fconfig.close()

class file_obj:
    def __init__(self):
        self.file_name=None
        self.uses=None
        self.contains=None
        self.depends_on=None

def print_header():
     print("  ")
     print ("\033[031m**************************************************************** \033[039m")
     print ("\033[031m*                                                              * \033[039m")
     print ("\033[031m*\033[039m         this is a configure file for FLL library \033[031m            * \033[039m")
     print ("\033[031m*                                                              * \033[039m")
     print ("\033[031m**************************************************************** \033[039m")
#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='FLL configure script')
    parser.add_argument('-f','--files',nargs='+',help='Files to process')
    parser.add_argument('-D',nargs='+',action='append',metavar='NAME=DESCRIPTION',
                        help="""The macro NAME is replaced by DEFINITION in 'use' statements""")
    parser.add_argument('-b','--build',nargs=1,help='Build Directory (prepended to all files in output',
                        default='')
    parser.add_argument('-o','--output',nargs=1,help='Output file')
    parser.add_argument('-v','--verbose',action='store_true',help='explain what is done')
    parser.add_argument('-w','--overwrite',action='store_true',help='Overwrite output file without warning')
    parser.add_argument('-c','--compiler',nargs=1,help='Compiler configuration')

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

    compiler = args.compiler[0] if args.compiler else None
    
    if not compiler:
        print ("\033[031mError: \033[039m missing compiler settings, specify option \033[031m-c \033[032m")
        print ("\033[031m       \033[039m available options are: \033[032m gfortran\033[039m")
        print ("\033[031m       \033[039m                        \033[032m gfortran_debug\033[039m")
        print ("\033[031m       \033[039m                        \033[032m x86_64\033[039m")
        print ("\033[031m       \033[039m                        \033[032m x86_64_debug\033[039m") 
        sys.exit()

    run(comp=compiler,verbose=args.verbose, overwrite=args.overwrite, macros=macros, output=output, build=build)
