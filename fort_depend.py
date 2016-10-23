#!/usr/bin/python
import os
import re
import glob
import fnmatch
import os
import sys

#Definitions

def run(path,files=None,verbose=True,overwrite=None,output=None,macros={},build=''):
   
    cwd = os.getcwd()
    
    path = check_path(path=path)
    cwd = check_path(path=cwd)

    ff=get_all_files(path=path) 
    l=create_file_objs(files,macros)
    mod2fil=file_objs_to_mod_dict(file_objs=l)
    depends=get_depends(fob=l,m2f=mod2fil,ffiles=ff)

    if verbose:
       for i in depends.keys():
           print ("\033[032m"+i+"\033[039m depends on :\033[034m")
           for j in depends[i]: print( "\t"+j)
           print ("\033[039m")

    if output is None:
        output = "makefile.dep"

    tmp=write_depend(path=path,cwd=cwd,outfile=output,dep=depends,overwrite=overwrite,build=build)

    return depends

def write_depend(path,cwd,outfile="makefile.dep",dep=[],overwrite=False,build=''):
    "Write the dependencies to outfile"
    #Test file doesn't exist
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

    #Open file
    f=open(outfile,'w')
    f.write('# This file is generated automatically. DO NOT EDIT!\n')
    for i in dep.keys():
        tmp,fil=os.path.split(i)
        stri="\n"+os.path.join(build, fil.split(".")[0]+".o"+" : ")
        for j in dep[i]:
            npathseg = j.count('/')
            if npathseg == 0:
                tmp,fil=os.path.split(j)
            else:
                fil = get_relative_path_name(j,path=path,cwd=cwd)

            if "../" in fil:
                stri = stri + " \\\n\t" + fil
            else:
                stri=stri+" \\\n\t"+os.path.join(build, fil.split(".")[0]+".o")
                
        stri=stri+"\n"
        f.write(stri)
    f.close()
    return

def get_source(ext=[".f90",".F90"]):
    "Return all files ending with any of ext"
    tmp=os.listdir(".")
    fil=[]
    for i in ext:
        fil.extend(filter(lambda x: x.endswith(i),tmp))
    return fil

def get_all_files(path):
    #l=[] 
    
    #for filename in glob.iglob('/home/jka/OSS_CFD/trunk/**/*.f90', recursive=True):
        #print(filename)
        #l.append(filename)
        
    #return l
    
    matches = []
    for root, dirnames, filenames in os.walk(path):
        for filename in fnmatch.filter(filenames, '*.f90'):
            matches.append(os.path.join(root, filename))
        
    return matches

def check_if_there(use,file):
    "return if you see module name"
    with open(file) as f:
        for line in f:
            if "module" in line.lower():
                if use in line:
                    return 1

    return 0


def create_file_objs(files=None, macros={}):
    l=[]

    if files is None:
        files = get_source()

    for i in files:
        source_file = file_obj()

        source_file.file_name = i
        source_file.uses = get_uses(i,macros)
        source_file.contains = get_contains(i)

        l.append(source_file)

    return l

def get_uses(infile=None, macros={}):
    "Return which modules are used in infile after expanding macros"
    p=re.compile("^\s*use\s*(?P<moduse>\w*)\s*(,)?\s*(only)?\s*(:)?.*?$",re.IGNORECASE).match

    uses=[]

    with open(infile,'r') as f:
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

def get_contains(infile=None):
    "Return all the modules that are in infile"
    p=re.compile("^\s*module\s*(?P<modname>\w*)",re.IGNORECASE).match

    contains=[]

    with open(infile,'r') as f:
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

def get_depends(fob=[],m2f=[], ffiles=[]):
    deps={}
    for i in fob:
        tmp=[]
        for j in i.uses:
            try:
                tmp.append(m2f[j.lower()])
            except KeyError:
                for k in ffiles:
                    retval=check_if_there(use=j,file=k)
                    if retval > 0:
                        name=os.path.splitext(k)[0]+'.o'
                        tmp.append(name.lower())
                        print ("\033[031mNote: \033[039m module \033[032m"+j+"\033[039m not defined in any files in this directory")
                        print ("\033[031m..... \033[039m module is in \033[032m"+name+"\033[039m file")
                        print ("\033[031m..... \033[039m adding it to dependency file, not checking its dependency further \033[032m")

        deps[i.file_name]=tmp

    return deps

def check_path(path):
    if path.endswith("/"):
        print("Path correct")
    else:
        print( "adding / to the path in "+path)
        path=path + "/"

    return path

def get_relative_path_name(file,path,cwd):
    length = len(path)
    #tmp,fil=os.path.split(j)
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
        self.contains=None
        self.depends_on=None


#Script
if __name__ == "__main__":
    import argparse

    # Add command line arguments
    parser = argparse.ArgumentParser(description='Generate Fortran dependencies')
    parser.add_argument('-f','--files',nargs='+',help='Files to process')
    parser.add_argument('-D',nargs='+',action='append',metavar='NAME=DESCRIPTION',
                        help="""The macro NAME is replaced by DEFINITION in 'use' statements""")
    parser.add_argument('-b','--build',nargs=1,help='Build Directory (prepended to all files in output',
                        default='')
    parser.add_argument('-o','--output',nargs=1,help='Output file')
    parser.add_argument('-v','--verbose',action='store_true',help='explain what is done')
    parser.add_argument('-w','--overwrite',action='store_true',help='Overwrite output file without warning')
    parser.add_argument('-r','--root_dir',nargs=1,help='Project root directory')

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
    
    if not root_dir:
        print ("\033[031mError: \033[039m missing path to project root directory \033[032m")
        sys.exit()

    run(path=root_dir, files=args.files, verbose=args.verbose, overwrite=args.overwrite, macros=macros, output=output, build=build)
