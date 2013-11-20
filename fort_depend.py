#!/usr/bin/python
import os
import re

#Definitions
def run(files=None,TALK=True,OVERW=None,output="makefile.dep",macros={}):

    l=create_file_objs(files,macros)
    mod2fil=file_objs_to_mod_dict(FIL_OBJS=l)
    depends=get_depends(fob=l,m2f=mod2fil)
    if TALK:
        for i in depends.keys():
            print "\033[032m"+i+"\033[039m depends on :\033[034m"
            for j in depends[i]: print "\t"+j
            print "\033[039m"
    tmp=write_depend(FILE=output,dep=depends,OVERW=OVERW)
    return depends

def write_depend(FILE="makefile.depend",dep=[],OVERW=False):
	#Test file doesn't exist
    if os.path.exists(FILE):
        if not(OVERW):
            print "\033[031mWarning file exists.\033[039m"
            opt=raw_input("Overwrite? Y... for yes.")
        else:
            opt="y"
        if opt.lower().startswith("y"):
            pass
        else:
            return

    #Open file
    f=open(FILE,'w')
    for i in dep.keys():
        stri="\n"+i.split(".")[0]+".o"+" : "
        for j in dep[i]:
            stri=stri+" \\\n\t"+j.split(".")[0]+".o"
        stri=stri+"\n"
        f.write(stri)
    f.close()
    return

def get_source(EXT=[".f90",".F90"]):
	#Function to return all files ending with any of EXT
    tmp=os.listdir(".")
    fil=[]
    for i in EXT:
        fil.extend(filter(lambda x: x.endswith(i),tmp))
    return fil

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

def get_uses(FNM=None, macros={}):
    p=re.compile("^\s*use\s*(?P<moduse>\w*)\s*(,)?\s*(only)?\s*(:)?.*?$",re.IGNORECASE).match

    uses=[]

    #Open file
    l=open(FNM,'r')
    t=l.readlines()
    l.close()

    for i in t:
        tmp=p(i)
        if tmp:
            uses.append(tmp.group('moduse').strip())

    uniq_mods = uniq_list(LIST=uses)

    for i, mod in enumerate(uniq_mods):
        for k, v in macros.items():
            if re.match(k, mod, re.IGNORECASE):
                uniq_mods[i] = mod.replace(k,v)

    return uniq_mods

def get_contains(FNM=None):
    p=re.compile("^\s*module\s*(?P<modname>\w*?)\s*$",re.IGNORECASE).match

    contains=[]

	#Open file
    l=open(FNM,'r')
    t=l.readlines()
    l.close()

    for i in t:
        tmp=p(i)
        if tmp:
            contains.append(tmp.group('modname').strip())
    return uniq_list(LIST=contains)

def uniq_list(LIST=[]):
    keys = {}
    for e in LIST:
        keys[e] = 1
    return keys.keys()

def file_objs_to_mod_dict(FIL_OBJS=[]):
    dic={}
    for i in FIL_OBJS:
        for j in i.contains:
            dic[j.lower()]=i.file_name
    return dic

def get_depends(fob=[],m2f=[]):
    deps={}
    for i in fob:
        tmp=[]
        for j in i.uses:
            try:
                tmp.append(m2f[j.lower()])
            except:
                print "\033[031mError\033[039m module \033[032m"+j+"\033[039m not defined in any files. Skipping..."

        deps[i.file_name]=tmp

    return deps

class file_obj:
    def __init__(self):
        self.file_name=None
        self.uses=None
        self.contains=None
        self.depends_on=None


#Script
if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Generate Fortran dependencies')
    parser.add_argument('-f','--files',nargs='+',help='Files to process')
    parser.add_argument('-D',nargs='+',action='append',metavar='NAME=DESCRIPTION',
                        help="""The macro NAME is replaced by DEFINITION in 'use' statements""")
    parser.add_argument('-o','--output',nargs=1,help='Output file')
    parser.add_argument('-v','--verbose',action='store_true',help='explain what is done')
    parser.add_argument('-w','--overwrite',action='store_true',help='Overwrite output file without warning')

    args = parser.parse_args()

    macros = {}
    for arg in args.D:
        for var in arg:
            temp = var.split('=')
            macros[temp[0]] = temp[1]

    run(files=args.files,TALK=args.verbose,OVERW=args.overwrite, macros=macros, output=args.output[0])
