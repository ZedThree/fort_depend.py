#!/usr/bin/python
import os
import re

#Definitions
def run(TALK=True,OVERW=None):

    l=create_file_objs()
    mod2fil=file_objs_to_mod_dict(FIL_OBJS=l)
    depends=get_depends(fob=l,m2f=mod2fil)
    if TALK:
        for i in depends.keys():
            print "\033[032m"+i+"\033[039m depends on :\033[034m"
            for j in depends[i]: print "\t"+j
            print "\033[039m"
	tmp=write_depend(FILE="makefile.dep",dep=depends,OVERW=OVERW)
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
    import os
    tmp=os.listdir(".")
    fil=[]
    for i in EXT:
        fil.extend(filter(lambda x: x.endswith(i),tmp))
	return fil

def create_file_objs():
    l=[]
    for i in get_source():
        tmp=file_obj()

        tmp.file_name=i
        tmp.uses=get_uses(i)
        tmp.contains=get_contains(i)

        l.append(tmp)
	return l

def get_uses(FNM=None):
    import re as re
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
	return uniq_list(LIST=uses)

def get_contains(FNM=None):
    import re as re
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
    run(TALK=False,OVERW=True)
