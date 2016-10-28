
import fnmatch
import os

matches = []
for root, dirnames, filenames in os.walk('/home/jka/OSS_CFD/trunk/accessories/ll_cat'):
    for filename in fnmatch.filter(filenames, '*.f90'):
        matches.append(os.path.join(root, filename))
        print(filename)
    
