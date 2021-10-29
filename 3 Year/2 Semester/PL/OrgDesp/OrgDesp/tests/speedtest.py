#!/usr/bin/env python
from time import time
import re

def main():

    data = ["test 123" for _ in range(999999)]

    #######################################

    pre_lc = time()
    [re.search(r'^\s*$', x) for x in data]
    lc = time() - pre_lc

    #######################################

    fll = []
    pre_fl = time()    
    for x in data:
        fll.append(re.search(r'^\s*$', x))
    fl = time() - pre_fl

    #######################################

    mwl = []
    def search_func(x):
        mwl.append(re.search(r'^\s*$', x))

    pre_mw = time()
    map(search_func, iter(data))
    mw = time() - pre_mw

    #######################################

    pre_ml = time()
    mll = []
    map(lambda x: mll.append(re.search(r'^\s*$', x)), iter(data))
    ml = time() - pre_ml

    #######################################

    print(f"List Comprehendsion : {lc}.")
    print(f"For Loop            : {fl}.")
    print(f"Map(without lambda) : {mw}.")
    print(f"Map(lambda)         : {ml}.")

if __name__ == "__main__":
    main()
