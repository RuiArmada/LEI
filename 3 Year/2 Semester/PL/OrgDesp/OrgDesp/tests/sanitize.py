#!/usr/bin/env python
from time import time
import re

def sanitize():
    inp = '<p> Somos os melhores\' & mais nada\"!'
    print(inp)
    inp = re.sub(r"\&", "&amp;", inp)
    inp = re.sub(r"\s", "&nbsp;", inp)
    inp = re.sub(r"\<", "&lt;", inp)
    inp = re.sub(r"\>", "&gt;", inp)
    inp = re.sub(r"\"", "&quot;", inp)
    inp = re.sub(r"\'", "&apos;", inp)
    print(inp)


if __name__ == "__main__":
    sanitize()
