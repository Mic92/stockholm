#!/usr/bin/env python3

# Usage:
# _from=krebs state_dir=. python sed-plugin.py 'dick butt'
# _from=krebs state_dir=. python sed-plugin.py 's/t/l/g'
## dick bull
import shelve
from os import environ
from os.path import join
from sys import argv
d = shelve.open(join(environ['state_dir'],'sed-plugin.shelve'),writeback=True)
usr = environ['_from']
import re

def is_regex(line):
    myre = re.compile(r'^s/(?:\\/|[^/])+/(?:\\/|[^/])*/[ig]?$')
    return myre.match(line)

line = argv[1]

if is_regex(line):
    last = d.get(usr,None)
    if last:
        from subprocess import Popen,PIPE
        p = Popen(['sed',line],stdin=PIPE,stdout=PIPE)
        so,se = p.communicate(bytes("{}\n".format(last),"UTF-8"))
        if p.returncode:
            print("something went wrong when trying to process your regex: {}".format(se.decode()))
        ret = so.decode()
        print("\x1b[1m{}\x1b[0m meinte: {}".format(usr,ret.strip()))
        if ret:
            d[usr] = ret

    else:
        print("no last message")
else:
    d[usr] = line

d.close()
