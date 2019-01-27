#!/usr/bin/env python3

# Usage:
# _from=krebs state_dir=. python sed-plugin.py 'dick butt'
# _from=krebs state_dir=. python sed-plugin.py 's/t/l/g'
# > dick bull
import shelve
from os import environ
from os.path import join
from sys import argv
from time import sleep
import re

# try to open the shelve file until it succeeds
while True:
    try:
        d = shelve.open(
            join(environ['state_dir'], 'sed-plugin.shelve'),
            writeback=True
        )
        break
    except:  # noqa: E722
        sleep(0.2)
usr = environ['_from']


def is_regex(line):
    myre = re.compile(r'^s/(?:\\/|[^/])+/(?:\\/|[^/])*/[ig]?$')
    return myre.match(line)


line = argv[1]

if is_regex(line):
    last = d.get(usr, None)
    if last:
        from subprocess import Popen, PIPE
        p = Popen(['sed', line], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        so, se = p.communicate(bytes("{}\n".format(last), "UTF-8"))
        if p.returncode:
            print("something went wrong when trying to process your regex: {}".format(line.strip()))
        ret = so.decode()
        if len(ret) > 512:
            print('message to long, skipped')
        elif len(ret.split('\n')) > 5:
            print('to many lines, skipped')
        else:
            if last.strip() != ret.strip():
                print("\x02{}\x02 meant: {}".format(usr, ret.strip()))
                if ret:
                    d[usr] = ret

    else:
        print("no last message")
else:
    d[usr] = line

d.close()
