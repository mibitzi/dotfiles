#!/usr/bin/env python

from json import loads
from os import popen
from sys import argv


def ipc(req="command", msg=""):
    ans = popen("i3-msg -t " + req + " " + msg).readlines()[0]
    print(ans)
    return loads(ans)


def get_focused_workspace():
    for w in ipc('get_workspaces'):
        if w['focused']:
            return w


def rename_ws(name):
    num = get_focused_workspace()['num']
    ipc(msg='rename workspace to {}:{}'.format(num, name))


if __name__ == '__main__':
    if len(argv) != 2:
        print("Usage rename-ws.py new-name")
        exit(-1)
    rename_ws(argv[1])
