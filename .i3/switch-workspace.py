#!/usr/bin/env python
# xmonad-like workspace handling for i3
# based on https://travisf.net/i3-wk-switcher/

from json import loads
from os import popen
from sys import argv
import logging


def ipc(req="command", msg=""):
    ans = popen("i3-msg -t " + req + " " + msg).readlines()[0]
    print(ans)
    return loads(ans)


def get_focused_workspace():
    for w in ipc('get_workspaces'):
        if w['focused']:
            return w


def get_active_outputs():
    return [out for out in ipc('get_outputs') if out['active']]


def get_workspace(num):
    ws = [w for w in ipc('get_workspaces') if w['num'] == num]
    return ws[0] if ws else None


def switch_workspace(num):
    ipc(msg='workspace number {}'.format(num))


def swap_visible_workspaces(a, b):
    switch_workspace(a['num'])
    ipc(msg='move workspace to output {}'.format(b['output']))

    switch_workspace(b['num'])
    ipc(msg='move workspace to output {}'.format(a['output']))


def change_workspace(num):
    focused = get_focused_workspace()

    if focused['num'] == num:
        logging.info('already on correct workspace')
        return

    want_ws = get_workspace(num)
    if not want_ws:
        logging.info('switching to workspace because it does not exist')
        switch_workspace(num)
        return

    other_out = [out for out in get_active_outputs()
                 if out['name'] == want_ws['output']][0]

    other_ws = [ws for ws in ipc('get_workspaces')
                if ws['name'] == other_out['current_workspace']][0]

    if focused['output'] == want_ws['output']:
        logging.info('wanted workspace already on focused output, '
                     'switching as normal')
        switch_workspace(num)
        return

    if not want_ws['visible']:
        logging.info('workspace to switch to is on other output, not showing')
        switch_workspace(num)

    swap_visible_workspaces(want_ws, focused)

    switch_workspace(other_ws['num'])
    switch_workspace(want_ws['num'])


if __name__ == "__main__":
    if len(argv) != 2:
        print("Usage: switch-workspace.py workspace-num")
        exit(-1)

    change_workspace(int(argv[1]))
