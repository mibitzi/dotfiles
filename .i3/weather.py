#!/usr/bin/env python3

import urllib.request
from sys import argv
import json


def get_weather(woeid):
    query = ("select * from weather.forecast "
             "where woeid={} and u='c'".format(woeid))
    query = query.replace(' ', '%20')

    url = ("https://query.yahooapis.com/v1/public/yql?q=" + query +
           "&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys")

    data = json.loads(urllib.request.urlopen(url).read().decode())
    channel = data['query']['results']['channel']
    condition = channel['item']['condition']

    text = "{}, {}{}".format(condition['text'],
                             condition['temp'],
                             channel['units']['temperature'])

    print(text)

if __name__ == '__main__':
    if len(argv) != 2:
        print("usage: ./weather.py woeid")
        exit(-1)

    get_weather(argv[1])
