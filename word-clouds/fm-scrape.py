#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov 14 17:54:39 2020

@author: gregory
"""

import urllib, re
from bs4 import BeautifulSoup

url = 'https://seattlefarmersmarkets.org/about-us'
#url = 'https://qafm.org/about'
#url = 'https://www.sfmamarkets.com/mission-and-history'
htmlRead = urllib.request.urlopen(url).read()
soup = BeautifulSoup(htmlRead, 'html.parser')


def wordFrequency(string):
    wf = dict()
    string = string.lower()
    string = string.split(' ')
    string = [i.strip() for i in string]
    for s in string:
        if s in wf:
            wf[s] += 1
        else:
            wf[s] = 1
    return(wf)

def singleWordFreq(string, word):
    string = string.lower()
    return(string.count(word))

def sortDict(dictionary, descending):
    sd = sorted(dictionary, key = dictionary.get, reverse = descending)
    return(sd)
    
    
    
words = ''
# ^[hp][1-6]?$ - start with p or h, and then 0 or 1 of [1-6]
# this captures <p> and <h1> thru <h6>
expr = re.compile('^[ph][1-6]?$')
tags = soup.find_all(expr)
for tag in tags:
        contents = tag.contents
        for content in contents:
            if not content.string == None:
#                print('two')
                words += content.string + ' '

freq = wordFrequency(words)
sortFreq = sortDict(freq, True)

highest = None
lowest = None
for k in sortFreq[:100]:
    if highest is None or highest < freq[k]:
        highest = freq[k]
    if lowest is None or lowest > freq[k]:
        lowest = freq[k]
print('Range of counts:', highest, lowest)

bigSize = 80
smallSize = 20

fhand = open('wordcloud.js', 'w')
fhand.write("wordcloud = [")
first = True
for k in sortFreq[:100]:
    if not first: fhand.write(",\n")
    first = False
    size = freq[k]
    size = (size - lowest) / float(highest - lowest)
    size = int((size * bigSize) + smallSize)
    fhand.write("{text: '" + k + "', size: " + str(size) + "}")
fhand.write("\n];\n")
fhand.close()