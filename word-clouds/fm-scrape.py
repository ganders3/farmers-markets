#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov 14 17:54:39 2020

@author: gregory
"""

import names, re, urllib
from bs4 import BeautifulSoup

#url = 'https://seattlefarmersmarkets.org/about-us'
url = 'https://qafm.org/about'
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
    
def prepareDict(dictionary):
    NON_WORD = '[^A-z]'
    EXCLUDE = ['and', 'all', 'anne', 'are',
               'for', 'from',
               'here',
               'our', 
               'qafm', 'qafms', 'queen',
               'that', 'them', 
               'were', 'with', 
               'you']
    d = dict()
    for item in dictionary:
        new = re.sub(NON_WORD, '', item)
        print(new)
        if len(new) > 2 and not(any(new in e for e in EXCLUDE)):
            d[new] = dictionary[item]
#            remove.append(item)
    return(d)
    
    
    
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
freq = prepareDict(freq)
sortFreq = sortDict(freq, True)

# Calculate the most and least occurring words
highest = None
lowest = None
top = 50
for k in sortFreq[:top]:
    if highest is None or highest < freq[k]:
        highest = freq[k]
    if lowest is None or lowest > freq[k]:
        lowest = freq[k]
print('Range of counts:', highest, lowest)

textLarge = 80
textSmall = 20

fhand = open('wordcloud.js', 'w')
fhand.write("wordcloud = [")
first = True
for k in sortFreq[:top]:
    if not first: fhand.write(",\n")
    first = False
    size = freq[k]
    size = (size - lowest) / (highest - lowest)
    size = int((size * textLarge) + textSmall)
    fhand.write("{text: '" + k + "', size: " + str(size) + "}")
fhand.write("\n];\n")
fhand.close()
