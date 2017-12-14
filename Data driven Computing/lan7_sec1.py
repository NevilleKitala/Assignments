#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 16:22:41 2017

@author: asiyambarawa
"""
def countWords(filename):
    mydict = {}
    file = open(filename,'r')
    for line in file:
        word = line.split()
        for w in word:
            if (w in mydict):
                mydict[w]+=1
            else:
                mydict[w]=1
    words = list(mydict.keys())
    words.sort(reverse=True, key=lambda v:mydict[v])
    for n in range(20):
        word =words[n]
        print(n+1, ':', word, '=', mydict[word])

def printTop20(mydict):
    mydict = {}
    file = open('mobypara.txt','r')
    for line in file:
        word = line.split()
        for w in word:
            if (w in mydict):
                mydict[w]+=1
            else:
                mydict[w]=1
    words = list(mydict.keys())
    words.sort(reverse=True, key=lambda v:mydict[v])
    for n in range(20):
        word =words[n]
        print(n+1, ':', word, '=', mydict[word])
        
def readStopWords(filename):
    stops = []
    file2 = open(filename,'r')
    for line in file2:
        word=line.strip()
        stops.append(word)
    return stops
    
    
def countWords2(filename):  
    stops = []
    file2 = open('stopwords.txt','r')
    for line in file2:
        word=line.strip()
        stops.append(word)
    mydict = {}
    file = open(filename,'r')
    for line in file:
        word = line.split()
        for w in word:
            if w not in stops:
               if (w in mydict):
                   mydict[w]+=1
               else:
                   mydict[w]=1
    words = list(mydict.keys())
    words.sort(reverse=True, key=lambda v:mydict[v])
    for n in range(20):
        word =words[n]
        print(n+1, ':', word, '=', mydict[word])
    return None


def similarity(file1,file2):
    N = 0
    mydict1 = {}
    file = open(file1,'r')
    for line in file:
        word = line.split()
        for w in word:
            if (w in mydict1):
                mydict1[w]+=1
            else:
                mydict1[w]=1
    mydict2 = {}
    file = open(file2,'r')
    for line in file:
        word = line.split()
        for w in word:
            if (w in mydict2):
                mydict2[w]+=1
            else:
                mydict2[w]=1 
    for text in mydict1:
        if text in mydict2:
            N+=1
    s1 = len(mydict1)
    s2 = len(mydict2)
    sim=N/((s1+s2)-N)
    
    return sim
        
        