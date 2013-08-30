#!/usr/bin/env python
# -*- coding: utf-8 -*-

import urllib2
from BeautifulSoup import BeautifulSoup
import time
import re
import csv
import pdb
import numpy as np
import pandas as pd
import unittest
import os.path
import os, errno
import random
from urllib import urlretrieve


def extractDate(url):
    """
    """
    return re.search("((Febrero|Junio)[0-9]*)", url).group(0)

def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else: raise

def appendData(list, res):
    """
    Append one row of scores to a Pandas Dataframe
    """
    row = pd.DataFrame(list)
    res = res.append(row, ignore_index=True)
    return res

def appendList(id, score, accepted, major, faculty, ll):
    """
    Append one row of scores to a Pandas Dataframe
    """
    row = dict(id = id, 
               score = score,
               accepted = accepted,
               major = major,
               faculty = faculty)
    ll.append(row)
    return ll

def scrapeHeader(string, soup):
    """
    """
    textRe = re.compile(string + '=([0-9]*)')
    value = textRe.search(str(soup)).group(1)
    return int(value)
    

def test(df, soup, url):
    """
    Test that the scraped count equals the count in the html header and
    that the number accepted also matches the header
    We don't stop the execution when a test fails
    because there are errors in the UNAM's web pages
    """
    # The following urls have errors and should not be checked
    exceptions = ["https://servicios.dgae.unam.mx/Junio2012/resultados/4/4337005.html",
                  "https://escolar0.unam.mx/Junio2011/resultados/3/3050075.html",
                  "https://escolar0.unam.mx/Junio2011/resultados/3/3054005.html"]
    #total number of persons who took the test
    total = scrapeHeader("ASPIRANTES", soup)
    minimo = scrapeHeader("MINIMOS", soup)
    sel = scrapeHeader("SELECCIONADOS", soup)
    try:
        assert (df.count(0)[1] == total)
    except:
        print('#################################')
        print("# The total count doesn't match")
        print("# the total in the header of the")
        print("# html page")
        print("################################")
        print("value on page: ", total)
        print("value scraped: ", df.count(0)[1])
        print("scraping page: " + url)
        print("\n")
        if url in exceptions:
            print "this is a known error. No exception raised\n"
            pass
        else:
            raise
    #make sure the number of students accepted matches and
    #make sure the number of accepted students > 0
    try:
        if sel != 0:
            assert (df[(df.accepted == "A")].score.astype("int").min() >= minimo)
    except:
        print('#################################')
        print("# The minimum score doesn't match")
        print("# the minimum in the header of the")
        print("# html page")
        print("################################")
        print("value on page: ", minimo)
        print("value scraped: ", \
            df[(df.accepted == "A")].score.astype("int").min())
        print("scraping page: " + url)
        print("\n")
        if url in exceptions:
            print "this is a known error. No exception raised\n"
            pass
        else:
            raise
        raise
    # seleccionados = re.compile('SELECCIONADOS=([0-9]*)')
    # sel = seleccionados.search(str(soup)).group(1)
    # assert (res[(res.faculty == faculty) & 
    #             (res.major == major)& 
    #             (res.accepted == "A")].accepted.count() == int(sel))


def cleanHTML(data, res, date, soup, url):
    """
    Convert the crappy html to a pandas dataframe

    The html we want to scrap looks like this:

    000187 93
    000196 101 A Cita para entregar documentación.
    000223 73

    The long number is the id of the student the next number the test
    score and the A means the student was accepted
    """
    regmajor = re.compile('[ \n]+$')
    major = regmajor.split(soup.html.body.center.h1.contents[0])[0]
    faculty = regmajor.split(soup.html.body.center.h1.contents[2])[0]
    
    folio = re.compile('([0-9]+) +([A-Z])')
    a = re.compile('([0-9]+) +(A)')
    ll = []
   
    data = [x for x in data if x != "\n"]
    i = 1
    while i<len(data):
          #skip
          if data[i] == 'Cita para entregar documentaci&oacute;n.':
              i = i + 1
              continue
          m = folio.search(data[i])
          #if the student was rejected
          if m:
              appendList(m.group(1) + date, 
                         m.group(2),
                         'R',
                         major,
                         faculty, ll)
              i=i+1
          else:
            if i < len(data)-1:
              m = a.search(data[i+1])
              if m:
                appendList(data[i] + date,
                           m.group(1).encode('utf-8'),
                           m.group(2).encode('utf-8'),
                           major,
                           faculty,
                           ll)
              else:
                appendList(data[i] + date,
                           data[i+1],
                           'R',
                           major,
                           faculty,
                           ll)
              i=i+2
            else:
              i=i+2
    #convert the linked list to a pandas dataframe
    tempDF = pd.DataFrame(ll)
    #Make sure no mistakes were made
    test(tempDF, soup, url)
    #add to the dataframe containing all entries
    res = res.append(tempDF, ignore_index=True)
    return res
    
def downloadHTML(pathToFile, url):
    """
    """
    # try the cache before hitting the unam web server
    if os.path.isfile(pathToFile):
        # print("scraping cached page " + url
        f = open(pathToFile, 'r')
        page = f.read()
        soup = BeautifulSoup(page)
        # no cache found
    else:
        # be a good internet citizen and wait 5s before downloading
        time.sleep(5 + random.random())
        # print("scraping page " + url
        try:
            request = urllib2.Request(url)
            page = urllib2.urlopen(request)
        except:
            print("Something went wrong when connecting to the UNAM")
            raise
        ## cache the page so that if we run the program again we
        # don't hit the unam's webserver repeatedly
        soup = BeautifulSoup(page)
        f = open(pathToFile, 'w')
        f.write(str(soup))
    f.close
    return soup


def get_data(majors, date, web):
    """
    read data from the UNAM website
    """
    #create an empty dataframe for storing the scraped data
    res =  pd.DataFrame(columns=('id', 'score', 'accepted', 'major', 'faculty'))
    mkdir_p("../../cache/pages/" + date)
        
    #for each of the majors in each area of study 
    #(Math and Eng/Social Science/Biological/etc)
    for con in majors:
        url = con['href']
        url = web + url
        filename = re.search('[0-9]+\.html', url).group(0)
        path = os.path.join("../../cache/pages/", date, filename)
        soup = downloadHTML(path, url)
        #An array to store all the strings in the html
        data = []
        for i in soup.body:
            if i.string is not None:
                data.append(i.string.encode('utf-8'))
        res = cleanHTML(data, res, date, soup, url)
    return res


def removeWhiteSpace(string):
    return string.replace(" ", "")

def getDate(string):
    string = re.search('Febrero[0-9]+|Junio[0-9]+', string).group(0)
    string = string.replace("Febrero", "Feb ")
    string = string.replace("Junio", "Jun ")
    return string

def main():
    """
    """
    print("Starting scrapper...\n")
    #results = open('results.csv','wb')
    #writer = csv.writer(results, dialect='excel')
    #def main():
    unam = pd.DataFrame(columns=('id', 'area', 'score', 
                                 'accepted', 'major', 'faculty'))
    temp = pd.DataFrame(columns=('id', 'area', 'score', 
                                 'accepted', 'major', 'faculty'))
    
    options = {"15.html" : "Ciencias Físico-Matemáticas y las Ingenierías", 
            "25.html" : "Biológicas, Químicas y de la Salud", 
            "35.html" : "Ciencias Sociales", 
            "45.html" : "Humanidades y Artes"}
    #the urls if the results
    urls = ["Junio2013/",
            "Febrero2013/",
            "Junio2012/",
            "Febrero2012/"]
    urls = map(lambda x: "https://servicios.dgae.unam.mx/" + x \
               + "resultados/", urls)
    #For some reason the June 2011 results are stored in a different server
    urls.append("https://escolar0.unam.mx/Junio2011/resultados/")
   
    ## Scrape the data for each of the exam dates    
    for url in urls:
        #The results are grouped by area 15 == Math / Engineering
        #25 - Medicine / Biology, etc
        for area in ["15.html", "25.html", "35.html", "45.html"]:
            date = extractDate(url)
            soup = downloadHTML("../../cache/index/" + \
                                date \
                                + "-" + area, url + area)
            majors = soup.findAll('a', href = re.compile("^[1234]/*"))
            
            temp = get_data(majors, date, url)
            temp['area'] = options[area]
            unam = unam.append(temp, ignore_index=True)
            

    unam.id = unam.id.map(str.strip)
    unam.score = unam.score.map(str.strip)
    unam.accepted = unam.accepted.map(str.strip)
    unam.id = unam.id.map(removeWhiteSpace)
    # Dear Future Diego, if for any reason you feel the need to consult
    # the code in this file here's one tip: Do not use python 2.7 for
    # unicode. This is so important that I'll have a cow say it
    #
    #__________________________________
    # / Diego: Do not use Python 2.7 for \
    # \ anything that requires unicode   /
    #  ----------------------------------
    #         \   ^__^
    #          \  (oo)\_______
    #             (__)\       )\/\
    #                 ||----w |
    #                 ||     ||
    unam['major'][(unam.major == u"(405)  DISEﾃ前 Y COMUNICACION VISUAL")] = u"(405)  DISEÑO Y COMUNICACION VISUAL"
    unam['major'][(unam.major == u"(405)  DISEÑO Y COMUNICACION VISUAL")] = u"(405)  DISEÑO Y COMUNICACION VISUAL"

    unam['major'][(unam.major == u"(105)  DISEﾃ前 INDUSTRIAL")] = u"(105)  DISEÑO INDUSTRIAL"
    unam['major'][(unam.major == u"(105)  DISEÑO INDUSTRIAL")] = u"(105)  DISEÑO INDUSTRIAL"

    unam['major'][(unam.major == u"(434)  ARTE Y DISEﾃ前")] = u"(434)  ARTE Y DISEÑO"

    unam['major'][(unam.major == u"(406)  DISEﾃ前 GRAFICO")] = u"(406)  DISEÑO GRAFICO"
    unam['major'][(unam.major == u"(406)  DISEÑO GRAFICO")] = u"(406)  DISEÑO GRAFICO"

    unam['major'][(unam.major == u"(408)  ENSEĂ&lsquo;ANZA DE INGLES")] = u"(408)  ENSEÑANZA DE INGLES"
    unam['major'][(unam.major == u"(408)  ENSEÑANZA DE INGLES")] = u"(408)  ENSEÑANZA DE INGLES"

    unam['faculty'][(unam.faculty == u"(02005)  ENAP")] = u"(02005)  ESCUELA NACIONAL DE ARTES PLASTICAS"

    unam['date'] = unam.id.map(getDate)
    unam.to_csv('../../clean-data/unam-admission.csv',
                encoding='utf-8',
                index = False)
    #pdb.set_trace()

if __name__ == '__main__':
    main()
