# -*- coding: utf-8 -*-
"""
Created on Sat Apr 15 23:29:20 2020

@author: rafaeljmoraes
"""
import pandas as pd
import re

def parse_title(file, match):
    print("Parsing file with title: '", match['title'], "'." )
    return file, match
    
def parse_zone(tecplot_file, match):
    print("Parsing ZONE '", match['name'], "'.")
    number_of_lines = int(match['I'])
    
    lines = []
    for i in range(number_of_lines):
        lines.append(tecplot_file.readline())
    
    data = [line.split() for line in lines]
    
    df = pd.DataFrame(data)
    df = df.apply(pd.to_numeric, errors='coerce', downcast='float')
    
    return tecplot_file, df
    
def parse_variable(file, match):
    # TODO: not taking any action when VARIABLE is read. We could, for instance
    # check if the total number of variable names provided is consistent with
    # the total number of data columns, or use the variable names as column
    # headers in the data frame
    print("Parsing VARIABLE")
    return file, match
    
def reader(teplot_file_name, variable):
    
    tecplot_keywords = {}
    tecplot_keywords['TITLE']       = re.compile(r'\s*TITLE\s+\=\s+\"(?P<title>.+?)\"')
    tecplot_keywords['VARIABLES']   = re.compile(r'\s*VARIABLES\s+\=\s+(".+?")+')
    tecplot_keywords['ZONE']        = re.compile(r'\s*ZONE\s+T="(?P<name>.+?)"\s+F\s*=\s*(.+?)\,\s*I\s*=\s*(?P<I>\d+),\s*J\s*=(?P<J>\d+),\s*K\s*=\s*(?P<K>\d+)')
    
    parsers = {}
    parsers['TITLE']       = parse_title
    parsers['VARIABLES']   = parse_variable
    parsers['ZONE']        = parse_zone
    
    tecplot_file = open(teplot_file_name, 'r')
    
    df = None
    stop_parsing = False
    for line in tecplot_file:
        for keyword, regex in tecplot_keywords.items():
            match = regex.match(line)
            if (match is not None):
                tecplot_file, df = parsers[keyword](tecplot_file, match)
                if(keyword == 'ZONE'):
                    if(match['name'] == variable):
                        stop_parsing = True
                        break
        if (stop_parsing):
            break
                
    print(r'Done!')
    
    tecplot_file.close()
    
    return df

if __name__ == '__main__':
    obs_file_name = r'C:\Users\b88i\dev\cygwin\home\b88i\dev\projects\EnKF_seir\run\obs.dat'
    reader(obs_file_name, "Observed hospitalized")
    reader(obs_file_name, "Observed deaths")
    