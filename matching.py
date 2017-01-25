# -*- coding: utf-8 -*-
"""
Created on Mon Jan 23 15:03:29 2017

@author: willi
"""


import os
import pandas as pd
from datetime import datetime
from dateutil.parser import parser
import regex as re


os.chdir('C:\\Users\\willi\\Desktop\\senior design 2')
from numpy import genfromtxt

stro = pd.read_csv('hosAmb.csv')
amb = pd.read_csv('Amb.csv')

stro['arrdatetime'] = pd.to_datetime(stro['arrdatetime'])
stro2 = stro
for d in range(0,len(amb)-1):
    amb.loc[d,'AT.HOSPITAL'] = pd.to_datetime(re.sub('[()]','',amb.loc[d,'AT.HOSPITAL']))
    if d%10000==0:
        print(d)
hosDat = stro
#date = datetime.strptime(re.sub('[()]', '', ambex),'%d/%m/%y %H:%M:%S')
#date2 = datetime.strptime(strex,'%m/%d/%Y  %H:%M:%S %p')

#columns = ['hos.id', 'amb.id', 'hos.arrival', 'amb.athos', 'hos.age', 'amb.age', 'hos.sex', 'amb.sex','itteration']
columns = ['itteration','hos.id','amb.id','hos','hos.gender','hos.age','hos.arrivalTime','amb.atHospital','amb.gender','amb.age']
match = pd.DataFrame(data = None)
# increase the broadness of how much of an effect itteration has
for it in range(0,12):
    stro = stro2 
    print(len(stro),',', it)
    for i in range(0,len(stro)-1): 
        hos = stro.iloc[i]['study_id']
        y = amb[amb['HOSPITAL'] == hos]
    
        gen = stro.iloc[i]['gender']
        if it<3:
            y = y[y['GENDER']==gen]
        elif it<8:   
            g = ['U',gen]
            y = y[y['GENDER'].isin(g)]
        else:
            g = ['U','M','F']
            y = y[y['GENDER'].isin(g)]
        yrs = stro.iloc[i]['age']
        v = [yrs]
        if it>0 :
            for f in range(1,it):
                v.append((yrs+1)*it)
                v.append((yrs-1)*it)
                v.append((yrs+10)*it)
                v.append((yrs-10)*it)
                v.append((yrs-11)*it)
                v.append((yrs+11)*it)
                v.append((yrs-9)*it)
                v.append((yrs+9)*it)       
        y = y[y['AGE'].isin(v)]

        for j in range(0, len(y)-1):
            if abs((stro.iloc[i]['arrdatetime'] - y.iloc[j]['AT.HOSPITAL']).total_seconds()/60) < 30*(it*it+1):
                
                x = [[it, stro.iloc[i]['id'], y.iloc[j]['id'],hos,gen,yrs,stro.iloc[i]['arrdatetime'],y.iloc[j]['AT.HOSPITAL'],y.iloc[j]['GENDER'],y.iloc[j]['AGE']]]
                row = pd.DataFrame(data = x, columns = columns)
                match = match.append(row)
                count = count +1
                stro2 = stro2[stro2['id'] != stro.iloc[i]['id']]
                
        if i%100 == 0:
            print(i)
#take found matches out of the stro data set