# -*- coding: utf-8 -*-
"""
NFIRS Data Blending Script

Description: This script blends the geocoded addresses generated from 
             a previous phase of this project to the NFIRS source data so that
             insights related to fire alarms present at fires can be derived.

Google Drive Data Link: https://drive.google.com/folderview\
                        ?id=0BxhID98AlD4ZcEg4RmpKd2tPSVE\
                        &usp=drive_web&tid=0Bxt-Sxy6HRaxZzhyeFRkUVRvckE#list

Created on Wed Jan 06 18:08:29 2016

@author: dtromero

Setting Data Working Directory
Setting the source directory for the data needed for this script. Data 
folder path and files required for this script can be downloaded from 
the google drive link in the description.

File structure setup for this project is as follows:
    nfirs_data/
    nfirs_data/NFIRS_2009_092710.zip
    nfirs_data/nfirs_2010_100711.zip
    nfirs_data/nfirs_2011_120612.zip
    nfirs_data/NFIRS_2012_052714.zip
    nfirs_data/NFIRS_2013_121514.zip   
    geocoded_addresses/
    geocoded_addresses/2009.zip
    geocoded_addresses/2010.zip
    geocoded_addresses/2011.zip
    geocoded_addresses/2012.zip
    geocoded_addresses/2013.zip
    output/
* Note: each zip file is unzipped into a new folder with the same name
"""
import os
import pandas as pd
pd.set_option('display.max_columns', None)
import pysal as ps
import numpy as np

folder_path = 'C:\\Users\\danromero\\Desktop\\DataKindDC\\data\\'
os.chdir(folder_path)
os.getcwd()

# function copied from https://gist.github.com/ryan-hill/f90b1c68f60d12baea81
# fairly slow function, significant run time
def dbf2DF(dbfile): #Reads in DBF files and returns Pandas DF
    db = ps.open(dbfile) #Pysal to open DBF
    d = {col: db.by_col(col) for col in db.header} #Convert dbf to dictionary
    pandasDF = pd.DataFrame(d) #Convert to Pandas DF
    db.close() 
    pandasDF = pandasDF.replace(r'', np.nan, regex=True)
    return pandasDF
    

filesDict = \
    {'2009': {'type':'dbf',
       'files': {
         'nfirs_bi':'nfirs_data\\NFIRS_2009_092710\\basicincident.dbf',
         'nfirs_ia':'nfirs_data\\NFIRS_2009_092710\\incidentaddress.dbf',
         'nfirs_fi':'nfirs_data\\NFIRS_2009_092710\\fireincident.dbf',
         'geo_add' :'geocoded_addresses\\2009\\2009_geocoded_addresses.csv',
         'frmt_add':'geocoded_addresses\\2009\\2009_formated_addresses.csv'
          }
      },
     '2010': {'type':'dbf',
        'files': {
         'nfirs_bi':'nfirs_data\\nfirs_2010_100711\\basicincident.dbf',
         'nfirs_ia':'nfirs_data\\nfirs_2010_100711\\incidentaddress.dbf',
         'nfirs_fi':'nfirs_data\\nfirs_2010_100711\\fireincident.dbf',
         'geo_add' :'geocoded_addresses\\2010\\2010_geocoded_addresses.csv',
         'frmt_add':'geocoded_addresses\\2010\\2010_formated_addresses.csv'
          }
      },
     '2011': {'type':'dbf',
        'files': {
         'nfirs_bi':'nfirs_data\\nfirs_2011_120612\\basicincident.dbf',
         'nfirs_ia':'nfirs_data\\nfirs_2011_120612\\incidentaddress.dbf',
         'nfirs_fi':'nfirs_data\\nfirs_2011_120612\\fireincident.dbf',
         'geo_add' :'geocoded_addresses\\2011\\2011_geocoded_addresses.csv',
         'frmt_add':'geocoded_addresses\\2011\\2011_formated_addresses.csv'
          }
      },
     '2012': {'type':'txt',
        'files': {
         'nfirs_bi':'nfirs_data\\NFIRS_2012_052714\\basicincident.txt',
         'nfirs_ia':'nfirs_data\\NFIRS_2012_052714\\incidentaddress.txt',
         'nfirs_fi':'nfirs_data\\NFIRS_2012_052714\\fireincident.txt',
         'geo_add' :'geocoded_addresses\\2012\\2012_geocoded_addresses.csv',
         'frmt_add':'geocoded_addresses\\2012\\2012_formated_addresses.csv'
          }
      },
     '2013': {'type':'txt',
        'files': {
         'nfirs_bi':'nfirs_data\\NFIRS_2013_121514\\basicincident.txt',
         'nfirs_ia':'nfirs_data\\NFIRS_2013_121514\\incidentaddress.txt',
         'nfirs_fi':'nfirs_data\\NFIRS_2013_121514\\fireincident.txt',
         'geo_add' :'geocoded_addresses\\2013\\2013_geocoded_addresses.csv',
         'frmt_add':'geocoded_addresses\\2013\\2013_formated_addresses.csv'
          }
      }
    }


nfirs_all_years = pd.DataFrame()
for f in filesDict:
    
    fileType = filesDict[f]['type']
    nfirs_bi_fp = filesDict[f]['files']['nfirs_bi']
    nfirs_ia_fp = filesDict[f]['files']['nfirs_ia']
    nfirs_fi_fp = filesDict[f]['files']['nfirs_fi']
    geo_add_fp  = filesDict[f]['files']['geo_add']
    frmt_add_fp = filesDict[f]['files']['frmt_add']
    
    if fileType == 'dbf':
        print f, fileType
        
                # geocoded addresses provided as part of earlier geocoding project
        geo_add = dbf2DF(geo_add_fp)\
                                 [['row_seq', 'rating', 'lon', 'lat',\
                                   'tabblock_id','county', 'tractid',\
                                   'geomout', 'output_address']]
        
        # formatted addresses from raw nfirs data, needed to join geocoded back to raw
        frmt_add = dbf2DF(frmt_add_fp)\
                                [['STATE', 'FDID', 'INC_DATE', 'INC_NO',\
                                  'EXP_NO','row_seq','input_address']]
        
        # nfirs address database needed to join back to formatted data
        nfirs_ia = dbf2DF(nfirs_ia_fp)\
                                [['STATE', 'FDID', 'INC_DATE', 'INC_NO',\
                                  'EXP_NO','CITY', 'ZIP5']]
                                  
        # nfirs fire incident dataset with fire detector information
        nfirs_fi = dbf2DF(nfirs_fi_fp)\
                                [['STATE', 'FDID', 'INC_DATE','INC_NO',\
                                  'EXP_NO','NUM_UNIT', 'NOT_RES', 'DETECTOR',\
                                  'DET_TYPE','DET_POWER', 'DET_OPERAT',\
                                  'DET_EFFECT', 'DET_FAIL']]
        
        # nfirs basic incident dataset with a few summary fields
        nfirs_bi = dbf2DF(nfirs_bi_fp)\
                                [['STATE', 'FDID', 'INC_DATE', 'INC_NO',\
                                  'EXP_NO','INC_TYPE', 'ALARM', 'FF_DEATH',\
                                  'OTH_DEATH','DET_ALERT']]
    
    if fileType == 'txt':
        print f, fileType

        # geocoded addresses provided as part of earlier geocoding project
        geo_add = pd.read_csv(geo_add_fp, low_memory=False)\
                                 [['row_seq', 'rating', 'lon', 'lat',\
                                   'tabblock_id','county', 'tractid',\
                                   'geomout', 'output_address']]
        
        # formatted addresses from raw nfirs data, needed to join geocoded back to raw
        frmt_add = pd.read_csv(frmt_add_fp, low_memory=False)\
                                [['STATE', 'FDID', 'INC_DATE', 'INC_NO',\
                                  'EXP_NO','row_seq','input_address']]
        
        # nfirs address database needed to join back to formatted data
        nfirs_ia = pd.read_csv(nfirs_ia_fp, sep='^')\
                                [['STATE', 'FDID', 'INC_DATE', 'INC_NO',\
                                  'EXP_NO','CITY', 'ZIP5']]
                                  
        # nfirs fire incident dataset with fire detector information
        nfirs_fi = pd.read_csv(nfirs_fi_fp, sep='^',low_memory=False)\
                                [['STATE', 'FDID', 'INC_DATE','INC_NO',\
                                  'EXP_NO','NUM_UNIT', 'NOT_RES', 'DETECTOR',\
                                  'DET_TYPE','DET_POWER', 'DET_OPERAT',\
                                  'DET_EFFECT', 'DET_FAIL']]
        
        # nfirs basic incident dataset with a few summary fields
        nfirs_bi = pd.read_csv(nfirs_bi_fp, sep='^')\
                                [['STATE', 'FDID', 'INC_DATE', 'INC_NO',\
                                  'EXP_NO','INC_TYPE', 'ALARM', 'FF_DEATH',\
                                  'OTH_DEATH','DET_ALERT']]

    #joining of files
    nfirs_basic = pd.merge(nfirs_bi, nfirs_ia, how = 'left',\
                            on=['STATE', 'FDID', 'INC_DATE',\
                                'INC_NO', 'EXP_NO'] )
    nfirs_fire = pd.merge(nfirs_fi, nfirs_basic, how = 'left',\
                            on=['STATE', 'FDID', 'INC_DATE',\
                                'INC_NO', 'EXP_NO'] )
    geo_address = pd.merge(frmt_add, geo_add, how='left', on='row_seq')
    nfirs_geo = pd.merge(nfirs_fire, geo_address, how='left',\
                            on=['STATE', 'FDID', 'INC_DATE',\
                                'INC_NO', 'EXP_NO'] )
                            
    #adding couple of helpful columns
    nfirs_geo['year'] = f
    output_fp = 'output/nfirs_geo_%s.csv' % str(f)
    
    nfirs_geo.to_csv(output_fp, sep='\t')

    nfirs_all_years = nfirs_all_years.append(nfirs_geo)
    
nfirs_all_years.to_csv('output/nfirs_geo_allYears.csv', sep='\t')

 