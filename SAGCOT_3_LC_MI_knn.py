#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 29 16:55:11 2023

@author: taishavenort
"""

#sources
#https://towardsdatascience.com/imputing-missing-data-with-simple-and-advanced-techniques-f5c7b157fb87
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.impute import KNNImputer 
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import RobustScaler
from sklearn.preprocessing import FunctionTransformer
from sklearn.compose import ColumnTransformer

#loading LCI data
lci_data = pd.read_csv('Datasets/HH_CA.csv')
print(lci_data)
lci_data.columns.to_list() 
lci_list = list(lci_data) # print column names
print(lci_list)

#Loading AIS data
ais_data = pd.read_csv('Datasets/HH_AIS.csv')
print(ais_data)
ais_data.columns.to_list() 
ais_list = list(ais_data) # print column names
print(ais_list)


#Merging all datasets
data = pd.concat([lci_data,ais_data],axis=1)
print(data)
data = data.loc[:,~data.columns.duplicated()].copy()
data = data.iloc[0:210,]

data.columns.to_list() 
data_list = list(data) # print column names
print(data_list)


#Summary statistcis of selected variables
# types = pd.DataFrame(data.dtypes.to_list(), index=data.columns, columns=['dtype'])
# stats = data.describe().T
# stats = pd.merge(stats, types, how='outer', left_index=True, right_index=True)
# stats.to_csv('Outputs/Stats/stats.csv')

#Imputation
#Define dataset
id_vars = ['hh_refno','landscape_no', 'eplot_no']
id_data = data[id_vars]

lci_knn =data.filter(['ave_precip', 'dist_water_bodies', 'Shannon_tree', 'richness_tree',
                      'Slope','farmproc_clay_icraf', 'Elevation', 'dist_forest', 'Male_Female_ratio', 'hhmembers', 
                      'hh_dep_ratio', 'literacy_rate', 'leveledu', 'hh_labor_days', 'Info_network',
                      'market_network', 'ag_implements', 'water_inf', 'farm_structures', 'comm_inf',
                      'dist_cell_wifi_tower', 'transp_inf', 'dist_main_roads', 'dist_any_roads', 
                      'time_field_travel', 'wage_entry_n', 'liv_sale_number', 'house_tenure_n', 
                      'house_walls_n', 'house_roof_n', 'house_floor_n', 'house_san_n', 
                      'house_electricity_n', 'house_water_n', 'house_fuel_cook_n', 'house_fuel_light_n', 
                      'hh_microf_n', 'landsize', 'farm_type', 'fields'])

ais_knn =data.filter(['fertuse_n', 'improvedseeds_n', 'mechtools_n', 'irr_n',
                      'pestuse_n', 'CIF'])

# for loop for histograms
for var in lci_knn.columns:
    plt.figure(var)  # make a different plot
    plt.title(var)  # title
    plt.hist(data[var])  # make a histogram
    plt.savefig('histograms/' + var + '_hist.png')  # save the figure


# Define KNN imputer and fill missing values

lci_knn_imputer = KNNImputer(n_neighbors=14,weights='uniform', metric='nan_euclidean')
ais_knn_imputer = KNNImputer(n_neighbors=14,weights='uniform', metric='nan_euclidean')
lci_knn_imputed = pd.DataFrame(lci_knn_imputer.fit_transform(lci_knn), columns=lci_knn.columns)
ais_knn_imputed = pd.DataFrame(ais_knn_imputer.fit_transform(ais_knn), columns=ais_knn.columns)
ais_knn_imputed = ais_knn_imputed.round()

lci_knn_imputed.info()
ais_knn_imputed.info()

#Master dataset
data_final = pd.concat([id_data,lci_knn_imputed,ais_knn_imputed],axis=1)
data_final.to_csv('Datasets/HH_masterdata_knnimp.csv', index=False)

print(data_final)
data_final.columns.to_list() 
my_list = list(data_final) # print column names
print(my_list)

#Master dataset normalized
# Data normalization
scaler = MinMaxScaler(feature_range=(0, 1))
lci_data= ['ave_precip', 'dist_water_bodies', 'Shannon_tree', 'richness_tree',
                      'Slope', 'farmproc_clay_icraf','Elevation', 'dist_forest', 'Male_Female_ratio', 'hhmembers', 
                      'hh_dep_ratio', 'literacy_rate', 'leveledu', 'hh_labor_days', 'Info_network',
                      'market_network', 'ag_implements', 'water_inf', 'farm_structures', 'comm_inf',
                      'dist_cell_wifi_tower', 'transp_inf', 'dist_main_roads', 'dist_any_roads', 
                      'time_field_travel', 'wage_entry_n', 'liv_sale_number', 'house_tenure_n', 
                      'house_walls_n', 'house_roof_n', 'house_floor_n', 'house_san_n', 
                      'house_electricity_n', 'house_water_n', 'house_fuel_cook_n', 'house_fuel_light_n', 
                      'hh_microf_n', 'landsize', 'farm_type', 'fields']

lci_data= data_final[lci_data]
lci_norm = pd.DataFrame(scaler.fit_transform(lci_data), columns = lci_data.columns)
others_data=['fertuse_n', 'improvedseeds_n', 'mechtools_n', 'irr_n', 'pestuse_n', 'CIF' ]
others_nonnorm= data_final[others_data]
data_norm =pd.concat([id_data,others_nonnorm,lci_norm],axis=1)
print(data_norm)
data_norm.to_csv('Datasets/HH_masterdata_knnimp_norm.csv', index=False)


# #Data distribution
# # for loop for histograms
# for var in data.columns:
#      plt.figure(var)  # make a different plot
#      plt.title(var)  # title
#      plt.hist(data[var])  # make a histogram
#      plt.savefig('Outputs/Graphs/histograms/' + var + '_hist.png')  # save the figure
    
# #for loop for pairplots
# sns.pairplot(data)  # make a pairplot
# plt.savefig('Outputs/Graphs/histograms/pairplot.png')
# my_list2 = list(data) # print column names
# print(my_list2)
