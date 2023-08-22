#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 23 14:08:10 2023

@author: taishavenort
"""

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




#Loading AG landscapes es data 
lag_data = pd.read_csv('Datasets/es_data_agl_eplot.csv')
print(lag_data)
lag_data.columns.to_list() 
lag_list = list(lag_data) # print column names
print(lag_list)

#Loading all landscapes es data 
all_data = pd.read_csv('Datasets/es_data_all_ls_data.csv')
print(all_data)
all_data.columns.to_list() 
all_list = list(all_data) # print column names
print(all_list)


#Loading maize model es data 
maize_data = pd.read_csv('ML_GSA/datasets/ml_es_maize_data_final.csv')
print(maize_data)
maize_data.columns.to_list() 
maize_list = list(maize_data) # print column names
print(maize_list)

#Loading rice model es data 
rice_data = pd.read_csv('ML_GSA/datasets/ml_es_rice_data_final.csv')
print(rice_data)
rice_data.columns.to_list() 
rice_list = list(rice_data) # print column names
print(rice_list)


#Imputation
#Define dataset
id_vars = ['hh_refno','landscape_no','eplot_no']
id_vars2 = ['landscape_no','eplot_no']
id_ml =['hh_refno','lands_eplot', 'landscape_no', 'eplot_no']
all_id_data = all_data[id_vars2]
lag_id_data = lag_data[id_vars]
maize_id_data = maize_data[id_ml]
rice_id_data = rice_data[id_ml]

all_knn =all_data.filter(['Clay_icraf', 'pH_indicator', 'soil_fertility_indicator', 
                          'C_deficit_indicator', 'Bulk_density', 'Depth_to_bedrock'])


lag_knn=lag_data.filter(['clay_content', 'Carbon_storage', 'Soil_fertility', 
                         'N_PNB', 'P_PNB', 'K_PNB','NPK_PNB', 'Bulk_density', 
                         'Depth_to_bedrock', 'E_CEC'])


ml_maize_knn=maize_data.filter(['Maize_yield', 'phosphorous', 'sulfur', 'pH', 'Clay', 'Ca', 'Mg', 'K', 
                                'C_capacity', 'Al_exc', 'Al_sat','Mg_indicator', 'K_indicator', 'P_indicator', 
                                'S_indicator', 'Ca_indicator', 'farmproc_ph_indicator',
                                'soil_fertility_indicator', 'C_deficit_indicator', 'N_PNB', 'P_PNB', 
                                'K_PNB', 'N_added', 'P_added', 'K_added',
                                'N_PNBI', 'P_PNBI', 'K_PNBI', 'all_PNBI',
                                'Carbon_storage', 'Soil_fertility', 
                                'Water_storage', 'Nutrient_supply'])

ml_rice_knn=rice_data.filter(['Rice_yield', 'phosphorous', 'sulfur', 'pH', 'Clay', 'Ca', 'Mg', 'K', 
                              'C_capacity', 'Al_exc', 'Al_sat', 'Mg_indicator', 'K_indicator', 'P_indicator', 
                              'S_indicator', 'Ca_indicator', 'farmproc_ph_indicator', 
                              'soil_fertility_indicator', 'C_deficit_indicator', 
                              'N_PNB', 'P_PNB', 'K_PNB', 'N_added', 'P_added', 'K_added',
                              'N_PNBI', 'P_PNBI', 'K_PNBI', 'all_PNBI',
                              'Carbon_storage', 'Soil_fertility', 'Water_storage',
                              'Nutrient_supply'])

# Define KNN imputer and fill missing values

all_knn_imputer = KNNImputer(n_neighbors=19,weights='uniform', metric='nan_euclidean')
lag_knn_imputer = KNNImputer(n_neighbors=8,weights='uniform', metric='nan_euclidean')
ml_maize_knn_imputer = KNNImputer(n_neighbors=12,weights='uniform', metric='nan_euclidean')
ml_rice_knn_imputer = KNNImputer(n_neighbors=8,weights='uniform', metric='nan_euclidean')

all_knn_imputed = pd.DataFrame(all_knn_imputer.fit_transform(all_knn), columns=all_knn.columns)
lag_knn_imputed = pd.DataFrame(lag_knn_imputer.fit_transform(lag_knn), columns=lag_knn.columns)
ml_maize_knn_imputed = pd.DataFrame(ml_maize_knn_imputer.fit_transform(ml_maize_knn), columns=ml_maize_knn.columns)
ml_rice_knn_imputed = pd.DataFrame(ml_rice_knn_imputer.fit_transform(ml_rice_knn), columns=ml_rice_knn.columns)

all_knn_imputed.info()
lag_knn_imputed.info()
ml_maize_knn_imputed.info()
ml_rice_knn_imputed.info()

#Data normalization
scaler = MinMaxScaler(feature_range=(0, 1))

all_data_full =all_knn_imputed.filter(['Clay_icraf', 'pH_indicator', 'soil_fertility_indicator', 
                                       'C_deficit_indicator', 'Bulk_density', 'Depth_to_bedrock'])

lag_data_full =lag_knn_imputed.filter(['clay_content', 'Carbon_storage', 'Soil_fertility', 'N_PNB', 
                                       'P_PNB', 'K_PNB','NPK_PNB', 'Bulk_density', 'Depth_to_bedrock',
                                       'E_CEC'])

ml_maize_full =ml_maize_knn_imputed.filter(['Maize_yield', 'phosphorous', 'sulfur', 'pH', 'Clay', 'Ca', 'Mg', 'K', 
                                'C_capacity', 'Al_exc', 'Al_sat','Mg_indicator', 'K_indicator', 'P_indicator', 
                                'S_indicator', 'Ca_indicator', 'farmproc_ph_indicator',
                                'soil_fertility_indicator', 'C_deficit_indicator', 'N_PNB',
                                'P_PNB', 'K_PNB', 'N_added', 'P_added', 'K_added',
                                'N_PNBI', 'P_PNBI', 'K_PNBI', 'all_PNBI',
                                'Carbon_storage', 'Soil_fertility', 'Water_storage', 
                                'Nutrient_supply'])
ml_rice_full =ml_rice_knn_imputed.filter(['Rice_yield', 'phosphorous', 'sulfur', 'pH', 'Clay', 'Ca', 'Mg', 'K', 
                              'C_capacity', 'Al_exc', 'Al_sat', 'Mg_indicator', 'K_indicator', 'P_indicator', 
                              'S_indicator', 'Ca_indicator', 'farmproc_ph_indicator', 
                              'soil_fertility_indicator', 'C_deficit_indicator', 
                              'N_PNB', 'P_PNB', 'K_PNB', 'N_added', 'P_added', 'K_added',
                              'N_PNBI', 'P_PNBI', 'K_PNBI', 'all_PNBI',
                              'Carbon_storage', 'Soil_fertility', 'Water_storage', 
                              'Nutrient_supply'])
                                           
all_norm = pd.DataFrame(scaler.fit_transform(all_data_full), columns = all_data_full.columns)
lag_norm = pd.DataFrame(scaler.fit_transform(lag_data_full), columns = lag_data_full.columns)
ml_maize_norm = pd.DataFrame(scaler.fit_transform(ml_maize_full), columns = ml_maize_full.columns)
ml_rice_norm = pd.DataFrame(scaler.fit_transform(ml_rice_full), columns = ml_rice_full.columns)
#concatenate with id data

all_data_norm =pd.concat([all_id_data,all_norm],axis=1)
lag_data_full =pd.concat([lag_id_data,lag_knn_imputed],axis=1)
lag_data_norm =pd.concat([lag_id_data,lag_norm],axis=1)
ml_maize_full =pd.concat([maize_id_data,ml_maize_knn_imputed],axis=1)
ml_maize_norm =pd.concat([maize_id_data,ml_maize_norm],axis=1)
ml_rice_full =pd.concat([rice_id_data,ml_rice_knn_imputed],axis=1)
ml_rice_norm =pd.concat([rice_id_data,ml_rice_norm],axis=1)
#Save
all_data_full.to_csv('Datasets/es_data_all_ls_knnimp.csv', index=False)
all_data_norm.to_csv('Datasets/es_data_all_ls_knnimp_norm.csv', index=False)
lag_data_full.to_csv('Datasets/es_data_agl_knnimp.csv', index=False)
lag_data_norm.to_csv('Datasets/es_data_agl_knnimp_norm.csv', index=False)
ml_maize_full.to_csv('ML_GSA/datasets/ml_es_maize_knnimp.csv', index=False)
ml_rice_full.to_csv('ML_GSA/datasets/ml_es_rice_knnimp.csv', index=False)


