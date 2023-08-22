#---setups-----
#standardizing functions
lib<-function(x){
  y<-1-((x-min(x))/(max(x)-min(x)))
  return(y)
}
mib<-function(x){
  y<-(x-min(x))/(max(x)-min(x))
  return(y)
}
#Min-max Normalization
norm<- function(x){
  (x- min(x)) /(max(x)-min(x))}

#geometric mean function
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
#---Spearman rank correlations for all possible pairs of ES and NC in each landscape type ------
#install.packages("ggcorrplot")
#we want the normalized data maybe (raw)
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
#library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(tidyverse)
cor_data<-read.csv(paste('./Data/HH_es_data_KE.csv', sep=""))
dim(cor_data)
colnames(cor_data)
cor_data<-cor_data[c("HH_ID_Number","Nutrient_supply",
                     "Soil_water_storage","C_deficit_indicator","Maize_yield",
                     "pot_yield")]
#cluster data
cluster<-read.csv(paste('./Data/som_ES_data.csv', sep=""))
colnames(cluster)
names(cluster)[5]<-'ESC'
cluster<-cluster[c(1,5)]
cluster
oldvals <- c(1,2,3)
newvals <- c('ESC1','ESC2','ESC3')
cluster$ESC<-newvals[match(cluster$ESC, oldvals)]
#Calling LC data
lc<-read.csv(paste('./Data/HH_indeces.csv',sep=""))
colnames(lc)
lc<-lc[c("HH_ID_Number","nc_si","hdc_si")]
lc
#Calling mi data
mi_data<-read.csv("./Data/HH_AII_KE.csv")
colnames(mi_data)
mi_data<-mi_data[c("HH_ID_Number","CIF","CPF")]
mi_data$CIF<-norm(mi_data$CIF)
mi_data$CPF<-norm(mi_data$CPF)
#merging data
library(dplyr)
cor_data<-cor_data%>%left_join(cluster,by='HH_ID_Number')%>%left_join(lc, by='HH_ID_Number')%>%left_join(mi_data,by='HH_ID_Number')
dim(cor_data)
colnames(cor_data)
cor_data$ESC<-as.factor(cor_data$ESC)
names(cor_data)[2]<-'NS'
names(cor_data)[3]<-'WS'
names(cor_data)[4]<-'CS'
names(cor_data)[5]<-'MP'
names(cor_data)[6]<-'PP'
names(cor_data)[8]<-'NC'
names(cor_data)[9]<-'HDC'
names(cor_data)[10]<-'CII'
names(cor_data)[11]<-'SMI'
cor_data<-cor_data[c(1,7,2:6,8:11)]
colnames(cor_data)
cor_data<-cor_data[c(1,2,5,4,6,7,3,8,9,10,11)]
cor_data
#--PLOT 1 ES---
library(ggplot2)  
library(GGally)
# cor_data_es<-cor_data[,c(1:7)]
# colnames(cor_data_es)
# ggpairs(cor_data, columns = 3:7,
#         aes(color = cor_data_es$ESC),                             # Separate data by levels of vs
#         upper = list((method = "spearman"),
#                      continuous = wrap('cor', size = 3)),
#         lower = list(combo = wrap("facethist", bins = 30, size = 5)),
#         diag = list(continuous = wrap("densityDiag",alpha = 0.5)))+
#   theme(text = element_text(size=12))
#plot 2 --- Realized yield & HDC + NC---
ggpairs(cor_data, columns = 3:11,
        #aes(color = cor_data$ESC),                             # Separate data by levels of vs
        upper = list((method = "spearman"),
                     continuous = wrap('cor', size = 3)),
        lower = list(combo = wrap("facethist", bins = 30, size = 5)),
        diag = list(continuous = wrap("densityDiag",alpha = 0.5)))+
  theme(text = element_text(size=12))
