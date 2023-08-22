###ES indicators data preparation###
#---Part I: All landscapes eplot level ecosystem services and natural capital indicators-----------
#--Setups--
getwd()
rm(list=ls(all=TRUE))  #Remove objects in memory
#Open libraries
#install.packages("tidyverse")
library(dplyr)
library(lubridate)
library(RColorBrewer)
# set directories
output_directory <- "./6_Outputs"
#spatial_directory <- "./3_Data_shapefiles_rasters" # Folder with shapefiles and rasters
rawdata_directory <- "./2_Data_VS_raw"
#add_data_directory <- "./4_Data_additional"
#Loading VS merging file **confidential
#VS_MergingFile<-read.csv("/Users/tvenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-read.csv("/Users/taishavenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-VS_MergingFile[c(3,2,4,5)]
VS_MergingFile$lands_eplot<-paste(VS_MergingFile$landscape_no,"_",VS_MergingFile$eplot_no)
VS_MergingFile
#reMOving row with eplot NA & 55 & 0
VS_MergingFile<-filter(VS_MergingFile, eplot_no != 55 & eplot_no != "NA" & eplot_no != 0 )
VS_MergingFile
#Dropping rows of 5 inexistant household 
VS_MergingFile<-VS_MergingFile[!(VS_MergingFile$hh_refno =="TZA-L10-H25" | VS_MergingFile$hh_refno =="TZA-L10-H09"| 
                                   VS_MergingFile$hh_refno =="TZA-L18-H29"| VS_MergingFile$hh_refno=="TZA-L19-H11"| VS_MergingFile$hh_refno  =="TZA-L19-H19"),]
colnames(VS_MergingFile)
dim(VS_MergingFile)

#---ES indicators------
#soil health indicators: fertility and carbon deficit 
soil<-read.csv(paste(output_directory,"/Soil_health/eplot_soil_info_indicators.csv", sep=""))
colnames(soil)
soil
soil_health<-soil[c("landscape_no","eplot_no","Clay_icraf","pH_indicator","soil_fertility_indicator",
                    "C_deficit_indicator")]
View(soil_health)
oldvals <- c('0','L03','L10','L11','L18','L19','L20','L22')
newvals <- c('L0','L03','L10','L11','L18','L19','L20','L22')
soil_health$landscape_no<-newvals[match(soil_health$landscape_no, oldvals)]
#soil properties from isda maps
soil_prop<-read.csv(paste("./8_Analyses/Datasets/HH_isDA_Soil_properties.csv", sep=""))
colnames(soil_prop)
soil_prop<-soil_prop[c("landscape_no","eplot_no","Bulk_density","Depth_to_bedrock")]
library('dplyr')
dim(soil_prop)
es_al<-soil_health%>%left_join(soil_prop,by=c("landscape_no","eplot_no"))
colnames(es_al)
dim(es_al)
#mising data
as.data.frame(colSums(is.na(es_al[,-c(1:2)])))
#Categorizing the landscapes
oldvals <- c('0','L03','L10','L11','L18','L19','L20','L22')
newvals <- c('Semi_natural','Maize_producing','Maize_producing','Maize_producing','Maize_producing',
             'Rice_producing','Rice_producing','Rice_producing')
es_al$LT<-newvals[match(es_al$landscape_no, oldvals)]
colnames(es_al)
dim(es_al)
as.data.frame(colSums(is.na(es_al[,-c(1:2)])))
write.csv(es_al,'./8_Analyses/Datasets/es_data_all_ls_data.csv',row.names = FALSE)

#---Part 2: Farm level ES indicators for agricultural landscapes only-----------
#standardizing functions
lib<-function(x){
  y<-1-((x-min(x))/(max(x)-min(x)))
  return(y)
}
mib<-function(x){
  y<-(x-min(x))/(max(x)-min(x))
  return(y)
}

opt_pH<-function(x){
  y<-1-((abs(x-7) - abs(min(x)-7))/(abs(max(x)-7) -abs(min(x)-7)))
  return(y)
}
opt_bd<-function(x){
  y<-1-((abs(x-130) - abs(130-min(x)))/(130-abs(max(x)) -abs(130-min(x))))
  return(y)
}
#soil health indicators 
soil<-read.csv(paste(output_directory,"/Soil_health/field_soil_info_indicators_eplot.csv", sep=""))
colnames(soil)
#means
mean(soil$farmproc_ph)
mean(soil$threshold_Al_saturation)
mean(soil$m3_Ca_cmolkg)
mean(soil$m3_Mg_cmolkg)
mean(soil$m3_K_cmolkg)
mean(soil$farmproc_m3_p)
mean(soil$farmproc_m3_s)
mean(soil$farmproc_acidified_carbon)
mean(soil$farmproc_clay_icraf)
mean(soil$farmproc_silt_icraf)
#sd
sd(soil$farmproc_ph)
sd(soil$threshold_Al_saturation)
sd(soil$m3_Ca_cmolkg)
sd(soil$m3_Mg_cmolkg)
sd(soil$m3_K_cmolkg)
sd(soil$farmproc_m3_p)
sd(soil$farmproc_m3_s)
sd(soil$farmproc_acidified_carbon)
sd(soil$farmproc_clay_icraf)
sd(soil$farmproc_silt_icraf)

#soil health
soil_health<-soil[c("landscape_no","eplot_no","farmproc_clay_icraf","C_deficit_indicator",
                    "soil_fertility_indicator")]
soil_health
#soil properties from isda maps
soil_prop<-read.csv(paste("./8_Analyses/Datasets/HH_isDA_Soil_properties.csv", sep=""))
colnames(soil_prop)
soil_prop<-soil_prop[c("landscape_no","eplot_no","Bulk_density","Depth_to_bedrock","E_CEC")]
library('dplyr')
soil_prop<-soil_prop%>%filter(landscape_no == 'L03' | landscape_no == 'L10'| landscape_no == 'L11'|
                  landscape_no == 'L18' |landscape_no == 'L19'| landscape_no == 'L20'|
                  landscape_no == 'L22')
soil_prop
as.data.frame(colSums(is.na(soil_prop[,-c(1:2)])))
dim(soil_prop)
soil_prop
#mean
mean(soil_prop$Bulk_density)
mean(soil_prop$Depth_to_bedrock)
mean(soil_prop$E_CEC,2)
#sd
sd(soil_prop$Bulk_density)
sd(soil_prop$Depth_to_bedrock)
sd(soil_prop$E_CEC,2)

#soil partial nutrient budget
PNB_data<-read.csv(paste(output_directory,"/Soil_nutrient_budget/PartNutrBudget_eplot.csv", sep=""))
colnames(PNB_data)
mean(PNB_data$NutrBudgNkgha_mean)
sd(PNB_data$NutrBudgNkgha_mean)
mean(PNB_data$NutrBudgPkgha_mean)
sd(PNB_data$NutrBudgPkgha_mean)
mean(PNB_data$NutrBudgKkgha_mean)
sd(PNB_data$NutrBudgKkgha_mean)
PNB_data<-PNB_data[c("landscape_no","eplot_no", "NutrPartialIndicatorN_mean","NutrPartialIndicatorP_mean",
                     "NutrPartialIndicatorK_mean","SH58_mean")]
PNB_data
mean(PNB_data$NutrPartialIndicatorN_mean)
# #Yield data
# #loading yields data
# yield_data<-read.csv(paste(output_directory,"/Yield/yield_data_eplot.csv", sep=""))
# colnames(yield_data)
# yield_data
# #focusing on Maize yield
# library("dplyr")
# #grain<- c("Maize","Paddy","Finger Millet","Wheat","Sorghum", "Bulrush Millet")
# yield_m<-filter(yield_data, crop_name == 'Maize')
# names(yield_m)[6]<-'Maize_yield'
# yield_m<-yield_m[c("landscape_no", "eplot_no","Maize_yield")]
# yield_m<-filter_if(yield_m, is.numeric, all_vars((.) != 0))
# yield_m
# #mean & std dev of yield
# mean(yield_m$Maize_yield)
# sd(yield_m$Maize_yield)
# #focusing on paddy
# library("dplyr")
# paddy<- c("Paddy")
# yield_r<-filter(yield_data, crop_name %in% paddy)
# names(yield_r)[6]<-'Rice_yield'
# yield_r<-yield_r[c("landscape_no", "eplot_no","Rice_yield")]
# yield_r
# #mean & std dev of yield
# mean(yield_r$Rice_yield)
# sd(yield_r$Rice_yield)
# #total cereal production
# cereal<-yield_m%>%left_join(yield_r,by=c('landscape_no', 'eplot_no'))
# cereal<-cereal%>%replace(is.na(.), 0)
# cereal$Cereal_production<-rowMeans(cereal[c("Maize_yield", "Rice_yield")])
# #animal production
# hh_livestock<-read.csv(paste(rawdata_directory,"/household_livestock/household_livestock_TZA.csv", sep=""))
# colnames(hh_livestock)
# liv_hold<-hh_livestock[c(4,5,10,11,12,13,14,15)]
# liv_hold<-liv_hold[!duplicated(liv_hold$hh_refno), ]
# liv_hold
# names(liv_hold)[4] <- 'indigeneous'
# names(liv_hold)[5] <- 'improved'
# names(liv_hold)[6] <- 'improvedD'
# names(liv_hold)[7] <- 'sale'
# names(liv_hold)[8] <- 'liv_sale_number'
# liv_hold
# liv_hold$indigeneous<-as.numeric(liv_hold$indigeneous)
# liv_hold$improved<-as.numeric(liv_hold$improved)
# liv_hold$improvedD<-as.numeric(liv_hold$improvedD)
# liv_hold<-liv_hold%>% replace(is.na(.), 0)
# liv_hold
# liv_hold$totalliv<-rowSums(liv_hold[,c(4,5,6)])
# liv_hold$liv_hold_sale<- as.integer(as.logical(liv_hold$sale))
# liv_hold<-liv_hold[c(1,2,9)]
# liv_hold
# library(dplyr)
# liv_hold_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
# liv_hold_hh
# liv_hold_hh<- left_join(liv_hold_hh, liv_hold, by='hh_refno')
# liv_hold_hh
# liv_hold_l<-liv_hold_hh[c(2,3,7)]
# names(liv_hold_l)[1]<-'landscape_no'
# names(liv_hold_l)[2]<-'eplot_no'
# names(liv_hold_l)[3]<-'Animal_production'
# liv_hold_l
#liv_hold_hh<-liv_hold_hh[c(4,7)]
#liv_hold_hh

#joining ecosystem services datasets
es_ag<-soil_health%>%left_join(PNB_data,by=c('landscape_no'='landscape_no',
        'eplot_no'='eplot_no'))%>%left_join(soil_prop,by=c('landscape_no'='landscape_no',
         'eplot_no'='eplot_no'))
colnames(es_ag)
es_ag<-distinct(es_ag, es_ag$landscape_no,es_ag$eplot_no, .keep_all= TRUE)
colnames(es_ag)
#mising data
as.data.frame(colSums(is.na(es_ag[,-c(1:2)])))
dim(es_ag)
names(es_ag)[3]<-'clay_content'
names(es_ag)[4]<-'Carbon_storage'
names(es_ag)[5]<-'Soil_fertility'
names(es_ag)[6]<-'N_PNB'
names(es_ag)[7]<-'P_PNB'
names(es_ag)[8]<-'K_PNB'
names(es_ag)[9]<-'NPK_PNB'
colnames(es_ag)
dim(es_ag)
es_ag%>%
  group_by(landscape_no) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))
es_ag<-es_ag[c("landscape_no", "eplot_no","clay_content","Carbon_storage",
               "Soil_fertility","N_PNB","P_PNB","K_PNB","NPK_PNB",
               "Bulk_density","Depth_to_bedrock", 
               "E_CEC")]
colnames(es_ag)
#putting es_ag at the hh level ----
#loading es data
#New TOP hh dataframe
es_hh<- VS_MergingFile%>%group_by(hh_refno)
es_hh
library(dplyr)
es<- full_join(es_ag, es_hh, by =c('landscape_no','eplot_no'))
dim(es)
colnames(es)
es<-es[,-c(13,15)]
colnames(es)
as.data.frame(colSums(is.na(es[,-c(1)])))
write.csv(es,'./8_Analyses/Datasets/es_data_agl_eplot.csv',row.names = FALSE)

#laoding imputed data to calculate final indicators
es_ag_imp<-read.csv(paste("./8_Analyses/Datasets/es_data_agl_knnimp.csv", sep=""))
colnames(es_ag_imp)
dim(es_ag_imp)
as.data.frame(colSums(is.na(es_ag_imp[,-c(1:3)])))
#es_ag_imp<-na.omit(es_ag_imp)
dim(es_ag_imp)
es_ag_imp
colnames(es_ag_imp)
#View(es_ag_imp)
#Soil water storage
es_ag_imp$clay_content<-mib(es_ag_imp$clay_content)
es_ag_imp$Bulk_density<-opt_bd(es_ag_imp$Bulk_density)
es_ag_imp$Depth_to_bedrock<-mib(es_ag_imp$Depth_to_bedrock)
es_ag_imp$Water_storage<-rowMeans(es_ag_imp[c("clay_content","Bulk_density","Depth_to_bedrock")])
es_ag_imp$NPK_PNB<-lib(es_ag_imp$NPK_PNB)
# #Soil nutrient suply
# es_ag_imp$N_PNB<-mib(es_ag_imp$N_PNB)
# es_ag_imp$P_PNB<-mib(es_ag_imp$P_PNB)
# es_ag_imp$K_PNB<-mib(es_ag_imp$K_PNB)
# es_ag_imp$E_CEC<-mib(es_ag_imp$E_CEC)
names(es_ag_imp)[10]<-'Nutrient_supply'
colnames(es_ag_imp)
# es_ag_imp$Nutrient_supply<-rowMeans(es_ag_imp[c("N_PNB","E_CEC")])
# colnames(es_ag_imp)
es_ag_imp<-es_ag_imp[c("hh_refno","landscape_no","eplot_no","Carbon_storage", 
                         "Water_storage","Nutrient_supply")]
dim(es_ag_imp)
es_ag_imp$lands_eplot<-paste(es_ag_imp$landscape_no,"_",es_ag_imp$eplot_no)
es_ag_imp<-es_ag_imp[c("hh_refno","lands_eplot","landscape_no","eplot_no","Carbon_storage", 
                      "Water_storage","Nutrient_supply")]
dim(es_ag_imp)
write.csv(es_ag_imp,'./8_Analyses/Datasets/es_data_agl_final.csv',row.names = FALSE)
View(es_ag_imp)
dim(es_ag_imp)

#---Part 3: ML household level ES indicators for ML model ----------------------
#Yield data
#loading yields data
yield_data<-read.csv(paste(output_directory,"/Yield/yield_data_hh.csv", sep=""))
colnames(yield_data)
#focusing on Maize yield
library("dplyr")
#grain<- c("Maize","Paddy","Finger Millet","Wheat","Sorghum", "Bulrush Millet")
yield_m<-filter(yield_data, crop_name == 'Maize')
names(yield_m)[6]<-'Maize_yield'
yield_m<-yield_m[c("hh_refno","Maize_yield")]
yield_m
#focusing on paddy
yield_r<-filter(yield_data, crop_name =='Paddy')
names(yield_r)[6]<-'Rice_yield'
yield_r<-yield_r[c("hh_refno","Rice_yield")]
yield_r
#soil health indicators: fertility and carbon deficit 
soil<-read.csv(paste(output_directory,"/Soil_health/field_soil_info_indicators_CONF.csv", sep=""))
colnames(soil)
#View(soil)
soil_health<-soil[c( "hh_refno","farmproc_m3_p","farmproc_m3_s","farmproc_ph",
                     "farmproc_clay_icraf", "m3_Ca_cmolkg","m3_Mg_cmolkg",             
                     "m3_K_cmolkg","C_capacity","farmproc_exch_al","threshold_Al_saturation",
                     "Mg_indicator","K_indicator",
                     "P_indicator","S_indicator","Ca_indicator","farmproc_ph_indicator",
                     "soil_fertility_indicator","C_deficit_indicator" )]
names(soil_health)[2]<-'phosphorous'
names(soil_health)[3]<- 'sulfur'
names(soil_health)[4]<-'pH'
names(soil_health)[5]<-'Clay'
names(soil_health)[6]<-'Ca'
names(soil_health)[7]<-'Mg'
names(soil_health)[8]<-'K'
names(soil_health)[9]<-'C_capacity'
names(soil_health)[10]<-'Al_exc'
names(soil_health)[11]<-'Al_sat'

colnames(soil_health)
#soil partial nutrient budget
PNB_data<-read.csv(paste(output_directory,"/Soil_nutrient_budget/PartNutrBudget_hh.csv", sep=""))
colnames(PNB_data)
PNB_data<-PNB_data[c("hh_refno","NutrBudgNkgha_mean","NutrBudgPkgha_mean","NutrBudgKkgha_mean",
                     "NaddedkghaAllSrcs_mean","PaddedkghaAllSrcs_mean","KaddedkghaAllSrcs_mean",
                     "NutrPartialIndicatorN_mean", "NutrPartialIndicatorP_mean","NutrPartialIndicatorK_mean",
                     "SH58_mean")]
colnames(PNB_data)
#View(PNB_data)
#PNB_data$Soil_nutrient_balance<-PNB_data$SH58_mean/3
names(PNB_data)[2]<-'N_PNB'
names(PNB_data)[3]<-'P_PNB'
names(PNB_data)[4]<-'K_PNB'
names(PNB_data)[5]<-'N_added'
names(PNB_data)[6]<-'P_added'
names(PNB_data)[7]<-'K_added'
names(PNB_data)[8]<-'N_PNBI'
names(PNB_data)[9]<-'P_PNBI'
names(PNB_data)[10]<-'K_PNBI'
names(PNB_data)[11]<-'all_PNBI'
colnames(PNB_data)
#maize dataset
es_hh_my<-yield_m%>%left_join(soil_health,by='hh_refno')%>%left_join(PNB_data,
              by='hh_refno')%>%left_join(es_ag_imp, by='hh_refno')
es_hh_my<-es_hh_my[!duplicated(es_hh_my$hh_refno),]
dim(es_hh_my)
colnames(es_hh_my)
write.csv(es_hh_my,'./8_Analyses/ML_GSA/datasets/ml_es_maize_data_final.csv',row.names = FALSE)
#write.csv(es_hh_my,'./8_Analyses/Datasets/ml_es_maize_data_final_hh.csv',row.names = FALSE)
#rice dataset
es_hh_ry<-yield_r%>%left_join(soil_health,by='hh_refno')%>%left_join(PNB_data,by='hh_refno')%>%
  left_join(es_ag_imp, by='hh_refno')
es_hh_ry<-es_hh_ry[!duplicated(es_hh_ry$hh_refno),]
dim(es_hh_ry)
write.csv(es_hh_ry,'./8_Analyses/ML_GSA/datasets/ml_es_rice_data_final.csv',row.names = FALSE)
#write.csv(es_hh_ry,'./8_Analyses/Datasets/ml_es_rice_data_final_hh.csv',row.names = FALSE)
