
#SIIL - AII-AIS-LII##
getwd()
rm(list=ls(all=TRUE))  #Remove objects in memory
#Open libraries
library(tidyverse)
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
#reomving row with eplot NA & 55 & 0
VS_MergingFile<-filter(VS_MergingFile, eplot_no != 55 & eplot_no != "NA" & eplot_no != 0 )
View(VS_MergingFile)

#--Data-------------------------------------------------------------------------
#Loading the raw household data
hh_field<-read.csv(paste(rawdata_directory,"/household_field/household_field_Jan2018_TZA_nodupl.csv", sep=""))
hh_field_season<-read.csv(paste(rawdata_directory,"/household_field_season/household_field_season_TZA.csv", sep=""))
hh_field_permcrop<-read.csv(paste(rawdata_directory,"/household_field_permcrop/household_field_permcrop_TZA.csv", sep=""))
hh_field_season_fieldcrop<-read.csv(paste(rawdata_directory,"/household_field_season_fieldcrop/household_field_season_fieldcrop_TZA.csv", sep=""))
hh_fieldcrop<-read.csv(paste(rawdata_directory,"/household_fieldcrop/household_fieldcrop_TZA.csv", sep=""))
#Folders I added in github raw data repository from VS-Agricultural Intensity & Management survey
hh_field_season_individual<-read.csv(paste(rawdata_directory,"/household_field_season_individual/household_field_season_individual_TZA.csv", sep=""))
hh_implement<-read.csv(paste(rawdata_directory,"/household_implements/household_implements_TZA.csv", sep=""))
hh_individual<-read.csv(paste(rawdata_directory,"/household_individual/household_individual_TZA.csv", sep=""))
#hh_resource<-read.csv(paste(rawdata_directory,"/household_resource/household_resource_TZA.csv",sep=""))
hh_livestock<-read.csv(paste(rawdata_directory,"/household_livestock/household_livestock_TZA.csv", sep=""))
hh_priceinfo<-read.csv(paste(rawdata_directory,"/household_priceinfo/household_priceinfo_TZA.csv", sep=""))
#Folders I added in raw data repository from VS-household assets survey
hh_water<-read.csv(paste(rawdata_directory,"/household/hh_secJ2_TZA_0.csv", sep=""))
hh_assets<-read.csv(paste(rawdata_directory,"/household/hh_secN_TZA_0.csv", sep=""))
colnames(hh_water)
#Loading VS merging file **confidential
#VS_MergingFile<-read.csv("/Users/tvenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-read.csv("/Users/taishavenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-VS_MergingFile[c(3,2,4,5)]
VS_MergingFile$lands_eplot<-paste(VS_MergingFile$landscape_no,"_",VS_MergingFile$eplot_no)
VS_MergingFile
#reomving row with eplot NA & 55 & 0
VS_MergingFile<-filter(VS_MergingFile, eplot_no != "55" & eplot_no != "NA" & eplot_no != "0" )
VS_MergingFile


#--CIF-----
#Irrigation
colnames(hh_field_season)
#ag3a_17-Was this FIELD irrigated?{1: 'Yes', 2: 'No'}
irr<-hh_field_season[c(5,7,20)]
head(irr)
#making variable names explicit
names(irr)[3]<- 'irr_field'
head(irr)
#recoding categorical to numeric
library(dplyr)
oldvals <- c("FALSE","TRUE", na.omit=TRUE)
newvals <- c(0,1)
irr$irr_n<-newvals[match(irr$irr_field, oldvals)]
irr$irr_n<-as.numeric(irr$irr_n)
irr<-aggregate(irr_n~ hh_refno,irr,max)
irr

#Mechanization
colnames(hh_implement)
#ag11_01--Number owned
#ag11_07--Number borrowed
mechtools<-hh_implement[c(5,7,8,10,11)]
head(mechtools)
#making variable names explicit
names(mechtools)[5] <- 'n_tools'
head(mechtools)
#counts of tools
library(dplyr)
mechtools<-mechtools%>% count(toolname, hh_refno)
mechtools
#recoding 
library(dplyr)
oldvals <- c('"soso" (used to dig holes/pluck plantains/harvest yams)','axe','cutlass','farm buildings',
             'geri cans/drums','hand-powered sprayer','hand hoe','hand mill/grinder','ox plough',
             'ox seed planter','sheds','sheller/thresher','sickle (for cocoa harvesting)','silos','storeroom',
             'tractor','tractor harrow','tractor plough','watering can')
newvals <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0)
mechtools$mechtools_n<-newvals[match(mechtools$toolname, oldvals)]
mechtools<-aggregate(mechtools_n~ hh_refno,mechtools,max)
mechtools$mechtools_n<-as.numeric(mechtools$mechtools_n)
mechtools$mechtools_n<-as.numeric(mechtools$mechtools_n)
mechtools

# #Fertilizer use
colnames(hh_field_season)
fert_use<-hh_field_season[c(5,7,41)]
#making variable names explicit
names(fert_use)[3]<- 'fertuse'
#counts of fertilized fields
library(dplyr)
fert_use<-fert_use%>% count(fertuse, hh_refno)
fert_use<-fert_use[c(2,1,3)]
head(fert_use)
#making variable names explicit
names(fert_use)[2]<- 'fertuse'
fert_use
#recoding categorical to numeric
library(dplyr)
oldvals <- c('FALSE','TRUE',na.omit(fert_use))
newvals <- c(0,1)
fert_use$fertuse_n<-newvals[match(fert_use$fertuse, oldvals)]
fert_use<-fert_use[c(1,4)]
fert_use<-na.omit(fert_use)
fert_use$fertuse_n<-as.numeric(fert_use$fertuse_n)
fert_use

#Pesticide use
colnames(hh_field_season)
head(hh_field_season)
#ag3a_17-Was this FIELD irrigated?{1: 'Yes', 2: 'No'}
pest_use<-hh_field_season[c(5,7,53)]
head(pest_use)
#making variable names explicit
names(pest_use)[3]<- 'pestuse'
head(pest_use)
#counts of fields on which pesticides are used
library(dplyr)
pest_use<-pest_use%>% count(pestuse, hh_refno)
pest_use<-pest_use[c(2,1,3)]
head(pest_use)
#making variable names explicit
names(pest_use)[3]<- 'pestuse'
pest_use<-pest_use[c(1,2)]
pest_use
#recoding categorical to numeric
library(dplyr)
oldvals <- c('FALSE','TRUE')
newvals <- c(0,1)
pest_use$pestuse_n<-newvals[match(pest_use$pestuse, oldvals)]
pest_use<-na.omit(pest_use)
pest_use<-pest_use[c(1,3)]
pest_use$pestuse_n<-as.numeric(pest_use$pestuse_n)
pest_use

#Improved seeds
#ag4a_23	What type of seed did you purchase ?
#{1: 'Traditional', 2: 'Purchased Improved Seeds', 3: 'Saved Improved Seeds'}
colnames(hh_field_season_fieldcrop)
certseeds<-hh_field_season_fieldcrop[c(5,7,26)]
certseeds
#making variable names explicit
names(certseeds)[3]<- 'improvedseeds'
head(certseeds)
#removing duplicates
certseeds<- certseeds[!duplicated(certseeds$hh_refno), ]
#recoding categorical to numeric
library(dplyr)
oldvals <- c( "Purchased Improved Seeds", "Traditional", "<NA>")
newvals <- c(1,0,0)
certseeds$improvedseeds_n<-newvals[match(certseeds$improvedseeds,oldvals)]
certseeds$improvedseeds_n<-as.numeric(certseeds$improvedseeds_n)
certseeds[is.na(certseeds)]<-0
certseeds<-certseeds[c(1,4)]
certseeds

#Merging of Inputs variables to be considered in ranking
Inputs<-fert_use%>%left_join(certseeds,by='hh_refno')%>%left_join(mechtools, 
          by='hh_refno')%>%left_join(irr,
        by='hh_refno')%>%left_join(pest_use,by='hh_refno')
Inputs<-Inputs[!duplicated(Inputs$hh_refno),]
Inputs[is.na(Inputs)]<-0
colnames(Inputs)
Inputs
dim(Inputs)

#Index with qualitative data
library(dplyr)
CIF<-Inputs%>%group_by(fertuse_n,improvedseeds_n,mechtools_n,irr_n,pestuse_n)%>%
  mutate(CIF = cur_group_id()) %>%ungroup() %>%arrange(CIF)
CIF
View(CIF)
# #--CI
#Get hh crops and cultivated are data
yield_data<- read.csv(paste(output_directory,"/Yield/yield_data_hh.csv", sep=""))
colnames(yield_data)
yield_data
#crop Intensity by hh
CI<- aggregate(data = yield_data,crop_name ~ hh_refno,function(x) length(unique(x)))
names(CI)[2] <- 'CI'
CI
#Replacing NA by 1
CI<-CI%>% replace(is.na(.), 1)
CI
CIF<-CIF%>%left_join(CI, by='hh_refno')
#classifying CIF - li, mi, hi
CIF$CIF_class<-ifelse(CIF$CIF >= 1 & CIF$CIF < 6,0,
                ifelse(CIF$CIF >= 6,1,""))
CIF$CIIF<-CIF$CIF*CIF$CI
View(CIF)
# #Get hh total ag fields area by hh
hh_field<-read.csv(paste(rawdata_directory,"/household_field/household_field_Jan2018_TZA_nodupl.csv", sep=""))
colnames(hh_field)
ag_area<-hh_field[c(5,7,18)]
head(ag_area)
#making variable names explicit
names(ag_area)[3] <- 'landsize'
ag_area
#aggregation by hhid
ag_area<-aggregate(landsize~ hh_refno, ag_area, sum)
#converting Acres to hectares
ag_area$landsize_ha<-ag_area$landsize*0.404686
ag_area<-ag_area[c(1,3)]
ag_area
# #total cultivated area of hh
cult_area<-subset(yield_data,select=c(hh_refno, areaharvestedpercrop_ha_sum))
cult_area<-aggregate(areaharvestedpercrop_ha_sum~hh_refno,cult_area,sum)
cult_area
#joining
CIF<-CIF%>%left_join(ag_area, by='hh_refno')
CIF$CIIF_adapted<-CIF$CIIF*(CIF$landsize_ha)
View(CIF)
dim(CIF)
colnames(CIF)
CIF<-CIF[c('fertuse_n', 'improvedseeds_n', 'mechtools_n', 'irr_n', 
           'pestuse_n', 'CIF')]
##--CMF----

# #Soil disturbance/ conventional tillage
# #yield_b_12_6	Disturbed soil (generally means tilled) - 
# #What is the surface condition of the field?
# #{0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9, 10: 10}
# colnames(hh_field)
# head(hh_field)
# soiltill<-hh_field[c(5,7,198)]
# soiltill$soil_dist<-(soiltill$yield_b_12_6)
# soiltill<-soiltill[c(1,2,3)]
# soiltill[is.na(soiltill)]<-0
# #make NA = no till
# #aggregation by hhid
# #Getting most repeated response by hhid/mode
# calculatemode<-function(x){
#   uniqx <- unique(x)
#   uniqx[which.max(tabulate(match(x, uniqx)) )]
# }
# soiltill<-aggregate(soiltill,
#                     by = list(soiltill$hh_refno),
#                     FUN = calculatemode)
# head(soiltill)
# soiltill<-soiltill[c(2,4)]
# soiltill$soil_conv_till<-ifelse(soiltill$yield_b_12_6>=5,"1","0")
# soil_conv_till<-soiltill[c(1,3)]
# soil_conv_till

#Soil disturbance/ minimum tillage
#yield_b_12_6	Disturbed soil (generally means tilled) - 
#What is the surface condition of the field?
#{0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9, 10: 10}
colnames(hh_field)
head(hh_field)
soiltill<-hh_field[c("hh_refno", "yield_b_12_6")]
soiltill
names(soiltill)[2]<- 'disturbed'
#removing duplicates
soiltill<- soiltill[!duplicated(soiltill$hh_refno), ]
soiltill[is.na(soiltill)]<-0
soiltill<-na.omit(soiltill)
soiltill
#make NA = no till
#aggregation by hhid
#Getting most repeated response by hhid/mode
# calculatemode<-function(x){
#   uniqx <- unique(x)
#   uniqx[which.max(tabulate(match(x, uniqx)) )]
# }
# soiltill<-aggregate(soiltill,
#                     by = list(soiltill$hh_refno),
#                     FUN = mean)

#soiltill<-aggregate(disturbed ~ hh_refno,soiltill,mean)
soiltill$soiltill<-ifelse(soiltill$disturbed == 0,0,1)
soiltill<-soiltill[c(1,3)]
soiltill

#Residue retention on surface
#ag5a_24	What was done with the residue from this crop?
#{1: 'Crop Produces No   Residue', 2: 'Residue Was Left In   Field', 
#3: 'Mulched', 4: 'For Grazing Own Animals', 5: "For Grazing Others'  
# Animals", 6: 'Feeding Own Animals', 7: 'For Sale'}
#colnames(hh_rrsales)
colnames(hh_fieldcrop)
resret<-hh_fieldcrop[c(5,7,21)]
head(resret)
#making variable names explicit
names(resret)[3]<- 'rr'
#counts
library(dplyr)
resret<-resret%>% count(rr , hh_refno)
#recoding 
library(dplyr)
oldvals <- c(	"Crop Produces No   Residue","Feeding Own Animals","For Grazing Others'   Animals",
              "Mulched","other","Residue Was Left In   Field", na.omit=T)
newvals <- c(1,1,1,2,1,2)
resret$rr_n<-newvals[match(resret$rr,oldvals)]
resret<-na.omit(resret)
resret$resret<-ifelse( resret$rr_n > 1,1,0)
resret$no_resret<-ifelse( resret$resret == 0,1,0)
resret
no_resret<-resret[c(2,6)]
no_resret

#Intercropping
colnames(hh_field_permcrop)
head(hh_field_permcrop)
#ag6a_05--Was cultivation intercropped during the most recent completed season?	{1: 'Yes', 2: 'No'}
intercrop<-hh_field_permcrop[c(5,7,10)]
head(intercrop)
#making variable names explicit
names(intercrop)[3]<- 'intercropping'
head(intercrop)
#recoding categorical to numeric
library(dplyr)
oldvals <- c("FALSE", "TRUE")
newvals <- c(0, 1)
intercrop$intercrop<-newvals[match(intercrop$intercropping,oldvals)]
intercrop
#removing duplicates
intercrop<- intercrop[!duplicated(intercrop$hh_refno), ]
# #aggregation by hhid
# #Getting most repeated response by hhid
# calculatemode<-function(x){
#   uniqx <- unique(x)
#   uniqx[which.max(tabulate(match(x, uniqx)) )]
# }
# intercrop<-aggregate(intercrop,
#                      by = list(intercrop$hh_refno),
#                      FUN = calculatemode)
# head(intercrop)
intercrop$intercropping_n<-ifelse(intercrop$intercrop == 1 ,"1","0")
intercrop<-intercrop[c(1,4)]
dim(intercrop)
# 92 complete observations, not enough
intercrop

#-CA based practices combinations
#merging datasets and replace NA by 0
library(dplyr)
Mgmt<-soiltill%>%left_join(no_resret, by='hh_refno')
Mgmt<- Mgmt %>% replace(is.na(.), 0)
Mgmt
#Index with qualitative data
library(dplyr)
CPF<-Mgmt%>%group_by(soiltill,no_resret) %>%
  mutate(CPF = cur_group_id()) %>%ungroup() %>%arrange(CPF)
#reclassing mgmt impact factor for conservation vs conventional
View(CPF)
#calling clay content data
soil_prop<-read.csv(paste(output_directory,"/Soil_health/field_soil_info_indicators_CONF.csv", sep=""))
soil_prop<-soil_prop[c("hh_refno","farmproc_clay_icraf")]
soil_prop
#joining
CPF<-CPF%>%left_join(soil_prop, by='hh_refno')
dim(CPF)
CPF<-na.omit(CPF)
CPF$farmproc_clay_icraf<-CPF$farmproc_clay_icraf
CPF$CPF_adapted<-CPF$farmproc_clay_icraf*CPF$CPF/100
#classification
CPF$CPF_class<-ifelse(CPF$CPF == 1 | CPF$CPF == 2,0,
                                    ifelse( CPF$CPF>= 3, 1,''))
View(CPF)
dim(CPF)

#--AIS--
# AIS<-CIF%>%left_join(CPF, by='hh_refno')
# colnames(AIS)
# AIS<-AIS[c("hh_refno","fertuse_n","improvedseeds_n","mechtools_n",
#            "irr_n","pestuse_n","CIF","CI","CIIF_adapted","soiltill",
#            "no_resret","CPF","CPF_adapted")]
# AIS<-AIS%>%full_join(VS_MergingFile, by='hh_refno')
# colnames(AIS)
# dim(AIS)
#---IS----
# library(dplyr)
# Practices<-CIF%>%left_join(CPF, by= "hh_refno")
# Practices<-merge(Practices,VS_MergingFile)
# Practices$lands_eplot<-paste(Practices$landscape_no,"_",Practices$eplot_no)
# colnames(Practices)
# Practices<-Practices[c("hh_refno","landscape_no","eplot_no", "fertuse_n","improvedseeds_n","mechtools_n",
#                        "irr_n","pestuse_n", "CIF","CIF_class","CI","CIIF","CIIF_adapted",
#                        "soiltill","no_resret","CPF", "CPF_adapted",           
#                        "CPF_class")]
# colnames(Practices)
# dim(Practices)

#Intensification strategy
#Here we are not considering crop orientation
#here AIS strategy is based on 2 classes Mgmt impact and 2 classes input impact
# library(dplyr)
# AIS<-Practices%>%group_by(CIF_class,CPF_class) %>%
#   mutate(AIS = cur_group_id()) %>%ungroup() %>%arrange(AIS)
# AIS$AIS_class<-ifelse(AIS$AIS == 1, 0,
#                ifelse(AIS$AIS == 2, 1,
#                ifelse(AIS$AIS == 3, 2,
#                ifelse(AIS$AIS == 4, 3,
#                ifelse(AIS$AIS == 5, 4,
#                ifelse(AIS$AIS == 6, 5,""))))))
# AIS<-AIS[!duplicated(AIS$hh_refno),]
# View(AIS)
#----Missing Data Analysis-------------
md_AIS<-as.data.frame(colSums(is.na(CIF[,-c(1)])))
md_AIS

# #--AIS proportions-----------
# dim(imputed.data)
# dim(AIS)
# #CIF
# sum_cif<-c()
# sum_cif<-AIS%>%count(CIF)
# sum_cif<-as.data.frame(sum_cif)
# sum_cif$percent<-sum_cif$n/210*100
# sum_cif<-as.data.frame(sum_cif)
# sum_cif
# #CPF
# sum_cpf<-c()
# sum_cpf<-AIS%>%count(CPF)
# sum_cpf<-as.data.frame(sum_cpf)
# sum_cpf$percent<-sum_cpf$n/210*100
# sum_cpf<-as.data.frame(sum_cpf)
# sum_cpf
write.csv(CIF,'./8_Analyses/Datasets/HH_AIS.csv',row.names = FALSE)


# #-----AII/LII method 1 siil-----------
# #Load data needed all the the household field level
# 
# #Get hh crops and cultivated are data
# yield_data<- read.csv(paste(output_directory,"/Yield/yield_data_hh.csv", sep=""))
# colnames(yield_data)
# yield_data
# 
# #Get hh Nutrients added data
# pnb<-read.csv(paste(output_directory,"/Soil_nutrient_budget/PartNutrBudget_hh.csv", sep=""))
# colnames(pnb)
# pnb
# 
# #Get hh total ag fields area by hh
# hh_field<-read.csv(paste(rawdata_directory,"/household_field/household_field_Jan2018_TZA_nodupl.csv", sep=""))
# colnames(hh_field)
# ag_area<-hh_field[c(5,7,18)]
# head(ag_area)
# #making variable names explicit
# names(ag_area)[3] <- 'landsize'
# ag_area
# #aggregation by hhid
# ag_area<-aggregate(landsize~ hh_refno, ag_area, sum)
# #converting Acres to hectares
# ag_area$landsize_ha<-ag_area$landsize*0.404686
# ag_area<-ag_area[c(1,3)]
# ag_area
# 
# # variables needed
# #crop Intensity by hh
# CI<- aggregate(data = yield_data,crop_name ~ hh_refno,function(x) length(unique(x)))
# names(CI)[2] <- 'CI'
# CI$CI_class<-ifelse(CI$CI == 1,0,
#             ifelse(CI$CI > 1,1,''))
# CI
# # cultivated area of hh
# ag_area<-subset(ag_area,select=c(hh_refno,landsize_ha))
# ag_area
# #  N, P, K inputs  by hh
# inputs<- subset(pnb,select=c(hh_refno,NaddedkghaAllSrcs_mean,PaddedkghaAllSrcs_mean,KaddedkghaAllSrcs_mean))
# inputs
# # Irrigation input by hh
# hh_field_season<-read.csv(paste(rawdata_directory,"/household_field_season/household_field_season_TZA.csv", sep=""))
# colnames(hh_field_season)
# #ag3a_17-Was this FIELD irrigated?{1: 'Yes', 2: 'No'}
# irr<-hh_field_season[c(5,7,20)]
# head(irr)
# #making variable names explicit
# names(irr)[3]<- 'irr_field'
# head(irr)
# #recoding categorical to numeric
# library(dplyr)
# oldvals <- c("FALSE","TRUE", na.omit=TRUE)
# newvals <- c(0,1)
# irr$irr_n<-newvals[match(irr$irr_field, oldvals)]
# irr$irr_n<-as.numeric(irr$irr_n)
# irr<-aggregate(irr_n~ hh_refno,irr,max)
# irr<-irr%>% replace(is.na(.), 0)
# irr
# #total cultivated area of hh
# cult_area<-subset(yield_data,select=c(hh_refno, areaharvestedpercrop_ha_sum))
# cult_area<-aggregate(areaharvestedpercrop_ha_sum~hh_refno,cult_area,sum)
# cult_area
# 
# #Merging the datasets
# library(dplyr)
# AII_data<-CI%>%left_join(cult_area, by='hh_refno')%>%left_join(inputs, by='hh_refno')%>%left_join(irr,
#            by='hh_refno')%>%left_join(ag_area, by='hh_refno')
# AII_data
# #Equation
# #AII = sum(CIF *CA)/AA; CIF is the combined impact factor, CA the cultivated area, AA the total ag land in eplot
# #LII = sum (CIF*CA)/LA; CIF is the combined impact factor, CA the cultivated area, AA the total ag land in the landscape
# #Definition variables N_opt & P_opt in kg/ha
# #CI is crop intensity
# #N_opt is optimal Nitrogen
# #P_opt is optimal Phosphorous
# #W_N is weight factor of Nitrogen
# #W_P is weight factor of Phosphorous
# #W_L is weight factor of land
# N_opt<-100
# P_opt<-30
# W_L<-0.6
# W_N<-0.2
# W_P<-0.1
# W_I<-0.1
# #AII calculations of inputs intensity (I)
# 
# #Nitrogen input intensity
# AII_data$NII<-((AII_data$NaddedkghaAllSrcs_mean/N_opt)*(AII_data$CI))
# AII_data
# #Irrigation input intensity
# AII_data$III<-((AII_data$irr_n*(AII_data$CI)))
# AII_data
# #Phosphorous input intensity
# AII_data$PII<-((AII_data$PaddedkghaAllSrcs_mean/P_opt)*(AII_data$CI))
# AII_data
# 
# #AII calculation of Combined impact factor (CIF)
# #Land impact factor
# #here only make 0.6 if land is cultivated response
# AII_data$LIF<-0.6
# #Nitrogen impact factor
# AII_data$NIF<-((AII_data$NII)*(W_N))
# AII_data
# #Irrigation impact factor
# AII_data$IIF<-((AII_data$III)*(W_I))
# AII_data
# #Phosphorous impact factor
# AII_data$PIF<-((AII_data$PII)*(W_P))
# AII_data
# 
# #Combined impact factor
# AII_data$CIF<-AII_data$LIF + AII_data$NIF + AII_data$IIF + AII_data$PIF
# 
# #AII calculations - calculating the agricultural impact at the field level
# AII_data$AII1<-(AII_data$CIF)*(AII_data$areaharvestedpercrop_ha_sum)/(AII_data$landsize_ha)
# AII_data
# dim(AII_data)
# AII<-AII_data[c(1,16,17)]
# AII
# #Merging with Landscape & eplot information
# AII<-merge(AII_data,VS_MergingFile, by='hh_refno')
# colnames(AII)
# AII<-AII[c("hh_refno","landscape_no","eplot_no","AII1")]
# AII<-AII%>%mutate(AII_Q1 = ntile(AII1, 3))
# AII$AII_Q1_class<-ifelse(AII$AII_Q1 ==1,0,
#                                        ifelse(AII$AII_Q1 ==2,1,
#                                               ifelse(AII$AII_Q1 ==3,2,"")))
# hist(AII$AII_Q1)
# #*(AII_data$areaharvestedpercrop_ha_sum))
# #Outputs
# #----Missing Data Analysis-------------
# md_aii<-as.data.frame(colSums(is.na(AII)))
# md_aii
# #AII sheet
# write.csv(AII,"./8_Analyses/Datasets/HH_AII1.csv", row.names = FALSE)
# 
# #-----AII/LII method 2 -----------
# #Merging the datasets needed
# library(dplyr)
# AII_data2<-cult_area%>%left_join(ag_area, by='hh_refno')%>%left_join(Practices,
#             by='hh_refno')%>%left_join(VS_MergingFile, by='hh_refno')
# colnames(AII_data2)
# #AII calculations - calculating the agricultural impact at the field level
# AII_data2$AII2<-AII_data2$CIIIF*(AII_data2$areaharvestedpercrop_ha_sum)/(AII_data2$landsize_ha)
# colnames(AII_data2)
# #Merging with Landscape & eplot information
# AII2<-AII_data2[c("hh_refno","landscape_no","eplot_no","CIIIF","AII2","Mgmt_index","Mgmt_impact_2class")]
# hist(AII2$AII2)
# #Terciles for AII2
# #terciles
# library(tidyverse)
# #AII2<-AII2%>%mutate(AII_class = ntile(AII2, 3))
# AII2
# AII2<-AII2%>%mutate(AII_Q2 = ntile(AII2, 3))
# AII2$AII_Q2_class<-ifelse(AII2$AII_Q2 ==1,0,
#                         ifelse(AII2$AII_Q2 ==2,1,
#                                ifelse(AII2$AII_Q2 ==3,2,"")))
# hist(AII2$AII_Q2)
# #hist(AII2$AII_class)
# library(dplyr)
# #AII2 %>%count(AII_class)
# #*(AII_data$areaharvestedpercrop_ha_sum))
# #Outputs
# #----Missing Data Analysis-------------
# md_aii<-as.data.frame(colSums(is.na(AII2)))
# md_aii
# colnames(AII2)
# #AII sheet 
# write.csv(AII2,"./8_Analyses/Datasets/HH_AII2.csv", row.names = FALSE)
