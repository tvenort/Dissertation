########################################################
#SIIL Analyses                                        ##
#Variables Selection - Capital Assets Framework      ##
#                                                   ##
######################################################

getwd()
rm(list=ls(all=TRUE))  #Remove objects in memory
#Open libraries
#install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.10.tar.gz", repos = NULL, type="source")
#update.packages('rlang')
library(tidyverse)
library(lubridate)
library(RColorBrewer)
# set directories
output_directory <- "./6_Outputs"
#spatial_directory <- "./3_Data_shapefiles_rasters" # Folder with shapefiles and rasters
rawdata_directory <- "./2_Data_VS_raw"
#add_data_directory <- "./4_Data_additional"


# Datasets ----------------------------------------------------------------

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
hh_fs_diet_san<-read.csv(paste(rawdata_directory,"/household_fs_diet_san/household_fs_diet_san_TZ.csv", sep=""))
colnames(hh_water)
#Loading VS merging file **confidential
#VS_MergingFile<-read.csv("/Users/tvenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-read.csv("/Users/taishavenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-VS_MergingFile[c(3,2,4,5)]
VS_MergingFile$lands_eplot<-paste(VS_MergingFile$landscape_no,"_",VS_MergingFile$eplot_no)
VS_MergingFile
#reomving row with eplot NA & 55 & 0
VS_MergingFile<-filter(VS_MergingFile, eplot_no != 55 & eplot_no != "NA" & eplot_no != 0 )
View(VS_MergingFile)


#----Livelihood Capitals -----
# NC variables ------------------------------------------------------------

# ## --Vegetation
# #loading Contextual Miscellaneous dataset
# GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
# colnames(GS250)
# evi<-GS250[c(334:397),]
# evi<-GS250[c(4,5,22)]
# evi
# names(evi)[3]<- 'Vegetation_index'
# #index binding
# library(dplyr)
# evi<-evi%>%group_by(landscape_no,eplot_no) 
# evi
# #New EVI hh dataframe
# evi_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
# evi_hh
# library(dplyr)
# evi<- full_join(evi_hh, evi, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
# evi<-evi[c(4,6)]
# evi<- evi[!duplicated(evi$hh_refno), ]
# evi

## --Distance to forest
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
dfres<-GS250[c(334:397),]
dfres<-GS250[c(4,5,10)]
dfres
names(dfres)[3]<- 'dist_forest'
#index binding
library(dplyr)
dfres<-dfres%>%group_by(landscape_no,eplot_no)
#New dfres hh dataframe
dfres_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
dfres_hh
library(dplyr)
dfres<- full_join(dfres_hh, dfres,by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
dfres<-dfres[c(4,6)]
dfres<- dfres[!duplicated(dfres$hh_refno), ]
dfres

#--Biodiversity
#Loading AI dataset
bd<-read.csv(paste(output_directory,"/Biodiversity/Biodiversity_All_eplot.csv", sep=""))
bd<-bd[c(329:391),]
bd<-bd[c(1,2,6,3,4,5,7,8,9)]
#index binding
library(dplyr)
bd<-bd%>%group_by(landscape_no,eplot_no) 
#New bd hh dataframe
bd_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
bd_hh
library(dplyr)
bd <- full_join(bd_hh, bd, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
bd<-bd[c(4,7:12)]
bd
#bd<-na.omit(bd)
tree_bd<-bd[c(1,2,3,4)]
herb_bd<-bd[c(1,5,6,7)]
tree_bd
herb_bd
#--Agrodiversity
ad<-read.csv(paste(output_directory,"/Agrodiversity/Agrodiversity_fields.csv", sep=""))
names(ad)[3]<- 'richness_agro_diversity'
names(ad)[4]<- 'shannon_agro_diversity'
names(ad)[5]<- 'simpson_agro_diversity'
ad
atd<-read.csv(paste(output_directory,"/Agrodiversity/Agrotreediversity_fields.csv", sep=""))
names(atd)[3]<- 'richness_agro_tree_diversity'
names(atd)[4]<- 'shannon_agro_tree_diversity'
names(atd)[5]<- 'simpson_agro_tree_diversity'
atd
#New hh dataframes
ad_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
ad_hh
atd_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
atd_hh
library(dplyr)
ad <- full_join(ad_hh, ad, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
ad<-ad[c(4,6,7,8)]
ad
atd<-full_join(atd_hh, atd, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
atd<-atd[c(4,6,7,8)]
atd


#--Rainfall
#loading rainfall dataset
precip<-read.csv(paste(output_directory,"/Precipitation/precip_indicators_eplot.csv", sep=""))
precip<-precip[c(1,2,4,13)] #picking only average precip
precip<-precip[c(344:407),]
precip
# index binding
precip<- precip%>%group_by(landscape_no,eplot_no) 
precip
precip_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
precip_hh
library(dplyr)
rainfall<- full_join(precip_hh, precip, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
rainfall<-rainfall[c(4,6,7)]
#rainfall<-na.omit(rainfall)
rainfall


# ## --Geology/Topography
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
top<-GS250[c(334:397),]
top<-GS250[c("landscape_no","eplot_no","SLOPE","MDEM")]
top
names(top)[3]<- 'Slope'
names(top)[4]<- 'Elevation'
#index binding
library(dplyr)
top<-top%>%group_by(landscape_no,eplot_no)
top
#New TOP hh dataframe
top_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
top_hh
library(dplyr)
top<- full_join(top_hh, top, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
top<-top[c(4,6,7)]
top<- top[!duplicated(top$hh_refno), ]
top


# ## --Air/Temperature
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
temp<-GS250[c(334:397),]
temp<-GS250[c("landscape_no","eplot_no","LSTD","LSTN")]
temp
names(temp)[3]<- 'day_time_temp'
names(temp)[4]<- 'night_time_temp'
#index binding
library(dplyr)
temp<-temp%>%group_by(landscape_no,eplot_no)
temp
#New TOP hh dataframe
temp_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
temp_hh
library(dplyr)
temp<- full_join(temp_hh, temp, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
temp<-temp[c(4,6,7)]
temp<- temp[!duplicated(temp$hh_refno), ]
temp


## --Distance to inland water bodies
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
dows<-GS250[c(334:397),]
dows<-GS250[c(4,5,17)]
dows
names(dows)[3]<-'dist_water_bodies'
#index binding
library(dplyr)
dows<-dows%>%group_by(landscape_no,eplot_no) 
#New dfres hh dataframe
dows_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
dows_hh
library(dplyr)
dows<- full_join(dows_hh, dows, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
dows<-dows[c(4,6)]
dows<- dows[!duplicated(dows$hh_refno), ]
dows

#--Aridity Index
#Loading AI dataset
ai<-read.csv(paste(output_directory,"/Aridity_Index_PET/AI_eplots.csv", sep=""))
ai<-ai[c(344:407),]
#index binding
library(dplyr)
ai<-ai%>%group_by(landscape_no,eplot_no)
#New AI hh dataframe
ai_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
ai_hh
library(dplyr)
ai<- full_join(ai_hh, ai, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
ai<-ai[c(4,6)]
ai


#--Soil
#-- for this data sheet, you want to use ag field soil properties data
#Loading soil health dataset
soil_prop<-read.csv(paste(output_directory,"/Soil_health/field_soil_info_indicators_CONF.csv", sep=""))
soil_prop$lands_eplot<-paste(soil_prop$landscape_no,"_",soil_prop$eplot_no)
colnames(soil_prop)
soil_prop<-soil_prop[c(4,2,3,6:27)]
soil_prop
# index binding
soil_prop<- soil_prop%>%group_by(landscape_no,eplot_no)
soil_prop
soil_prop_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
soil_prop_hh
library(dplyr)
soil_prop<- full_join(soil_prop_hh, soil_prop, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
colnames(soil_prop)
soil_prop<-soil_prop[c(4,7:28)]
soil_prop<- soil_prop[!duplicated(soil_prop$hh_refno.x), ]
names(soil_prop)[1]<-'hh_refno'
soil_prop<-soil_prop[c("hh_refno","farmproc_clay_icraf")]
soil_prop

#soil water
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
sw<-GS250[c(334:397),]
sw<-GS250[c("landscape_no","eplot_no","SWIR1","SWIR2")]
sw
names(sw)[3]<- 'sw_r1'
names(sw)[4]<- 'sw_r2'
#index binding
library(dplyr)
sw<-sw%>%group_by(landscape_no,eplot_no)
sw
#New TOP hh dataframe
sw_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
sw_hh
library(dplyr)
sw<- full_join(sw_hh, sw, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
sw<-sw[c(4,6,7)]
sw<- sw[!duplicated(sw$hh_refno), ]
sw

# #soil physical indicators
# library(dplyr)
# library(tidyverse)
# soil_erod<-read.csv(paste(output_directory,"/Soil_physical/SH_T2_T4_erodibility_Eplot.csv", sep=""))
# soil_erod<-soil_erod[c("landscape_no","eplot_no","SH15")]
# names(soil_erod)[3]<-'soil_erodibility'
# soil_erod<-soil_erod %>%filter(!duplicated(cbind(landscape_no,eplot_no)))
# #normalizing soil erod with less is better
# soil_depth<-read.csv(paste(output_directory,"/Soil_physical/SH_T2_T4_soildepth_Eplot.csv", sep=""))
# soil_depth<-soil_depth[c("landscape_no", "eplot_no","SH46")]
# names(soil_depth)[3]<-'soil_depth'
# soil_depth<-soil_depth%>%filter(!duplicated(cbind(landscape_no,eplot_no)))
# soil_surf<-read.csv(paste(output_directory,"/Soil_physical/SH_T2_T4_soilsurface_Eplot.csv", sep=""))
# soil_surf<-soil_surf[c("landscape_no", "eplot_no","SH27mean")]
# names(soil_surf)[3]<-'soil_surface'
# soil_surf<-soil_surf%>%filter(!duplicated(cbind(landscape_no,eplot_no)))
# #Merging soil physical indicators
# library(dplyr)
# soil_physical<-soil_erod%>%left_join(soil_depth, by=c('landscape_no','eplot_no'))%>%left_join(soil_surf,  
#                                                                                               by=c('landscape_no','eplot_no'))
# soil_physical<-soil_physical[c('landscape_no','eplot_no',"soil_erodibility","soil_depth","soil_surface")]
# soil_physical
# #New hh dataframes
# soil_physical_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
# soil_physical_hh
# library(dplyr)
# soil_physical<- full_join(soil_physical_hh,soil_physical, by = c('landscape_no'='landscape_no',
#                                                                  'eplot_no'='eplot_no'))
# colnames(soil_physical)
# soil_physical<-soil_physical[c("hh_refno","soil_erodibility","soil_depth", "soil_surface")]
# soil_physical<-soil_physical[!duplicated(soil_physical$hh_refno), ]
# soil_physical

#--Connecting NC dataframes
library(tidyverse)
NC<-ai%>%left_join(rainfall, by='hh_refno')%>%left_join(top, by='hh_refno')%>%left_join(temp,
    by='hh_refno')%>%left_join(dows,by='hh_refno')%>%left_join(dfres,by='hh_refno')%>%left_join(tree_bd,
   by='hh_refno')%>%left_join(herb_bd,by='hh_refno')%>%left_join(ad,by='hh_refno')%>%left_join(atd,
  by='hh_refno')%>%left_join(soil_prop,
  by='hh_refno')%>%left_join(sw,by='hh_refno')
NC<- NC[!duplicated(NC$hh_refno), ]
colnames(NC)
NC<-NC[c("hh_refno","ave_precip","dist_water_bodies","Shannon_tree","richness_tree","Slope","farmproc_clay_icraf","Elevation", "dist_forest" )]
as.data.frame(colSums(is.na(NC[,-c(1)])))

# HC variables ------------------------------------------
#--Gender ratio of household
gender_ratio<-hh_individual[c(5,12)]
head(gender_ratio)
male_count<-filter(gender_ratio, ag_indid_gender == "Male")     
male_count<-male_count %>%group_by(hh_refno) %>% count(ag_indid_gender == "Male")
male_count
female_count<-filter(gender_ratio, ag_indid_gender == "Female")   
female_count<-female_count %>%group_by(hh_refno) %>% count(ag_indid_gender == "Female")
female_count
gender_ratio<-merge(male_count,female_count, by='hh_refno')
gender_ratio
names(gender_ratio)[3]<-'male_count'
names(gender_ratio)[5]<-'Female_count'
gender_ratio<-gender_ratio[c(1,3,5)]
gender_ratio$gender_ratio<-gender_ratio$male_count/gender_ratio$Female_count
gender_ratio<-gender_ratio[c(1,4)]
names(gender_ratio)[2]<-'Male_Female_ratio'
gender_ratio
#New hh dataframes
gender_ratio_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
gender_ratio_hh
gender_ratio <- full_join(gender_ratio_hh, gender_ratio, by = 'hh_refno')
gender_ratio<-gender_ratio[c(4,6)]
gender_ratio

#--Age dependency ratio of households
colnames(hh_individual)
members_age<-hh_individual[c(5,7,11)]
head(members_age)
members_age
library("dplyr")
dependents<-members_age%>% group_by(hh_refno) %>%count(ag_indid_age < 15 | ag_indid_age > 64)
names(dependents)[2]<-'dependents'
dependents<-filter(dependents,dependents == 'TRUE')
dependents<-dependents[c(1,3)]
dependents
working<-members_age%>% group_by(hh_refno) %>%count(ag_indid_age >= 15 | ag_indid_age <= 64, sort = TRUE)
names(working)[2]<-'working'
working<-working[c(1,3)]
working
members_dep<-merge(dependents,working, by='hh_refno')
names(members_dep)[2]<-'dependents'
names(members_dep)[3]<-'working'
members_dep$dep_ratio<-members_dep$dependents/members_dep$working
members_dep
dep_ratio<-members_dep[c(1,4)]
names(dep_ratio)[2]<- 'hh_dep_ratio'
dep_ratio
#New hh dataframes
dep_ratio_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
dep_ratio_hh
dep_ratio <- full_join(dep_ratio_hh, dep_ratio, by = 'hh_refno')
dep_ratio<-dep_ratio[c(4,6)]
dep_ratio

# #Highest level of education of HH head
#hh_c07
colnames(hh_individual)
hh_level_edu<-hh_individual[c(5,17,23)]
hh_level_edu
#Filtering age of head of household
library("dplyr")
hh_level_edu<-filter(hh_level_edu, hh_b05 == "Head")
#making variable names explicit
names(hh_level_edu)[3]<- 'leveledu_hh'
hh_level_edu
#recoding categorical to numeric
library(dplyr)
oldvals <- c('D1',"D2","D3","D4", 'D5', 'D6',"D7","D8","F1","F2","F4","F6","U3")
newvals <- c(1,1,1,1,1,1,1,1,2,2,2,2,3)
hh_level_edu$`leveledu`<-newvals[match(hh_level_edu$leveledu, oldvals)]
hh_level_edu<-hh_level_edu[c(1,4)]
head(hh_level_edu)
#Replacing NA by 0
hh_level_edu<- hh_level_edu%>% replace(is.na(.), 0)
hh_level_edu

#Household literacy rate
colnames(hh_individual)
age_edu<-hh_individual[c(5,11,17,23)]
age_edu
#making variable names explicit
names(age_edu)[4]<- 'leveledu'
age_edu
#recoding categorical to numeric
library(dplyr)
oldvals <- c('D1', "D2","D3","D4", 'D5', 'D6',"D7","D8","F1","F2","F4","F6","U3")
newvals <- c(1,1,1,1,1,1,1,1,2,2,2,2,3)
age_edu$leveledu<-newvals[match(age_edu$leveledu, oldvals)]
age_edu<-age_edu[c(1,2,3,4)]
#Replacing NA by 0
age_edu<- age_edu%>% replace(is.na(.), 0)
age_edu
#getting skilled labor ratio
library("dplyr")
literate<-age_edu%>% group_by(hh_refno) %>%count(ag_indid_age >= 15 | leveledu >= 1)
names(literate)[2]<-'literate_members'
literate<-filter(literate,literate_members == 'TRUE')
literate <-literate [c(1,3)]
literate
total_members<-age_edu%>% group_by(hh_refno) %>%count()
total_members
literacy_rate<-merge(literate,total_members, by='hh_refno')
names(literacy_rate)[2]<-'literate'
names(literacy_rate)[3]<-'total_members'
literacy_rate$literacy_rate<-literacy_rate$literate/literacy_rate$total_members
literacy_rate
#New hh dataframes
literacy_rate_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
literacy_rate_hh
literacy_rate <- full_join(literacy_rate_hh, literacy_rate, by = 'hh_refno')
literacy_rate<-literacy_rate[c(4,8)]
literacy_rate

#--Household Members
colnames(hh_individual)
members<-hh_individual[c(5,7)]
head(members)
library(dplyr)
members <-members %>% group_by(hh_refno) %>% 
  summarise(total_count=n(),.groups = 'drop') %>% 
  as.data.frame()
head(members)
#making variable names explicit
names(members)[2]<- 'hhmembers'
members

#--Human labor hours/ days
colnames(hh_field_season_individual)
#ag3a_70_preparing - How many days did the individual spend on the following activities on this field
#ag3a_70_weeding
#ag3a_70_fertilizing
#ag3a_70_harvesting
labor<-hh_field_season_individual[c(5,13,14,15,16)]
head(labor)
#making variable names explicit
names(labor)[2]<- 'prep'
names(labor)[3]<- 'weed'
names(labor)[4]<- 'fert'
names(labor)[5]<- 'harv'
head(labor)
#Replacing NA by 0
labor<- labor%>% replace(is.na(.), 0)
labor
#total hrs of human labor
labor$prep<-as.numeric(labor$prep)
labor$weed<-as.numeric(labor$weed)
labor$fert<-as.numeric(labor$fert)
labor$harv<-as.numeric(labor$harv)
labor$totallabordays = rowSums(labor[,c(2,3,4,5)])
#aggregation by hhid
labor<-aggregate(totallabordays ~ hh_refno ,labor, mean)
names(labor)[2]<- 'hh_labor_days'
labor
#New hh dataframes
labor_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
labor_hh
labor <- full_join(labor_hh, labor, by = 'hh_refno')
labor<-labor[c(4,6)]
labor

#Connecting HC dataframes
library(tidyverse)
HC<-gender_ratio%>%left_join(members,by='hh_refno')%>%left_join(dep_ratio,by='hh_refno')%>%left_join(literacy_rate,
    by='hh_refno')%>%left_join(hh_level_edu,by='hh_refno')%>%left_join(labor, by='hh_refno')
                                                                                    
HC<- HC[!duplicated(HC$hh_refno), ]
as.data.frame(colSums(is.na(HC[,-c(1)])))

#-----SC Capital------------------------------------------------------------

#--Information network
hh_priceinfo
#source_name_family
#{1: 'Ministry Of Agriculture', 2: 'Ngo', 3: 'Cooperative',
#4: 'Large Scale Farmer/Outgrowers', 5: 'Radio',
#'3a': "Community Based Farmers Organisations (Cbos)/Farmer Based Organisations (Fbos)",
#7: 'Neighbour', 9: 'Chief Farmer', 6: 'Publication (Newspapers/Articles)', '6a': 'Market Women / Middlemen'}
an<-hh_priceinfo[c(5,10)]
#making variable names explicit
names(an)[2] <- 'advnet'
head(an)
oldvals <- c('CHIEF FARMER',  'COOPERATIVE', 'LARGE SCALE FARMER/OUTGROWERS',
             'MARKET WOMEN / MIDDLEMEN', 'MINISTRY OF AGRICULTURE',
             'MINISTRY OF FOOD & AGRICULTURE (MOFA)','NEIGHBOUR', 'NGO',
             'OTHER, (SPECIFY)','PUBLICATION','RADIO')
newvals <- c(1,2,3,4,5,6,7,8,9,10,11)
an$advnet_n<-newvals[match(an$advnet, oldvals)]
head(an)
#Replacing NA by 0
an<- an%>% replace(is.na(.), 0)
colnames(an)
SC<-an[c("hh_refno","advnet","advnet_n")]
people_oldvals<-c(1,2,3,4,5,6,7,8,9,10,11)
people_newvals<-c(1,0,0,1,0,0,1,0,0,0,0)
media_oldvals<-c(1,2,3,4,5,6,7,8,9,10,11)
media_newvals<-c(0,0,0,0,0,0,0,0,0,1,1)
institutions_oldvals<-c(1,2,3,4,5,6,7,8,9,10,11)
institutions_newvals<-c(0,1,1,0,1,1,0,2,0,0,0)
SC$advnet_people<-people_newvals[match(SC$advnet_n, people_oldvals)]
SC$advnet_media<-media_newvals[match(SC$advnet_n, media_oldvals)]
SC$advnet_institutions<-institutions_newvals[match(SC$advnet_n, institutions_oldvals)]
SC
#recoding
SC$Info_network<-ifelse(SC$advnet_people == 0 & SC$advnet_media == 0 & SC$advnet_institutions == 0,1,
                  ifelse(SC$advnet_people == 0 & SC$advnet_media == 1 & SC$advnet_institutions == 0,2,
                  ifelse(SC$advnet_people == 0 & SC$advnet_media == 0 & SC$advnet_institutions == 1,3,
                  ifelse(SC$advnet_people == 1 & SC$advnet_media == 0  & SC$advnet_institutions == 0,4,
                  ifelse(SC$advnet_people == 0 & SC$advnet_media == 1 & SC$advnet_institutions == 1,4, 
                  ifelse(SC$advnet_people == 1 & SC$advnet_media == 0 & SC$advnet_institutions == 1,5,
                   ifelse(SC$advnet_people == 1 & SC$advnet_media == 1 & SC$advnet_institutions == 1,6,  
                                                                                  "1")))))))

SC
#New dfres hh dataframe
SC_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
SC_hh
library(dplyr)
SC<- full_join(SC_hh, SC, by = "hh_refno")
SC<- SC[!duplicated(SC$hh_refno), ]
SC<-SC[c(4,11)]
# NA replacement by 0
SC$Info_network <-SC$Info_network%>%replace(is.na(.), 1)
#SC<-SC[c("hh_refno"," Social_network")]
as.data.frame(colSums(is.na(SC[,-c(1)])))
SC


#Market network
#--Information network
hh_priceinfo
#source_name_family
#{1: 'Ministry Of Agriculture', 2: 'Ngo', 3: 'Cooperative',
#4: 'Large Scale Farmer/Outgrowers', 5: 'Radio',
#'3a': "Community Based Farmers Organisations (Cbos)/Farmer Based Organisations (Fbos)",
#7: 'Neighbour', 9: 'Chief Farmer', 6: 'Publication (Newspapers/Articles)', '6a': 'Market Women / Middlemen'}
an<-hh_priceinfo[c(5,10)]
#making variable names explicit
names(an)[2] <- 'advnet'
head(an)
oldvals <- c('CHIEF FARMER',  'COOPERATIVE', 'LARGE SCALE FARMER/OUTGROWERS',
             'MARKET WOMEN / MIDDLEMEN', 'MINISTRY OF AGRICULTURE',
             'MINISTRY OF FOOD & AGRICULTURE (MOFA)','NEIGHBOUR', 'NGO',
             'OTHER, (SPECIFY)','PUBLICATION','RADIO')
newvals <- c(1,2,3,4,5,6,7,8,9,10,11)
an$advnet_n<-newvals[match(an$advnet, oldvals)]
head(an)
#Replacing NA by 0
an<- an%>% replace(is.na(.), 0)
colnames(an)
market<-an[c("hh_refno","advnet","advnet_n")]
market$market_network<-ifelse(market$advnet_n == 4, 1,0)
market<-market[c("hh_refno","market_network")]
# #--distance from field to market
# #ag2a_10_3--MARKET (KM) - What is the distance from this FIELD to
# colnames(hh_field)
# market<-hh_field[c(5,23)]
# market
# #making variable names explicit 
# names(market)[2] <- 'Market_network'
# #market$marketdist<-as.numeric(market$marketdist)
# #aggregation by hhid
# market<-aggregate(Market_network ~ hh_refno,market,mean)
# market
# #New hh dataframes
# market_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
# market_hh
# market <- full_join(market_hh, market, by = 'hh_refno')
# market<-market[c(4,6)]
# market
SC<-SC%>%left_join(market,by='hh_refno')
SC

##------PC variables---------------------------------------------- 

#--Agricultural implements 
colnames(hh_implement)
#ag11_01--Number owned
#ag11_07--Number borrowed
implements<-hh_implement[c(5,7,8,10,11)]
head(implements)
#making variable names explicit
names(implements)[5] <- 'ag_implements'
head(implements)
implements<-aggregate(ag_implements ~ hh_refno,implements,sum)
implements

#--Irrigation Infrastructure
colnames(hh_assets)
#water infrastructure : 440 hoes,468 watering can,442 waterpumping machine,
#455 irrigation sprinklers
#water infrastructure
water_inf<-hh_assets[c(6,76,48,63,50)]
water_inf
#making variable names explicit
names(water_inf)[2] <- 'watering can'
names(water_inf)[3] <- 'hoes'
names(water_inf)[4] <- 'sprinkler'
names(water_inf)[5] <- 'pumping machine'
water_inf
nonmech_water_inf<-as.data.frame(rowSums(water_inf[,c(2,3)]))
nonmech_water_inf<-cbind(water_inf[1],nonmech_water_inf)
mech_water_inf<-as.data.frame(rowSums(water_inf[,c(4,5)]))
mech_water_inf<-cbind(water_inf[1],mech_water_inf)
names(nonmech_water_inf)[2] <- 'nonmech_water_inf'
names(mech_water_inf)[2] <- 'mech_water_inf'
nonmech_water_inf
mech_water_inf
#recoding
nonmech_water_inf$nonmech_water_inf<-ifelse(nonmech_water_inf$nonmech_water_inf >= 1,1,0)
mech_water_inf$mech_water_inf<-ifelse(mech_water_inf$mech_water_inf >= 1,1,0)
water_inf<-merge(nonmech_water_inf,mech_water_inf)
water_inf
water_inf$water_inf<-ifelse(water_inf$nonmech_water_inf == 0 & water_inf$mech_water_inf== 0,2,
                            ifelse(water_inf$nonmech_water_inf == 1 & water_inf$mech_water_inf== 0,3,
                                   ifelse(water_inf$nonmech_water_inf == 0 & water_inf$mech_water_inf== 1,4,
                                          ifelse(water_inf$nonmech_water_inf == 1 & water_inf$mech_water_inf== 1,5,       
                                                 "1"))))      

water_inf<-water_inf[c(1,4)]
water_inf
names(water_inf)[1]<-'householdid'
water_inf
#merge
water_inf<-merge(water_inf,VS_MergingFile, by="householdid")
water_inf<-water_inf[c(5,2)]
water_inf
#New hh dataframe
water_inf_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
water_inf_hh
water_inf<- full_join(water_inf_hh, water_inf, by = 'hh_refno')
water_inf<-water_inf[c(4,6)]
water_inf


#--Farm structures
colnames(hh_assets)
#water infrastructure : 440 hoes,468 watering can,442 waterpumping machine,
#455 irrigation sprinklers
#water infrastructure
farm_struct<-hh_assets[c(6,68,69,70)]
farm_struct
#making variable names explicit
names(farm_struct)[2] <- 'silos'
names(farm_struct)[3] <- 'sheds'
names(farm_struct)[4] <- 'storeroom'
farm_struct
#recoding
farm_struct$farm_structures<-ifelse(farm_struct$silos == 0 & farm_struct$sheds== 0 & farm_struct$storeroom==0,1,
                                    ifelse(farm_struct$silos >= 0 & farm_struct$sheds== 0 & farm_struct$storeroom==0,2,
                                           ifelse(farm_struct$silos == 0 & farm_struct$sheds >= 0 & farm_struct$storeroom==0,2,
                                                  ifelse(farm_struct$silos == 0 & farm_struct$sheds== 0 & farm_struct$storeroom>=0,2,      
                                                         ifelse(farm_struct$silos >= 0 & farm_struct$sheds >= 0 & farm_struct$storeroom==0,3,
                                                                ifelse(farm_struct$silos >= 0 & farm_struct$sheds == 0 & farm_struct$storeroom>=0,3,         
                                                                       ifelse(farm_struct$silos == 0 & farm_struct$sheds >= 0 & farm_struct$storeroom>=0,3,             
                                                                              ifelse(farm_struct$silos >= 0 & farm_struct$sheds>= 0 & farm_struct$storeroom >=0,4,       
                                                                                     "na"))))))))     

farm_struct
farm_struct<-farm_struct[c(1,5)]
farm_struct
names(farm_struct)[1]<-'householdid'
farm_struct
#merge
farm_struct<-merge(farm_struct,VS_MergingFile, by="householdid")
farm_struct<-farm_struct[c(5,2)]
farm_struct
#New hh dataframe
farm_struct_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
farm_struct_hh
farm_struct<- full_join(farm_struct_hh, farm_struct, by = 'hh_refno')
farm_struct<-farm_struct[c(4,6)]
farm_struct


#---Communication Infrastructure
colnames(hh_assets)
#401 radio, 042 landline, 403 mobile phone
comm_inf<-hh_assets[c(6,9,11)]
names(comm_inf)[2] <- 'radio'
names(comm_inf)[3] <- 'mobile phone'
comm_inf
#Recoding 
comm_inf$radio_n<-ifelse(comm_inf$radio >= 1,1,0)
comm_inf$mobile_n<-ifelse(comm_inf$`mobile phone` >= 1,1,0)
comm_inf$comm_inf<-ifelse( comm_inf$radio_n== 0 & comm_inf$mobile_n== 0,1,
                           ifelse(comm_inf$radio_n == 1 & comm_inf$mobile_n== 0,2,
                                  ifelse( comm_inf$radio_n== 0 & comm_inf$mobile_n == 1,3,
                                          ifelse(comm_inf$radio_n == 1 &comm_inf$mobile_n == 1,4,       
                                                 "na"))))   
comm_inf<-comm_inf[c(1,6)]
comm_inf
names(comm_inf)[1]<-'householdid'
comm_inf
#merge
comm_inf<-merge(comm_inf,VS_MergingFile, by="householdid")
comm_inf<-comm_inf[c(5,2)]
comm_inf
#New hh dataframe
comm_inf_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
comm_inf_hh
comm_inf<- full_join(comm_inf_hh, comm_inf, by = 'hh_refno')
comm_inf<-comm_inf[c(4,6)]
comm_inf


#--distance to nearest cellular tower or wi-fi
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
dcell<-GS250[c(4,5,9)]
dcell<-dcell[(334:397),]
dcell
#index binding
library(dplyr)
dcell<-dcell%>%group_by(landscape_no,eplot_no) 
#New dfres hh dataframe
dcell_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
dcell_hh
library(dplyr)
dcell<- full_join(dcell_hh, dcell, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
dcell<-dcell[c(4,6)]
#dfres<-na.omit(dfres)
names(dcell)[2] <- 'dist_cell_wifi_tower'
dcell


#transportation means 
colnames(hh_assets)
#425 motor vehicle,426, motor cycle,427 bicycle
transp_inf<-hh_assets[c(6,35,34,33)]
names(transp_inf)[2] <- 'bicycle'
names(transp_inf)[3] <- 'motor cycle'
names(transp_inf)[4] <- 'motor vehicle'
transp_inf
#Recoding 
transp_inf$bicycle<-ifelse(transp_inf$bicycle >= 1,1,0)
transp_inf$`motor cycle`<-ifelse(transp_inf$`motor cycle` >= 1,1,0)
transp_inf$`motor vehicle`<-ifelse(transp_inf$`motor vehicle` >= 1,1,0)
transp_inf$transp_inf<-ifelse(transp_inf$bicycle  == 0 & transp_inf$`motor cycle` == 0 & transp_inf$`motor vehicle`==0 ,1,
                        ifelse(transp_inf$bicycle  == 1 & transp_inf$`motor cycle` == 0 & transp_inf$`motor vehicle`==0 ,2,
                         ifelse( transp_inf$bicycle  == 0 & transp_inf$`motor cycle` == 1 & transp_inf$`motor vehicle`==0 ,3,
                         ifelse( transp_inf$bicycle  == 1 & transp_inf$`motor cycle` == 1 & transp_inf$`motor vehicle`==0 ,4,        
                         ifelse(transp_inf$bicycle  == 0 & transp_inf$`motor cycle` == 0 & transp_inf$`motor vehicle`==1,5,
                          ifelse(transp_inf$bicycle  == 1 & transp_inf$`motor cycle` == 0 & transp_inf$`motor vehicle`==1,6,
                           ifelse(transp_inf$bicycle  == 0 & transp_inf$`motor cycle` == 1 & transp_inf$`motor vehicle`==1 ,7,
                         ifelse(transp_inf$bicycle  == 1 & transp_inf$`motor cycle` == 1 & transp_inf$`motor vehicle`==1 ,8, 
                                                                                 "na")))))))) 
transp_inf<-transp_inf[c(1,5)]
transp_inf
names(transp_inf)[1]<-'householdid'
transp_inf
#merge
transp_inf<-merge(transp_inf,VS_MergingFile, by="householdid")
transp_inf<-transp_inf[c(5,2)]
transp_inf
#New hh dataframe
transp_inf_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
transp_inf_hh
transp_inf<- full_join(transp_inf_hh, transp_inf, by = 'hh_refno')
transp_inf<-transp_inf[c(4,6)]
transp_inf

#--Distance to main roads (km)
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
dor1<-GS250[c(4,5,15)]
dor1<-dor1[(334:397),]
dor1
#index binding
library(dplyr)
dor1<-dor1%>%group_by(landscape_no,eplot_no) 
#New dfres hh dataframe
dor1_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
dor1_hh
dor1<- full_join(dor1_hh, dor1, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
dor1<-dor1[c(4,6)]
#dfres<-na.omit(dfres)
names(dor1)[2] <- 'dist_main_roads'
dor1

#Distance to any known  road/path (km)
#loading Contextual Miscellaneous dataset
GS250<-read.csv(paste(output_directory,"/Contextual_misc/GS250_Eplot.csv", sep=""))
colnames(GS250)
dor2<-GS250[c(4,5,16)]
dor2<-dor2[(334:397),]
dor2
#index binding
library(dplyr)
dor2<-dor2%>%group_by(landscape_no,eplot_no)
#New dfres hh dataframe
dor2_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no)
dor2_hh
library(dplyr)
dor2<- full_join(dor2_hh, dor2, by = c('landscape_no'='landscape_no','eplot_no'='eplot_no'))
dor2<-dor2[c(4,6)]
#dfres<-na.omit(dfres)
names(dor2)[2] <- 'dist_any_roads'
dor2
#--Time travel to field
#ag2a_2_2--ROAD (KM) - What is time travel to field
colnames(hh_field)
time_field_travel<-hh_field[c(5,16)]
time_field_travel
#making variable names explicit
names(time_field_travel)[2] <- 'time_field_travel'
time_field_travel$time_field_travel<-as.numeric(time_field_travel$time_field_travel)
#Replacing NA by 0
time_field_travel<- time_field_travel%>% replace(is.na(.), 0)
time_field_travel
#aggregation by hhid
time_field_travel<-aggregate(time_field_travel ~ hh_refno,time_field_travel,mean)
time_field_travel
#New dfres hh dataframe
time_field_travel_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
time_field_travel_hh
time_field_travel<- full_join(time_field_travel_hh, time_field_travel, by = 'hh_refno')
time_field_travel<-time_field_travel[c(4,6)]
time_field_travel

#Connecting PC dataframes
library(tidyverse)
PC<-implements%>%left_join(water_inf,by='hh_refno')%>%left_join(farm_struct,by='hh_refno')%>%left_join(comm_inf,
     by='hh_refno')%>%left_join(dcell,by='hh_refno')%>%left_join(transp_inf,
    by='hh_refno')%>%left_join(dor1,by='hh_refno')%>%left_join(dor2,by='hh_refno')%>%left_join(time_field_travel,by='hh_refno')
#PC<-na.omit(PC)
PC<- PC[!duplicated(PC$hh_refno), ]
colnames(PC)
PC<-PC[c("hh_refno","ag_implements", "water_inf", "farm_structures",
         "comm_inf", "dist_cell_wifi_tower","transp_inf","dist_main_roads","dist_any_roads", "time_field_travel")]
as.data.frame(colSums(is.na(PC[,-c(1)])))


##---FC variables----------------------------------------------------------

#--Basic Housing infrastructure
#--Sanitation infrastructure
colnames(hh_fs_diet_san)
hh_fs_diet_san
housing_inf<-hh_fs_diet_san[c("hh_refno", "hh_j05","hh_j06","hh_j07", "hh_j09","hh_j13","hh_j15","hh_j11","hh_j12")]
names(housing_inf)[2] <- 'walls'
names(housing_inf)[3] <- 'roof'
names(housing_inf)[4] <- 'floor'
names(housing_inf)[5] <- 'san'
names(housing_inf)[6] <- 'electricity'
names(housing_inf)[7] <- 'water'
names(housing_inf)[8] <- 'fuel_cook'
names(housing_inf)[9] <- 'fuel_light'
housing_inf
#walls recoding
list1 <- as.list(unique(housing_inf$walls))
list1
oldvals <- c("Mud Only","Mud Bricks","Baked/Burnt Bricks","Poles And Mud/Mud And Stones", "Poles (Including Bamboo","Concret")
newvals <- c(1,2,3,4,5,6)
housing_inf$house_walls_n<-newvals[match(housing_inf$walls, oldvals)]
housing_inf
#roof recoding 
list2 <- as.list(unique(housing_inf$roof))
list2
oldvals <- c("Grass (Thatch","Thatch And Wood","Metal Sheets (Gci)","Concret")
newvals <- c(1,2,3,4)
housing_inf$house_roof_n<-newvals[match(housing_inf$roof, oldvals)]
housing_inf
#floor recoding
list3 <- as.list(unique(housing_inf$floor))
list3
oldvals <- c("Earth","Concret")
newvals <- c(1,2)
housing_inf$house_floor_n<-newvals[match(housing_inf$floor, oldvals)]
housing_inf
# san recoding
list4 <- as.list(unique(housing_inf$san))
list4
oldvals <- c("No Toilet","Unimproved Pit Latrine (Slab Not Washable)", "Improved Pit Latrine (Slab Washable)", "Pour Flush")
newvals <- c(1,2,3,4)
housing_inf$house_san_n<-newvals[match(housing_inf$san, oldvals)]
housing_inf
# electricity recoding
list5 <- as.list(unique(housing_inf$electricity))
list5
oldvals <- c("NA","National Grid","Community Generator","Solar Panels","Own Generator")
newvals <- c(1,2,3,4,5)
housing_inf$house_electricity_n<-newvals[match(housing_inf$electricity, oldvals)]
#Replacing NA by 1
housing_inf$house_electricity_n<-housing_inf$house_electricity_n%>% replace(is.na(.), 1)
housing_inf
#house_water recoding
list6 <- as.list(unique(housing_inf$water))
list6
oldvals <- c('Rive','Othe','Public Standpipe/Tap','Neighbouring Household','Unprotected Well Without Pump','Protected Well Without Pump',
             'Unprotected Well With Pump','Protected Well With Pump','Private Outside Standpipe/Tap','Piped Water Inside Dwelling')
newvals <- c(1,2,3,4,5,6,7,8,9,10)
housing_inf$house_water_n<-newvals[match(housing_inf$water, oldvals)]
housing_inf
#house_fuel_cook recoding
list7<-as.list(unique(housing_inf$fuel_cook))
list7
oldvals <- c("Firewood","Gas Charcoal","Paraffin/Kerosene")
newvals <- c(1,2,3)
housing_inf$house_fuel_cook_n<-newvals[match(housing_inf$fuel_cook, oldvals)]
housing_inf
#house_fuel_light recoding
list8<-as.list(unique(housing_inf$house_fuel_light))
list8
oldvals <- c("Firewood","Candle ","other", "Lamp Oil ", "Solar", "Electricity","Private  Generator")
newvals <- c(1,2,3,3,4,5,6)
housing_inf$house_fuel_light_n<-newvals[match(housing_inf$fuel_light, oldvals)]
colnames(housing_inf)
housing_inf<-housing_inf[c("hh_refno","house_walls_n","house_roof_n","house_floor_n",      
                           "house_san_n","house_electricity_n", "house_water_n","house_fuel_cook_n",
                           "house_fuel_light_n")]
housing_inf

#-income entries
colnames(hh_individual)
wage_entry<-hh_individual[c("hh_refno","hh_e08")]
wage_entry$wage_entry_n<- as.integer(as.logical(wage_entry$hh_e08))
#Replacing NA by 0
wage_entry<-wage_entry%>% replace(is.na(.), 0)
wage_entry
#aggregation by hhid
wage_entry<-aggregate(wage_entry_n ~ hh_refno,wage_entry,sum)
wage_entry


#--Livestock holdings
#ag10a_05_1-Indigenous - How many does this household currently own?
#ag10a_05_2- Improved - How many does this household currently own?
#ag10a_05_3-Improved Dairy - How many does this household currently own?
colnames(hh_livestock)
liv_hold<-hh_livestock[c(5,10,11,12,13,14,15)]
liv_hold<-liv_hold[!duplicated(liv_hold$hh_refno), ]
liv_hold
#making variable names explicit
names(liv_hold)[3] <- 'indigeneous'
names(liv_hold)[4] <- 'improved'
names(liv_hold)[5] <- 'improvedD'
names(liv_hold)[6] <- 'sale'
names(liv_hold)[7] <- 'liv_sale_number'
liv_hold
#making variable names explicit
liv_hold$indigeneous<-as.numeric(liv_hold$indigeneous)
liv_hold$improved<-as.numeric(liv_hold$improved)
liv_hold$improvedD<-as.numeric(liv_hold$improvedD)
#Replacing NA by 0
liv_hold<-liv_hold%>% replace(is.na(.), 0)
liv_hold
liv_hold$totalliv<-rowSums(liv_hold[,c(3,4,5)])
liv_hold$liv_hold_sale<- as.integer(as.logical(liv_hold$sale))
liv_hold<-liv_hold[c(1,9,7)]
liv_hold
library(dplyr)
#New hh dataframe
liv_hold_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
liv_hold_hh
liv_hold<- full_join(liv_hold_hh, liv_hold, by = 'hh_refno')
liv_hold<-liv_hold[c(4,7)]
liv_hold

#--Land size 
colnames(hh_field)
#"ag2a_04"--Area (Acres) Farmers estimate
land<-hh_field[c(5,7,11)]
head(land)
#making variable names explicit
names(land)[3] <- 'landsize'
land
#aggregation by hhid
land<-aggregate(landsize~ hh_refno, land, sum)
land[land == 0] <- NA
#New hh dataframe
land_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
land_hh
land<- full_join(land_hh, land, by = 'hh_refno')
land<-land[c(4,6)]
land$farm_type<-ifelse(land$landsize < 5, 1,
                       ifelse(land$landsize >= 5 & land$landsize < 10,2,
                              ifelse(land$landsize >=10,3,'NA')))

land
#--cultivated fields & fallow fields
#ag3a_03--How did you use this FIELD?{1: 'Cultivated', 2: 'Rented Out', 3: 'Given Out', 4: 'Fallow', 5: 'Forest'}
colnames(hh_field_season)
landuse<-hh_field_season[c(5,7,12)]
landuse
#Removing duplicates
landuse<-landuse[!duplicated(landuse[,c("hh_refno","field_no")]),]
#making variable names explicit
names(landuse)[3] <- 'landtype'
landuse
#count
library(dplyr)
lu<-landuse%>% count(landtype, hh_refno)
lu
#recoding likert scale
library(dplyr)
oldvals <- c( 'Cultivated','Fallow','Rented Out','Forest')
newvals <- c(0,1,2,3)
landuse$landtype_n<-newvals[match(landuse$landtype, oldvals)]
head(landuse)
#making new binary columns for cultivated land and fallow land variables
cl_oldvals<-c(0,1,2,3)
cl_newvals<-c(1,0,0,0)
fl_oldvals<-c(0,1,2,3)
fl_newvals<-c(0,1,0,0)
landuse$cult_fields<-cl_newvals[match(landuse$landtype_n, cl_oldvals)]
landuse$fallow_fields<-fl_newvals[match(landuse$landtype_n, fl_oldvals)]
landuse<-landuse[c(1,5,6)]
landuse
#--cultivalted fields
cl<-landuse[c(1,2)]
# Aggregation by hhid
cl<-aggregate(cult_fields~ hh_refno, cl,sum)
cl
#--fallow fields
fl<-landuse[c(1,3)]
# Aggregation by hhid --removing duplicates
fl<-aggregate(fallow_fields~ hh_refno, fl,sum, na.omit=T)
fl
fields<-cbind(cl,fl)
fields<-fields[c(1,2,4)]
fields
fields<-rowSums(fields[, c("cult_fields", "fallow_fields")])
fields<-as.data.frame(fields)
fields
#Sum of avg cultivated and fallow fields
Ag_fields<-cbind(fl$hh_refno,fields)
Ag_fields
names(Ag_fields)[1] <- 'hh_refno'
Ag_fields
#New hh dataframe
Ag_fields_hh <- VS_MergingFile%>%group_by(landscape_no,eplot_no) 
Ag_fields_hh
Ag_fields<- full_join(Ag_fields_hh, Ag_fields, by = 'hh_refno')
Ag_fields<-Ag_fields[c(4,6)]
Ag_fields
#-Household tenure
colnames(hh_fs_diet_san)
hh_tenure<-hh_fs_diet_san[c( "hh_refno","hh_j01")]
names(hh_tenure)[2] <- 'housing_tenure'
hh_tenure
list10 <- as.list(unique(hh_tenure$housing_tenure))
list10
oldvals <- c("Rented","Employer Provided - Free","Free", "Owner Occupied")
newvals <- c(1,2,3,4)
hh_tenure$house_tenure_n<-newvals[match(hh_tenure$housing_tenure, oldvals)]
hh_tenure<-hh_tenure[c(1,3)]
hh_tenure

#-Microfinancing 
colnames(hh_fs_diet_san)
hh_microf<-hh_fs_diet_san[c("hh_refno","ag12b_09a")]
names(hh_microf)[2] <- 'microfinance'
hh_microf
hh_microf$microfinance_n <- as.integer(as.logical(hh_microf$microfinance))
hh_microf<-hh_microf[c(1,3)]
hh_microf
oldvals <- c(0,1)
newvals <- c(1,2)
hh_microf$hh_microf_n<-newvals[match(hh_microf$microfinance_n, oldvals)]
hh_microf<-hh_microf[c(1,3)]
hh_microf
#Connecting FC dataframes
library(tidyverse)
FC<-wage_entry%>%left_join(liv_hold,by='hh_refno')%>%left_join(hh_tenure,
    by='hh_refno')%>%left_join(housing_inf, by='hh_refno')%>%left_join(hh_microf,
    by='hh_refno')%>%left_join(land, by='hh_refno')%>%left_join(Ag_fields, by='hh_refno')
FC<- FC[!duplicated(FC$hh_refno), ]
as.data.frame(colSums(is.na(FC[,-c(1)])))

#---Livelihood strategies------
#--PO--
#Primary crops
colnames(hh_field_season_fieldcrop)
crops<-hh_field_season_fieldcrop[c(5,7,12)]
#Getting most repeated crops in total fields per household
calculatemode<-
  function(x){
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)) )]
  }
prim_crops<-aggregate(crops,
                      by = list(crops$hh_refno),
                      FUN = calculatemode)
prim_crops<-prim_crops[c(2,4)]
prim_crops
#categorizing crops by maize, rice, wheat or non-cereal orientation 
# here we ignore wheat and finger millet as they are not so popular 
oldvals <- c("Paddy","Wheat","Finger Millet","Groundnut","Cashew nut","Sesame","Sesame/Simsim",
             "sugar Cane","Maize","Cassava","Beans","Onions",
             "Timber")
newvals <- c(2,0,0,0,0,0,0,0,1,0,0,0,0)
prim_crops$prim_crops_cereal<-newvals[match(prim_crops$crop_name, oldvals)]
prim_crops$prim_crops_cereal<-as.numeric(prim_crops$prim_crops_cereal)
prim_crops
#--Livelihood outcomes --------
colnames(hh_fs_diet_san)
hh_fs<-hh_fs_diet_san[c("hh_refno","hh_i01", "hh_i08")]
names(hh_fs)[2]<-'short_term_FI'
names(hh_fs)[3]<-'long_term_FI'
cols <- sapply(hh_fs, is.logical)
hh_fs[,cols] <- lapply(hh_fs[,cols], as.numeric)
hh_fs 
#----Merging all Capitals-------------------------------------------------------
#Connecting all raw dataframes
library(tidyverse)
LF<-NC %>%left_join(HC, by='hh_refno')%>%left_join(SC, by='hh_refno')%>%left_join(PC,
     by='hh_refno') %>%left_join(FC, by='hh_refno') %>%left_join(prim_crops, by='hh_refno') %>%left_join(hh_fs, by='hh_refno')
LF<-LF[!duplicated(LF$hh_refno),]
#Dropping rows of 5 inexistant household 
LF<-LF[!(LF$hh_refno =="TZA-L10-H25" | LF$hh_refno =="TZA-L10-H09"| 
        LF$hh_refno =="TZA-L18-H29"| LF$hh_refno=="TZA-L19-H11"| LF$hh_refno  =="TZA-L19-H19"),]
colnames(LF)
View(LF)
dim(LF)
#Integrating landscape and eplot number to dataframe
LF<-LF%>%left_join(VS_MergingFile,by="hh_refno")
colnames(LF)
LF<-LF[c(1,46,47,48,49,2:45)]
#----Missing Data Analysis-------------
md_lfi<-as.data.frame(colSums(is.na(LF[,-c(1)])))
md_lfi
#LCI sheet 
write.csv(LF,'./8_Analyses/Datasets/HH_CA.csv',row.names = FALSE)



