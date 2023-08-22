#####################################################
#Laikipia Agricultural management indicators       ##
##                                                 ##
####################################################

#Dataset                                                                       
getwd()
#Data Sheets
TNCCETRAD<-read.csv("./data/tnc_cetrad_cp.csv")
colnames(TNCCETRAD)


#--CIF-------
#combined impact factor
colnames(TNCCETRAD)

#cultivated land
cult_area<-TNCCETRAD[c("HH_ID_Number","Q6_CultivatedLandAreaOwned","Q8_CultivatedLandAreaLeasOrRent","Q9_IrrigatedLandAreaLeaseOrRent",
                       "Q8_0_RentLeasedLand")]
cult_area$total_cult_area<-rowSums(cult_area[c("Q6_CultivatedLandAreaOwned","Q8_CultivatedLandAreaLeasOrRent","Q9_IrrigatedLandAreaLeaseOrRent",
                                               "Q8_0_RentLeasedLand")])
cult_area
cult_area<-cult_area[c("HH_ID_Number","total_cult_area")]
cult_area$cult_land<-ifelse(cult_area$total_cult_area > 0,1,0)
cult_area

#Agricultural land total
ag_land<-TNCCETRAD[c("HH_ID_Number","Q5_FreeHoldLandAreaOwned")]
ag_land

#mechanized tools #tractors
mechtools<-TNCCETRAD[c("HH_ID_Number","Q52_EquipmentOwned__1" ,"Q52_EquipmentOwned__4","Q53_EquipmentLeased__1","Q53_EquipmentLeased__4")]
mechtools$mech_tools<-ifelse(mechtools$Q52_EquipmentOwned__1 == 0 & mechtools$Q52_EquipmentOwned__1 == 0 & mechtools$Q52_EquipmentOwned__1 == 0 &
                               mechtools$Q52_EquipmentOwned__1 == 0, 0,
                             ifelse(mechtools$Q52_EquipmentOwned__1 >= 0 | mechtools$Q52_EquipmentOwned__1 >= 0 | mechtools$Q52_EquipmentOwned__1 >= 0 |
                                      mechtools$Q52_EquipmentOwned__1 >= 0, 1,'NA'))
mechtools

irr<-TNCCETRAD[c("HH_ID_Number","q22_Irrigation0")]
irr$irrigation<-ifelse(irr$q22_Irrigation0 ==2,0,
                            ifelse(irr$q22_Irrigation0 ==1, 1,'NA'))
irr


#Improved seeds used
certseeds<-TNCCETRAD[c("HH_ID_Number","Q73_CertifiedSeedPurchase")]
certseeds$cert_seeds<-ifelse(certseeds$Q73_CertifiedSeedPurchase == 1, 1,
                             ifelse(certseeds$Q73_CertifiedSeedPurchase == 2, 0, "NA"))
certseeds

#Fertilizer use 
fertuse<-TNCCETRAD[c("HH_ID_Number","Q49_CropsFertilizerNo")]
fertuse$fert_use<-ifelse(fertuse$Q49_CropsFertilizerNo == 0,0,
                  ifelse(fertuse$Q49_CropsFertilizerNo > 0,1,'NA'))
fertuse 

#Fertilizer rate
fertapp<-TNCCETRAD[c("HH_ID_Number","Q49_CropsFertilizerNo","Q49_2_FertilizApplied0","Q51_RateOfFertApplication0",
                     "Q51_1_UnitsOfRateFert0", "Q49_3_AmendmApplied0","Q51B_RateAmendApplic0","Q51B_1_UnitsOfRateAmend0")]
View(fertapp)

#Pesticide application
pestapp<-TNCCETRAD[c("HH_ID_Number","Q66_PesticideApplication")]
pestapp
pestapp$pest_app<-ifelse(pestapp$Q66_PesticideApplication ==2,0,
                         ifelse(pestapp$Q66_PesticideApplication ==1, 1,'NA'))
pestapp[is.na(pestapp)]<-0
pestapp[ pestapp == 'NA'] <- 0
pestapp
#Merging of Input Intensity variables
library(dplyr)
Inputs<-cult_area%>%left_join(irr, by='HH_ID_Number')%>%left_join(mechtools, by='HH_ID_Number')%>%left_join(fertuse,
        by='HH_ID_Number')%>%left_join(certseeds, by='HH_ID_Number')%>%left_join(pestapp, by='HH_ID_Number')
Inputs<-Inputs[!duplicated(Inputs$HH_ID_Number),]
Inputs[is.na(Inputs)]<-0
colnames(Inputs)
Inputs<-Inputs[c("HH_ID_Number","fert_use","pest_app","cert_seeds","mech_tools", "irrigation")]
Inputs

#Unique combinations of inputs --trying a new order 
#this is based on impact on their relative impact on productivity as in vs approach ** discuss with Cheryl
library(dplyr)
Inputs_combination<-Inputs%>%group_by(fert_use,pest_app,cert_seeds,mech_tools,irrigation) %>%
  mutate(Inputs_index = cur_group_id()) %>%ungroup() %>%arrange(Inputs_index)
View(Inputs_combination)
#Making Input Intensity binary
# is is li, 2 is mi, 3 is hi 
Inputs_combination$InputImpact_3class<-ifelse(Inputs_combination$Inputs_index ==1 ,0,
                                         ifelse(Inputs_combination$Inputs_index >= 2 & Inputs_combination$Inputs_index < 4,1,
                                          ifelse(Inputs_combination$Inputs_index >= 4,3,'NA')))
Inputs_combination
colnames(Inputs_combination)
dim(Inputs_combination)
#CIF
sum_cif<-c()
sum_cif<-Inputs_combination%>%count(Inputs_index)
sum_cif<-as.data.frame(sum_cif)
sum_cif$percent<-sum_cif$n/500*100
sum_cif<-as.data.frame(sum_cif)
sum_cif

# ---CA based practices-------------------------
# the method/package used in r is the group indeces package 
colnames(TNCCETRAD)

#minimum tillage
mintill<-TNCCETRAD[c("HH_ID_Number","Q57_FieldPreparation__3","Q57_FieldPreparation__4","Q57_FieldPreparation__5")]
mintill$min_till<-ifelse(mintill$Q57_FieldPreparation__3 == 0 & mintill$Q57_FieldPreparation__4 == 0 & mintill$Q57_FieldPreparation__5 == 0,0,
                  ifelse(mintill$Q57_FieldPreparation__3 == 1 | mintill$Q57_FieldPreparation__4 == 1 | mintill$Q57_FieldPreparation__5 == 1,1,'NA'))
mintill$soil_till<-ifelse(mintill$min_till==1,0,
                          ifelse(mintill$min_till== 0,1,""))

#Residue retention
resret<-TNCCETRAD[c("HH_ID_Number","Q77_ResidueManage__1")]
resret[is.na(resret)]<-0
resret$res_ret<-ifelse(resret$Q77_ResidueManage__1 == 0,0,
                       ifelse(resret$Q77_ResidueManage__1 == 1,1,'NA'))
resret$no_res_ret<-ifelse(resret$res_ret == 0,1,
                   ifelse(resret$res_ret == 1,0,""))      

#Intercropping 
intercrop<-TNCCETRAD[c("HH_ID_Number","Q33_ExistenceOfInterCropLong")]
intercrop[is.na(intercrop)]<-0
intercrop$intercrop<-ifelse(intercrop$Q33_ExistenceOfInterCropLong == 2,0,
                      ifelse(intercrop$Q33_ExistenceOfInterCropLong == 1,1,0))
intercrop

#Merging of CA based practices variables
library(dplyr)
CA_practices<-mechtools%>%left_join(mintill,by='HH_ID_Number')%>%left_join(resret, 
                by='HH_ID_Number')
CA_practices[is.na(CA_practices)]<-0
colnames(CA_practices)
CA_practices<-CA_practices[c("HH_ID_Number","soil_till","no_res_ret")]
CA_practices

#Unique combinations of CA practices
library(dplyr)
CA_practices_combination<-CA_practices %>%group_by(soil_till,no_res_ret) %>%
  mutate(Mgmt_index = cur_group_id()) %>%ungroup() %>%arrange(Mgmt_index)
#View(CA_practices_combination)
#Making management combinations binary
#2 is sm, 1 is cm
CA_practices_combination$Mgmt_class<-ifelse(CA_practices_combination$Mgmt_index > 2,1,0)
View(CA_practices_combination)
colnames(CA_practices_combination)
#CPF
sum_cpf<-c()
sum_cpf<-CA_practices_combination%>%count(Mgmt_index)
sum_cpf<-as.data.frame(sum_cpf)
sum_cpf$percent<-sum_cpf$n/500*100
sum_cpf<-as.data.frame(sum_cpf)
sum_cpf


#---Production orientation ------
colnames(TNCCETRAD)
prodor<-TNCCETRAD[c( "HH_ID_Number","Q29_LongRainCrop","Q35_ShortRainCrop")]
table(prodor$Q29_LongRainCrop)
oldvals<-c(1,2,3,4,5,7,8,9,10,11,15,21,30,34)
newvals<-c("Maize","Beans","Peas","Potatoes","Tomatoes","Cowpeas","Cabbages","Kale","Onions","Sweet_potatoes","Carrots","Wheat","Rhode_grass",
           "French_beans","Other")
prodor$prodor_longrain_crop<-newvals[match(prodor$Q29_LongRainCrop, oldvals)]
prodor
# table(prodor$Q35_ShortRainCrop)
# oldvals<-c(1,2,3,4,5,7,8,9,10,17,21,30,51)
# newvals<-c("Maize","Beans","Peas","Potatoes","Tomatoes","Cowpeas","Cabbages","Kale","Onions","Sweet_potatoes","Cotton","Wheat","Rhode_grass",
#            "Other")
# prodor$prodor_shortrain_crop<-newvals[match(prodor$Q35_ShortRainCrop, oldvals)]
# prodor

# production orientation classification 
# in the end, classify based on cereal vs legume/vegetables orientation 
# non-cereal is 1, cereal is 2
oldvals<-c("Maize","Beans","Peas","Potatoes","Tomatoes","Cowpeas","Cabbages","Kale","Onions","Sweet_potatoes","Carrots","Wheat","Rhode_grass",
           "French_beans","Other")
newvals<-c(2,1,1,1,1,1,1,1,1,1,1,2,1,1,1)
prodor$prodor_longrain_crop_class<-newvals[match(prodor$prodor_longrain_crop, oldvals)]
prodor

#----Merged Management data sets 
IS<-Inputs_combination%>%left_join(CA_practices_combination, by='HH_ID_Number')
colnames(IS)
AIS<-IS[c( "HH_ID_Number","InputImpact_3class","Mgmt_class")]
AIS
as.data.frame(colSums(is.na(AIS[,-c(1)])))

# #*KNN imputation
# #install.packages("bnstruct")
# library(bnstruct)
# colnames(IS)
# imputed_IS<-c()
# imputed_IS[]<- lapply(IS, function(x) as.numeric(as.numeric(x)))
# imputed_IS<-as.matrix(IS)
# str(imputed_IS)
# imputed_IS<-knn.impute(
#   imputed_IS[,-1],
#   k = 1,
#   #cat.var = 1:ncol(imputed_FC[,-1]),
#   to.impute = 1:nrow(imputed_IS),
#   using = 1:nrow(imputed_IS[,-1])
# )
# imputed_IS<-as.data.frame(imputed_IS)
# imputed_IS
# imputed_IS<-cbind(IS$HH_ID_Number,imputed_IS)
# imputed_IS
# names(imputed_IS)[1]<-'HH_ID_Number'
# as.data.frame(colSums(is.na(imputed_IS[,-c(1)])))
#-- Intensification strategy -----------------
library(dplyr)
AIS<-IS%>%group_by(InputImpact_3class,Mgmt_class) %>%
  mutate(AIS = cur_group_id()) %>%ungroup() %>%arrange(AIS)
AIS
View(AIS)
#--AII---------------
colnames(Inputs_combination)
CIF<-Inputs_combination[c("HH_ID_Number","Inputs_index")]
names(CIF)[2]<-'CIF'
CIF
#Crop intensity
colnames(TNCCETRAD)
CI<-TNCCETRAD[c("HH_ID_Number","Q29_LongRainCrop", "Q35_ShortRainCrop", "Q42_TheOtherCrop")]
CI
CI_long<- aggregate(data = CI,Q29_LongRainCrop ~ HH_ID_Number,function(x) length(unique(x)))
names(CI_long)[2] <- 'CI_long'
CI_long
CI_short<- aggregate(data = CI,Q35_ShortRainCrop ~ HH_ID_Number,function(x) length(unique(x)))
names(CI_short)[2] <- 'CI_short'
CI_other<- aggregate(data = CI,Q42_TheOtherCrop ~ HH_ID_Number,function(x) length(unique(x)))
names(CI_other)[2] <- 'CI_other'
CI<-CI_long%>%left_join(CI_short, by="HH_ID_Number")%>%left_join(CI_other, by="HH_ID_Number")
CI$CI<-rowSums(CI[c("CI_long","CI_short","CI_other")],na.rm=T)
CI<-CI[c("HH_ID_Number","CI")]
CI
AII<-CIF%>%left_join(CI, by="HH_ID_Number")%>%left_join(cult_area, by="HH_ID_Number")%>%left_join(ag_land, 
            by="HH_ID_Number")%>%left_join(AIS, by="HH_ID_Number")
AII[is.na(AII)]<-0
names(AII)[6]<-'total_ag_land'
colnames(AII)
#multiplying combined impact factor by crop intensity
AII$CIIF<-AII$CIF*(AII$CI)
#AII calculations - calculating the agricultural impact at the field level
#Fixing land area size data
AII$fixed_cult_area<-ifelse(AII$total_cult_area >= AII$total_ag_land,AII$total_ag_land,AII$total_cult_area)
AII$fixed_ag_land<-ifelse(AII$total_ag_land == 0,AII$total_cult_area,AII$total_ag_land)
AII$AII<-(AII$CIIF)*(AII$fixed_cult_area)/(AII$fixed_ag_land)
View(AII)
hist(AII$AII)
colnames(AII)
AII<-AII[c("HH_ID_Number","fert_use","cert_seeds","mech_tools","irrigation","soil_till","no_res_ret","CIF",
           "Mgmt_index","AII")]
names(AII)[9]<-'CPF'
View(AII)
dim(AII)
#KNN imputation
library(bnstruct)
colnames(AII)
imputed_AII<-c()
imputed_AII[]<- lapply(AII, function(x) as.numeric(as.numeric(x)))
imputed_AII<-as.matrix(AII)
str(imputed_AII)
imputed_AII<-knn.impute(
  imputed_AII[,-1],
  k = 22,
  #cat.var = 1:ncol(imputed_FC[,-1]),
  to.impute = 1:nrow(imputed_AII),
  using = 1:nrow(imputed_AII[,-1])
)
imputed_AII<-as.data.frame(imputed_AII)
imputed_AII<-cbind(AII$HH_ID_Number,imputed_AII)
names(imputed_AII)[1]<-'HH_ID_Number'
View(imputed_AII)
as.data.frame(colSums(is.na(imputed_AII[,-c(1)])))
#Outputs
write.csv(imputed_AII,"./data/HH_AII_KE.csv", row.names = FALSE)
