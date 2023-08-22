#####################################################
#Laikipia Livelihoods capital indicators selection ##
                                                  ##
####################################################

#Dataset                                                                       
getwd()
#Data Sheets
TNCCETRAD<-read.csv("./data/tnc_cetrad_cp.csv")
#View(TNCCETRAD)
rainfall<-read.csv("./data/HH_rainfall.csv")
treecover<-read.csv("./data/HH_treecover.csv")
elevation<-read.csv("./data/HH_elevation.csv")
clay<-read.csv("./data/HH_clay_percent.csv")
colnames(TNCCETRAD)
dim(TNCCETRAD)
dim(rainfall)
dim(elevation)
dim(treecover)
dim(clay)
#Connecting PC dataframes
library(tidyverse)
TNCCETRAD<-TNCCETRAD%>%left_join(rainfall,by='HH_ID_Number')%>%left_join(clay,
            by='HH_ID_Number')%>%left_join(treecover,by='HH_ID_Number')
#View(TNCCETRAD)
##----NC Variables---
colnames(TNCCETRAD)
NC<-TNCCETRAD[c("HH_ID_Number","Q15_BorderingStreamorRiver","Treecover_percent",
                "Q92_1_NumberTreeSpecies","Rainfall","clay_percent")]
mean(NC$Soil)
sd(NC$Soil)
NC<-as.data.frame(NC)
#Transforming variables to likert scale
NC$Q15_BorderingStreamorRiver<-ifelse(NC$Q15_BorderingStreamorRiver== 1,2,
                                      ifelse(NC$Q15_BorderingStreamorRiver== 2,1,1))
colnames(NC)
names(NC)[1]<-'HH_ID_Number'
names(NC)[2]<-'Freshwater_access'
names(NC)[3]<-'Tree_cover'
names(NC)[4]<-'Tree_diversity'
names(NC)[5]<-'Rainfall'
names(NC)[6]<-'Soil'
NC

##-------HC Variables-------
colnames(TNCCETRAD)
HC<-TNCCETRAD[c("HH_ID_Number","H0_HHNumbers","H3_Gender0", "H4_Age0","H10_LevelOfSchooling0",         
                     "Q55_HiringOfLabour",
                     "Q101_AgrProjPert_Train")]
HC<-as.data.frame(HC)
HC
#Transforming variables to likert scale
#gender
HC$H3_Gender0<-ifelse(HC$H3_Gender0 == 1,2,
                      ifelse(HC$H3_Gender0 == 2,1,'NA'))   
                    
#level of schooling
HC$H10_LevelOfSchooling0<-ifelse(HC$H10_LevelOfSchooling0 == 7 & HC$H10_LevelOfSchooling0 == 8 & HC$H10_LevelOfSchooling0 == 99,1,
                                 ifelse(HC$H10_LevelOfSchooling0 == 1 ,2,
                                        ifelse(HC$H10_LevelOfSchooling0 == 2 ,3,
                                               ifelse(HC$H10_LevelOfSchooling0 == 3 ,4,
                                                      ifelse(HC$H10_LevelOfSchooling0 == 4 ,5,
                                                             ifelse(HC$H10_LevelOfSchooling0 == 5 ,6,
                                                                    ifelse(HC$H10_LevelOfSchooling0 == 6 ,6,1)))))))
#Household labor
HC$Q55_HiringOfLabour<-ifelse(HC$Q55_HiringOfLabour == 2,1,
                              ifelse(HC$Q55_HiringOfLabour == 2,2,0))

colnames(HC)
HC<-HC[c("HH_ID_Number","H0_HHNumbers","H3_Gender0","H4_Age0","H10_LevelOfSchooling0","Q55_HiringOfLabour",    
         "Q101_AgrProjPert_Train")]
HC
names(HC)[1]<-'HH_ID_Number'
names(HC)[2]<-'HH_members'
names(HC)[3]<-'Head_gender'
names(HC)[4]<-'Head_age'
names(HC)[5]<-'Head_education'
names(HC)[6]<-'Labor_hire'
names(HC)[7]<-'Ag_trainings'
HC
#---Social CA Variables-------------
colnames(TNCCETRAD)
SC<-TNCCETRAD[c("HH_ID_Number","Q98_AgrInfoAccess__3","Q98_AgrInfoAccess__7","Q127_MarketLinkSupp")]
SC<-as.data.frame(SC)
SC$Info_network<-ifelse(SC$Q98_AgrInfoAccess__3 == 0 & SC$Q98_AgrInfoAccess__7 == 0, 1,
                           ifelse(SC$Q98_AgrInfoAccess__3 == 1 & SC$Q98_AgrInfoAccess__7 == 0, 2,
                                  ifelse(SC$Q98_AgrInfoAccess__3 == 0 & SC$Q98_AgrInfoAccess__7 == 1, 2,
                                         ifelse(SC$Q98_AgrInfoAccess__3 == 1 & SC$Q98_AgrInfoAccess__7 == 1, 3,'NA'))))

SC$Market_network<-ifelse(SC$Q127_MarketLinkSupp == 1,2,
                               ifelse(SC$Q127_MarketLinkSupp == 2,1,"NA"))


                           
SC<-SC[c("HH_ID_Number","Info_network","Market_network")]
View(SC)

#Physical CA Variables
colnames(TNCCETRAD)
PC<-TNCCETRAD[c("HH_ID_Number","Q52_EquipmentOwned__1","Q52_EquipmentOwned__2", "Q52_EquipmentOwned__3","Q52_EquipmentOwned__4",        
                        "Q52_EquipmentOwned__5","Q52_EquipmentOwned__6","Q52_EquipmentOwned__7",        
                        "Q52_EquipmentOwned__8", "Q53_EquipmentLeased__1","Q53_EquipmentLeased__2","Q53_EquipmentLeased__3","Q53_EquipmentLeased__4",          
                        "Q53_EquipmentLeased__5","Q53_EquipmentLeased__6",  "Q98_AgrInfoAccess__2" ,"Q98_AgrInfoAccess__10",
                        "Q98_AgrInfoAccess__12","Q98_AgrInfoAccess__13",
                        "Q53_EquipmentLeased__7","Q53_EquipmentLeased__8", "Q7_IrrigatedLandArea","Q123_TransportMode",
                          "Q122_RoadState")]
PC<-as.data.frame(PC)
colnames(PC)
PC
PC$Comm_infra<-ifelse(PC$Q98_AgrInfoAccess__13 == 0 & PC$Q98_AgrInfoAccess__12 == 0 & PC$Q98_AgrInfoAccess__2 == 0 &PC$Q98_AgrInfoAccess__10 == 0,1,
              ifelse(PC$Q98_AgrInfoAccess__13 >= 0 & PC$Q98_AgrInfoAccess__12 >= 0 & PC$Q98_AgrInfoAccess__2 == 0 &PC$Q98_AgrInfoAccess__10 == 0,2,
            ifelse(PC$Q98_AgrInfoAccess__13 >= 0 & PC$Q98_AgrInfoAccess__12 >=0  & PC$Q98_AgrInfoAccess__2 == 1 &PC$Q98_AgrInfoAccess__10 == 0,3,
            ifelse(PC$Q98_AgrInfoAccess__13 >= 0 & PC$Q98_AgrInfoAccess__12 >=0  & PC$Q98_AgrInfoAccess__2 >=0  &PC$Q98_AgrInfoAccess__10 == 1,4,'NA'))))

PC
PC$Implements<-ifelse(PC$Q52_EquipmentOwned__1 ==0 & PC$Q52_EquipmentOwned__2 ==0 & PC$Q52_EquipmentOwned__3 ==0 & PC$Q52_EquipmentOwned__4 ==0
                         & PC$Q52_EquipmentOwned__5 ==0 & PC$Q52_EquipmentOwned__6 ==0 & PC$Q52_EquipmentOwned__7 ==0 & PC$Q52_EquipmentOwned__8 ==0 & 
                         PC$Q53_EquipmentLeased__1==0 & PC$Q53_EquipmentLeased__2 ==0 & PC$Q53_EquipmentLeased__3==0 & PC$Q53_EquipmentLeased__4==0 &
                           PC$Q53_EquipmentLeased__5==0 & PC$Q53_EquipmentLeased__6==0 & PC$Q53_EquipmentLeased__7==0 & PC$Q53_EquipmentLeased__8==0,1,
                         ifelse(PC$Q52_EquipmentOwned__1 ==0 & PC$Q52_EquipmentOwned__2 ==0 & PC$Q52_EquipmentOwned__3 ==0 & PC$Q52_EquipmentOwned__4 ==0 &
                          PC$Q52_EquipmentOwned__5 ==0 & PC$Q52_EquipmentOwned__6 ==0 & PC$Q52_EquipmentOwned__7 ==0 & PC$Q52_EquipmentOwned__8 ==0 & 
                          PC$Q53_EquipmentLeased__1>0 | PC$Q53_EquipmentLeased__2 >0 | PC$Q53_EquipmentLeased__3>0 | PC$Q53_EquipmentLeased__4>0 &
                          PC$Q53_EquipmentLeased__5>0 | PC$Q53_EquipmentLeased__6>0 | PC$Q53_EquipmentLeased__7>0 | PC$Q53_EquipmentLeased__8>0, 2,
                          ifelse(PC$Q52_EquipmentOwned__1 >0 | PC$Q52_EquipmentOwned__2 >0 | PC$Q52_EquipmentOwned__3 >0 | PC$Q52_EquipmentOwned__4 >0 &
                          PC$Q52_EquipmentOwned__5 >0 & PC$Q52_EquipmentOwned__6 >0 & PC$Q52_EquipmentOwned__7 >0 & PC$Q52_EquipmentOwned__8 >0 & 
                          PC$Q53_EquipmentLeased__1==0 & PC$Q53_EquipmentLeased__2 ==0 & PC$Q53_EquipmentLeased__3==0 & PC$Q53_EquipmentLeased__4==0 &
                          PC$Q53_EquipmentLeased__5==0 & PC$Q53_EquipmentLeased__6==0 & PC$Q53_EquipmentLeased__7==0 & PC$Q53_EquipmentLeased__8==0, 3,
                          ifelse(PC$Q52_EquipmentOwned__1 >0 | PC$Q52_EquipmentOwned__2 >0 | PC$Q52_EquipmentOwned__3 >0 | PC$Q52_EquipmentOwned__4 >0 &
                          PC$Q52_EquipmentOwned__5 >0 | PC$Q52_EquipmentOwned__6 >0 | PC$Q52_EquipmentOwned__7 > 0 | PC$Q52_EquipmentOwned__8 > 0 | 
                          PC$Q53_EquipmentLeased__1>0 | PC$Q53_EquipmentLeased__2 >0 | PC$Q53_EquipmentLeased__3>0 | PC$Q53_EquipmentLeased__4> 0 |
                          PC$Q53_EquipmentLeased__5>0 | PC$Q53_EquipmentLeased__6>0 | PC$Q53_EquipmentLeased__7>0 | PC$Q53_EquipmentLeased__8 > 0, 4,"NA"))))


PC$Transport_mode<-ifelse(PC$Q123_TransportMode == 1,1,
                              ifelse(PC$Q123_TransportMode == 4,2,
                                     ifelse(PC$Q123_TransportMode == 2,3,
                                            ifelse(PC$Q123_TransportMode == 3,4,'NA'))))

PC$Road_state<-ifelse(PC$Q122_RoadState == 4, 1,
                          ifelse(PC$Q122_RoadState == 3, 2,
                                 ifelse(PC$Q122_RoadState == 2, 3,
                                        ifelse(PC$Q122_RoadState == 1, 4,"NA"))))



colnames(PC) 
PC<-PC[c("HH_ID_Number", "Implements","Comm_infra","Transport_mode","Road_state")]       
PC

#Financial CA Variables
colnames(TNCCETRAD)
FC<-TNCCETRAD[c("HH_ID_Number","Q121B_MarketDistance","Q122_RoadState","Q120_SalesPoints__1",              
                         "Q120_SalesPoints__2","Q120_SalesPoints__3","Q120_SalesPoints__4", "Q120_SalesPoints__5",            
                         "Q120_SalesPoints__6","Q120_SalesPoints__7","Q4_NumberLandParcels","Q5_FreeHoldLandAreaOwned",
                         "Q80_MainLVNumbers1","Q80_MainLVNumbers2","Q80_MainLVNumbers3","Q80_MainLVNumbers4",             
                         "Q80_MainLVNumbers5","Q80_MainLVNumbers6","Q80_MainLVNumbers7","Q80_MainLVNumbers8",             
                          "Q80_MainLVNumbers9","Q80_MainLVNumbers12","Q80_MainLVNumbers13","Q80_MainLVNumbers14",
                            "Q17_LandTenurePerPlot0", "Q167_AbilityToBorrow", "Q168_DeptStatus","Q5_FreeHoldLandAreaOwned",
                              "Q129_WallMaterial","Q130_RoofMaterial","Q131_FloorMaterial","Q132_LightingSource",
                                 "Q133_CookingFuel","Q134_HeatingFuel", "Q135_ToiletFacility")]
FC<-as.data.frame(FC)
colnames(FC)

# Housing infrastructure
#walls
list1 <- as.list(unique(PC$Q129_WallMaterial))
list1
oldvals <- c(1,12,7,4,5,9,10,2,3,11,6)
newvals <- c(1,4,5,3,5,1,5,2,5,4,5)
FC$house_walls_n<-newvals[match(FC$Q129_WallMaterial, oldvals)]
FC
#Roof
list2<- as.list(unique(PC$Q130_RoofMaterial))
list2
oldvals <- c(1,9,2,8)
newvals <- c(3,2,1,2)
FC$house_roof_n<-newvals[match(FC$Q130_RoofMaterial, oldvals)]
FC
#Floor
list3<- as.list(unique(FC$Q131_FloorMaterial))
list3
oldvals <- c(3,2,1,6,4)
newvals <- c(1,3,5,4,2)
FC$house_floor_n<-newvals[match(FC$Q131_FloorMaterial, oldvals)]
FC
#Sanitation
list4<- as.list(unique(FC$Q135_ToiletFacility))
list4
oldvals <- c(8,1,2,3,4,5,6)
newvals <- c(1,1,2,3,3,4,5)
FC$house_san_n<-newvals[match(FC$Q135_ToiletFacility, oldvals)]
FC
#Lighting
list5<- as.list(unique(FC$Q132_LightingSource))
list5
oldvals <- c(1,2,4,5,3,10,6,9,8)
newvals <- c(3,6,7,7,5,4,8,1,2)
FC$house_light_n<-newvals[match(FC$Q132_LightingSource, oldvals)]
FC
#Cooking 
list6<- as.list(unique(FC$Q133_CookingFuel))
list6
oldvals <- c(1,5,2,4,8,6)
newvals <- c(1,3,2,4,6,5)
FC$house_cooking_n<-newvals[match(FC$Q133_CookingFuel, oldvals)]
FC


FC$Land_holding<-ifelse(FC$Q5_FreeHoldLandAreaOwned < 5, 1,
                        ifelse(FC$Q5_FreeHoldLandAreaOwned >= 5 & FC$Q5_FreeHoldLandAreaOwned  < 10,2,
                               ifelse(FC$Q5_FreeHoldLandAreaOwned >=10,3,'NA')))

#Land tenure
list7<- as.list(unique(FC$Q17_LandTenurePerPlot0))
list7
oldvals <- c(11,6,3,5,4,12,"NA",7)
newvals <- c(5,2,1,3,4,2,"NA",1)
FC$Land_tenure<-newvals[match(FC$Q17_LandTenurePerPlot0, oldvals)]
FC

FC$Crop_salepoint<-ifelse(FC$Q120_SalesPoints__7 == 1,1,
                          ifelse(FC$Q120_SalesPoints__6 ==1,2,
                                 ifelse(FC$Q120_SalesPoints__5 ==1,2,
                                        ifelse(FC$Q120_SalesPoints__4 ==1,3,
                                               ifelse(FC$Q120_SalesPoints__1 ==1,4,
                                                      ifelse(FC$Q120_SalesPoints__2 ==1,5,
                                                             ifelse(FC$Q120_SalesPoints__3 ==1,6,'NA')))))))


FC$Livestock_holding<-rowSums(FC[c("Q80_MainLVNumbers1","Q80_MainLVNumbers2", "Q80_MainLVNumbers3",
                                   "Q80_MainLVNumbers4","Q80_MainLVNumbers5","Q80_MainLVNumbers6",
                                   "Q80_MainLVNumbers7","Q80_MainLVNumbers8","Q80_MainLVNumbers9",
                                   "Q80_MainLVNumbers12","Q80_MainLVNumbers13","Q80_MainLVNumbers14")],na.rm=TRUE)

FC$Loan_access<-ifelse(FC$Q167_AbilityToBorrow == 1, 1,
                          ifelse(FC$Q167_AbilityToBorrow == 2,2,
                           ifelse(FC$Q167_AbilityToBorrow == 3,3,
                           ifelse(FC$Q167_AbilityToBorrow == 4, 4,'NA'))))

FC$Debt_status<-ifelse(FC$Q168_DeptStatus ==4,1,
                    ifelse(FC$Q168_DeptStatus ==3,2,
                     ifelse(FC$Q168_DeptStatus ==2,3,
                     ifelse(FC$Q168_DeptStatus == 1,4,'NA'))))
colnames(FC)
FC<-FC[c("HH_ID_Number","Livestock_holding","house_walls_n","house_roof_n","house_floor_n","house_san_n","house_light_n",             
         "house_cooking_n","Land_holding","Land_tenure","Crop_salepoint","Loan_access",
         "Debt_status")]
FC

# Combining all selected CA variables & cleaning the datasheet 
LCI<-(cbind(NC,HC[,-1],SC[,-1],PC[,-1],FC[,-1]))
LCI<-as.data.frame(LCI)
LCI[ LCI == "#N/A"] <- NA
LCI[ LCI == "##N/A##"] <- NA
LCI[ LCI == "NA"] <- NA
LCI[ LCI == " "] <- NA
LCI[ LCI == "-999999999"] <- NA
LCI[ LCI == "99"] <- NA
LCI[] <- lapply(LCI, function(x) as.numeric(as.character(x)))
colnames(LCI)

#------Missing data analysis-------
as.data.frame(colSums(is.na(LCI)))
#** LCI Nearest neighbor imputation**
as.data.frame(colSums(is.na(LCI[,-c(1)])))
#View(LCI)
#---KNN Imputation----------
#*#Since KNN calculates the Euclidean distance between points to 
#*determine the nearest neighboring points, it’s important that all of the data be on the same scale.
#*k =sqrt(#var)
#install.packages("bnstruct")
library(bnstruct)
colnames(LCI)
imputed_LCI<-c()
imputed_LCI[]<- lapply(LCI, function(x) as.numeric(as.numeric(x)))
imputed_LCI<-as.matrix(LCI)
str(imputed_LCI)
imputed_LCI<-knn.impute(
  imputed_LCI[,-1],
  k = 22,
  #cat.var = 1:ncol(imputed_FC[,-1]),
  to.impute = 1:nrow(imputed_LCI),
  using = 1:nrow(imputed_LCI[,-1])
)
imputed_LCI<-as.data.frame(imputed_LCI)
imputed_LCI<-cbind(LCI$HH_ID_Number,imputed_LCI)
names(imputed_LCI)[1]<-'HH_ID_Number'
View(imputed_LCI)
as.data.frame(colSums(is.na(imputed_LCI[,-c(1)])))
#Outputs
write.csv(LCI,"./data//HH_LCI_KE.csv", row.names = FALSE)
write.csv(imputed_LCI,"./data/HH_imp_LCI_KE.csv", row.names = FALSE)
