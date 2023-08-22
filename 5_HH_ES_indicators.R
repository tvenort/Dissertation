#Dataset                                                                       
getwd()
#calling soil properties data
soils<-read.csv("./data/HH_Soil_properties.csv")
colnames(soils)
#View(soil_prop)
#--Setups-------
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
  y<-1-((abs(x-7) - abs(7-min(x)))/(abs(max(x)-7) -abs(7-min(x))))
  return(y)
}
# opt_bd<-function(x){
#   y<-abs(1-(abs(x-130) - abs(min(x)-130))/(abs(max(x)-130) -abs(min(x)-130)))
#   return(y)
# }

opt_bd<-function(x){
  y<-1-((abs(x-130) - abs(130-min(x)))/(130-abs(max(x)) -abs(130-min(x))))
  return(y)
}

colnames(soils)
#View(soils)
#conversion from mg/kg*or ppm to cmol/kg for Ca, Mg,K, 
#P, S stays in mg/kg
soils$Calcium_extractable<-soils$Calcium_extractable/200
soils$Magnesium<-soils$Magnesium/120
soils$Potassium<-soils$Potassium/390
mean(soils$Calcium_extractable)
sd(soils$Calcium_extractable)
mean(soils$Magnesium)
sd(soils$Magnesium)
mean(soils$Potassium)
sd(soils$Potassium)
mean(soils$Phosphorous)
sd(soils$Phosphorous)
mean(soils$Sulfur)
sd(soils$Sulfur)
mean(soils$Total_Nitrogen)
sd(soils$Total_Nitrogen)
mean(soils$E_CEC)
sd(soils$E_CEC)
mean(soils$pH)
sd(soils$pH)
mean(soils$Depth_to_bedrock)
sd(soils$Depth_to_bedrock)
mean(soils$Bulk_density)
sd(soils$Bulk_density)
mean(soils$Clay_content)
sd(soils$Clay_content)
mean(soils$Silt_content)
sd(soils$Silt_content)
#Indicators calculation
soils$Ca_indicator<-ifelse(soils$Calcium_extractable < 2, 0,
                           ifelse(soils$Calcium_extractable >=2 &soils$Calcium_extractable <=7,
                                  soils$Calcium_extractable*0.167/7,
                                  ifelse(soils$Calcium_extractable>7,0.167,"")))
soils$Mg_indicator<-ifelse(soils$Magnesium < 2, 0,
                           ifelse(soils$Magnesium >=2 &soils$Magnesium <=7,
                                  soils$Magnesium*0.167/7,
                                  ifelse(soils$Magnesium>7,0.167,"")))
soils$K_indicator<-ifelse(soils$Potassium < 0.2, 0,
                           ifelse(soils$Potassium >=0.2 &soils$Potassium <=0.7,
                                  soils$Potassium*0.167/7,
                                  ifelse(soils$Potassium>0.7,0.167,"")))
soils$P_indicator<-ifelse(soils$Phosphorous < 5, 0,
                          ifelse(soils$Phosphorous >=5 &soils$Phosphorous <=30,
                                 soils$Phosphorous*0.167/30,
                           ifelse(soils$Phosphorous > 30,0.167,"")))
soils$P_indicator<-as.numeric(soils$P_indicator)
soils$S_indicator<-ifelse(soils$Sulfur < 5, 0,
                          ifelse(soils$Sulfur >=5 &soils$Sulfur <=15,
                                 soils$Sulfur*0.167/15,
                                 ifelse(soils$Sulfur > 15,0.167,"")))
soils$soil_acidity<-ifelse(soils$pH <= 4.5,0,
                   ifelse(soils$pH >=4.5 &soils$pH <=5.5,
                                soils$pH*0.167/5.5,
                                ifelse(soils$pH >5.5,0.167,"")))
soils$soil_acidity<-as.numeric(soils$soil_acidity)
soils$soil_fertility_indicator<-rowSums(soils[c( "Ca_indicator","Mg_indicator",
                                                 "K_indicator","P_indicator",
                                                 "S_indicator","soil_acidity")])

soils$TIC<-soils$Carbon_total-soils$Organic.Carbon
soils$TIC
mean(soils$TIC)
sd(soils$TIC)
soils$C_Capacity<-(soils$TIC/(exp(1.33+0.00994*soils$Clay_content+0.00699*soils$Silt_content-
                                   0.156*(0.923*soils$pH-0.6)))*((10/7)^(-0.58)))*100
soils$C_Capacity
soils$C_deficit_indicator<-ifelse(soils$C_Capacity < 50, 0,
                                  ifelse(soils$C_Capacity >=50 &soils$C_Capacity <=80,
                                         soils$C_Capacity*1/80,
                                         ifelse(soils$C_Capacity > 80,1,"")))
soils$C_deficit_indicator
#Soil nutrient suply
soils$Total_Nitrogen<-mib(soils$Total_Nitrogen)
soils$Potassium<-mib(soils$Potassium)
soils$Phosphorous<-mib(soils$Phosphorous)
soils$E_CEC<-mib(soils$E_CEC)
soils$Nutrient_supply<-rowMeans(soils[c("Total_Nitrogen",
                                        "E_CEC")])

#soil water retention
soils$Clay_content<-mib(soils$Clay_content)
soils$Bulk_density<-opt_bd(soils$Bulk_density)
soils$Depth_to_bedrock<-mib(soils$Depth_to_bedrock)
soils$Soil_water_storage<-rowMeans(soils[c("Clay_content","Bulk_density","Depth_to_bedrock")])
colnames(soils)
#View(soils)
dim(soils)
#--Maize yield--------------------------
#Dataset
getwd()
#Data Sheets
TNCCETRAD<-read.csv("./data/tnc_cetrad_cp.csv")
colnames(TNCCETRAD)
#----Primary crop Maize yield
#Load data needed all the the household field level
#harvest
library(dplyr)
harvest<- TNCCETRAD[c("HH_ID_Number","Q29_LongRainCrop","Q32_ProductionQuantityLongCrop","Q32_1_UnitsOfMeasureLong",
                      "Q35_ShortRainCrop" ,"Q38_ProductionQuantityShortCrop", "Q38_1_UnitsOfMeasureShort")]

colnames(harvest)
c_harvest_long<-filter(harvest,Q29_LongRainCrop == 1); dim(c_harvest_long)
c_harvest_long
categories1 <- unique(harvest$Q32_1_UnitsOfMeasureLong)
categories1
oldvals1<-c("90kgs","bags","Bags","90kgs bag" ,"small bags" , "debes", "90kg bags" , "90 KG BAG" ,"Sacks",
            "kgs" ,"" ,"90 kgs bag","bags of 90 kgs" , "90kg per bag" , "50kg bags" ,"50 kg bag" , "kg","90kg bag",
            "90 kg bags" , "big bags", "bucket","bag" ,"50 kgs bag","Debes","Bag" ,"Kgs","debe",
            "90KGS BAG" , "bags of 90kgs","90kg per acre","bags/0.5acre", "Tonnes","##N/A##",
            "90kgs bags", "90 kg bag","kilograms","50kgs bag","tonnes","rolls",
            "bag of 90kg" , "90 kgs bags" ,"90kg Sack","90 Kg bags","sacks per acre", 
            "lorry when the maize is still green", "Small sacks","5okg bags")

newvals1<-c(90,90,90,90,50,20,90,90,90,
            1,1,90,90,90,50,50,1,90,
            90,90,20,90,50,20,90,1,20,
            90,90,90,90,1000,'',
            90,90,1,50,1000,1000,
            90,90,90,90,90,
            7000,50,50)

c_harvest_long$kg_eq_long<-as.numeric(newvals1[match(c_harvest_long$Q32_1_UnitsOfMeasureLong, oldvals1)])
c_harvest_long<-c_harvest_long[c("HH_ID_Number","Q32_ProductionQuantityLongCrop","Q32_1_UnitsOfMeasureLong","kg_eq_long")]
c_harvest_long$harvest_long<-c_harvest_long$Q32_ProductionQuantityLongCrop*c_harvest_long$kg_eq_long
colnames(c_harvest_long)
c_harvest_long<-na.omit(c_harvest_long)
#cultivated area from acres to hectares (average)
cult_area<-TNCCETRAD[c("HH_ID_Number","Q6_CultivatedLandAreaOwned","Q8_CultivatedLandAreaLeasOrRent")]
cult_area
cult_area$total_cult_area<-rowMeans(cult_area[c("Q6_CultivatedLandAreaOwned","Q8_CultivatedLandAreaLeasOrRent")])*2.471
cult_area<-cult_area[c("HH_ID_Number","total_cult_area")]
cult_area<-na.omit(cult_area)
library("dplyr")
yield_long<-c_harvest_long%>%left_join(cult_area, by="HH_ID_Number")
colnames(yield_long)
yield_long<-yield_long[c( "HH_ID_Number",
                          "kg_eq_long","harvest_long","total_cult_area")]
yield_long$yield_long<-yield_long$harvest_long/yield_long$total_cult_area
yield_long
#New AI hh dataframe
yield_hh <- TNCCETRAD%>%group_by(HH_ID_Number)
yield_hh
yield_long<-full_join(yield_long,yield_hh, by="HH_ID_Number")
colnames(yield_long)
yield_long<-yield_long[c("HH_ID_Number","Q32_ProductionQuantityLongCrop","Q32_1_UnitsOfMeasureLong",
                            "kg_eq_long","harvest_long","total_cult_area","yield_long")]
yield_long[ yield_long == "Inf"] <- 0
yield_short[ yield_short == ""] <- 0
yield_long <- filter_if(yield_long, is.numeric, all_vars((.) != 0))
#yield_long<-na.omit(yield_long)
#View(yield_long)
dim(yield_long)
#spreadsheet with UENB spreadsheet information
write.csv(yield_long,"./data/HH_maize_yield_data_lr_KE.csv", row.names = FALSE)

#Secondary crop Maize yield
c_harvest_short<-filter(harvest, Q35_ShortRainCrop == 1); dim(c_harvest_short)
c_harvest_short
categories2 <- unique(harvest$Q38_1_UnitsOfMeasureShort)
categories2
oldvals2<-c("90 kgs","bags","Bags","50 kg bag","small bags","",
            "50kg bags" ,"50kg sack","bag","90kgs bag", "debes (kiswahili)",
            "kgs", "bag of 90kg","debe", "debes","big bags","50 kgs bag",
            "90 kgs bag","90 kg per bag","90kg bag","kg bag","kg" ,"Debes",
            "pieces","Pieces","50 kg bags","50 kgs bags" ,"tonne","Bag",
            "large bags" ,"Sacks" , "##N/A##","50kg bag","Kgs" ,"50kgs bag",
            "90kg bags" ,"bags of 90kgs","sacks" , "50kgs bags", "90kgs bags","Half bag",
            "have a bag" ,"50kg  per acre" , "Bucket" , "bag (50kgs)", "heads","60kg bag",
            "100 kg bag", "bags of 90kg","90 kgs bags" , "20 kg Buckets", "bales or rolls","buckets",
            "bags of 90 kgs","kasuku" ,"90 kg bags","bucket","Debe" , "sacks per half acre",
            "90 kg bag","tonnes","Crates" ,"pickup" )

newvals2<-c(90,90,90,50,50,'',
            50,50,50,90,20,1,90,20,20,90,50,
            90,90,90,90,1,20,
            1/3,1/3,50,50,1000,90,
            90,90,90,50,90,45,
            45,50,20,50,1/3,60,
            90,90,90,20,1000,20,
            90,2,90,20,20,90,
            90,1000,20,1000)

c_harvest_short$kg_eq_short<-as.numeric(newvals2[match(c_harvest_short$Q32_1_UnitsOfMeasureLong, oldvals2)])
c_harvest_short<-c_harvest_short[c("HH_ID_Number","Q38_ProductionQuantityShortCrop","Q38_1_UnitsOfMeasureShort","kg_eq_short")]
c_harvest_short$harvest_short<-c_harvest_short$Q38_ProductionQuantityShortCrop*c_harvest_short$kg_eq_short
colnames(c_harvest_short)
c_harvest_long
c_harvest_short

#--yield calculation-----
yield_short<-c_harvest_short%>%left_join(cult_area, by="HH_ID_Number")
colnames(yield_short)
yield_short<-yield_short[c( "HH_ID_Number", "kg_eq_short","harvest_short","total_cult_area")]
yield_short$yield_short<-yield_short$harvest_short/yield_short$total_cult_area
yield_short<-na.omit(yield_short)
#New AI hh dataframe
yield_hh <- TNCCETRAD%>%group_by(HH_ID_Number)
yield_hh
yield_short<-full_join(yield_short,yield_hh, by="HH_ID_Number")
colnames(yield_short)
yield_short<-yield_short[c("HH_ID_Number","Q38_ProductionQuantityShortCrop","Q38_1_UnitsOfMeasureShort",
                           "kg_eq_short","harvest_short","total_cult_area","yield_short")]
yield_short[ yield_short == "Inf"] <- 0
yield_short[ yield_short == ""] <- 0
#yield_short<-na.omit(yield_short)
dim(yield_short)
#View(yield_short)
#spreadsheet with UENB spreadsheet information
#write.csv(yield_short,"./data/HH_maize_yield_data_sr_KE.csv", row.names = FALSE)

# Total Maize yield
TNCCETRAD<-read.csv("./data/tnc_cetrad_cp.csv")
hh_id<-TNCCETRAD[c("HH_ID_Number")]
maize_yield<-full_join(yield_short,yield_long, hh_id,by="HH_ID_Number")
colnames(maize_yield)
dim(maize_yield)
View(maize_yield)
#View(maize_yield)
maize_yield$Maize_harvest<-rowMeans(maize_yield[c( "harvest_short","harvest_long")],na.rm=T)
maize_yield$Maize_yield<-rowMeans(maize_yield[c( "yield_short","yield_long")],na.rm=T)
maize_yield<-as.data.frame(maize_yield)
maize_yield[ maize_yield == "Inf"] <- 0
as.data.frame(colSums(is.na(maize_yield[,-c(1)])))
#View(maize_yield)
# NA replacement by 0
maize_yield$Maize_yield <-maize_yield$Maize_yield%>%replace(is.na(.), 0)
colnames(maize_yield)
maize_yield<-maize_yield[c("HH_ID_Number","yield_short",
                          "yield_long","Maize_harvest","Maize_yield")]
as.data.frame(colSums(is.na(maize_yield)))
maize_yield<-as.data.frame(maize_yield)
colnames(maize_yield)
dim(maize_yield)
mean(maize_yield$Maize_yield)
sd(maize_yield$Maize_yield)
write.csv(maize_yield,"./data/maize_yield_KE.csv", row.names = FALSE)
maize_yield$Maize_yield<-mib(maize_yield$Maize_yield)
#-----Potato yield-------
#Dataset
getwd()
#Data Sheets
TNCCETRAD<-read.csv("./data/tnc_cetrad_cp.csv")
colnames(TNCCETRAD)
#----Primary crop potato
#Load data needed all the the household field level
#harvest
library(dplyr)
harvest<- TNCCETRAD[c("HH_ID_Number","Q29_LongRainCrop","Q32_ProductionQuantityLongCrop","Q32_1_UnitsOfMeasureLong",
                      "Q35_ShortRainCrop" ,"Q38_ProductionQuantityShortCrop", "Q38_1_UnitsOfMeasureShort")]

colnames(harvest)
c_harvest_long<-filter(harvest,Q29_LongRainCrop == 4); dim(c_harvest_long)
c_harvest_long
categories1 <- unique(harvest$Q32_1_UnitsOfMeasureLong)
categories1
oldvals1<-c("90kgs","bags","Bags","90kgs bag" ,"small bags" , "debes", "90kg bags" , "90 KG BAG" ,"Sacks",
            "kgs" ,"" ,"90 kgs bag","bags of 90 kgs" , "90kg per bag" , "50kg bags" ,"50 kg bag" , "kg","90kg bag",
            "90 kg bags" , "big bags", "bucket","bag" ,"50 kgs bag","Debes","Bag" ,"Kgs","debe",
            "90KGS BAG" , "bags of 90kgs","90kg per acre","bags/0.5acre", "Tonnes","##N/A##",
            "90kgs bags", "90 kg bag","kilograms","50kgs bag","tonnes","rolls",
            "bag of 90kg" , "90 kgs bags" ,"90kg Sack","90 Kg bags","sacks per acre", 
            "lorry when the maize is still green", "Small sacks","5okg bags")

newvals1<-c(90,90,90,90,50,20,90,90,90,
            1,1,90,90,90,50,50,1,90,
            90,90,20,90,50,20,90,1,20,
            90,90,90,90,1000,'',
            90,90,1,50,1000,1000,
            90,90,90,90,90,
            7000,50,50)

c_harvest_long$kg_eq_long<-as.numeric(newvals1[match(c_harvest_long$Q32_1_UnitsOfMeasureLong, oldvals1)])
c_harvest_long<-c_harvest_long[c("HH_ID_Number","Q32_ProductionQuantityLongCrop","Q32_1_UnitsOfMeasureLong","kg_eq_long")]
c_harvest_long$harvest_long<-c_harvest_long$Q32_ProductionQuantityLongCrop*c_harvest_long$kg_eq_long
colnames(c_harvest_long)
c_harvest_long<-na.omit(c_harvest_long)
#cultivated area from acres to hectares (average)
cult_area<-TNCCETRAD[c("HH_ID_Number","Q6_CultivatedLandAreaOwned","Q8_CultivatedLandAreaLeasOrRent")]
cult_area
cult_area$total_cult_area<-rowMeans(cult_area[c("Q6_CultivatedLandAreaOwned","Q8_CultivatedLandAreaLeasOrRent")])*2.471
cult_area<-cult_area[c("HH_ID_Number","total_cult_area")]
cult_area<-na.omit(cult_area)
library("dplyr")
yield_long<-c_harvest_long%>%left_join(cult_area, by="HH_ID_Number")
colnames(yield_long)
yield_long<-yield_long[c( "HH_ID_Number",
                          "kg_eq_long","harvest_long","total_cult_area")]
yield_long$yield_long<-yield_long$harvest_long/yield_long$total_cult_area
yield_long
#New AI hh dataframe
yield_hh <- TNCCETRAD%>%group_by(HH_ID_Number)
yield_hh
yield_long<-full_join(yield_long,yield_hh, by="HH_ID_Number")
colnames(yield_long)
yield_long<-yield_long[c("HH_ID_Number","Q32_ProductionQuantityLongCrop","Q32_1_UnitsOfMeasureLong",
                         "kg_eq_long","harvest_long","total_cult_area","yield_long")]
yield_long[ yield_long == "Inf"] <- 0
yield_long[ yield_long == ""] <- 0
yield_long <- filter_if(yield_long, is.numeric, all_vars((.) != 0))
#yield_long<-na.omit(yield_long)
#View(yield_long)
#spreadsheet with UENB spreadsheet information
write.csv(yield_long,"./data/HH_potato_yield_data_lr_KE.csv", row.names = FALSE)

#Secondary crop Potato yield
c_harvest_short<-filter(harvest, Q35_ShortRainCrop == 4); dim(c_harvest_short)
c_harvest_short
categories2 <- unique(harvest$Q38_1_UnitsOfMeasureShort)
categories2
oldvals2<-c("90 kgs","bags","Bags","50 kg bag","small bags","",
            "50kg bags" ,"50kg sack","bag","90kgs bag", "debes (kiswahili)",
            "kgs", "bag of 90kg","debe", "debes","big bags","50 kgs bag",
            "90 kgs bag","90 kg per bag","90kg bag","kg bag","kg" ,"Debes",
            "pieces","Pieces","50 kg bags","50 kgs bags" ,"tonne","Bag",
            "large bags" ,"Sacks" , "##N/A##","50kg bag","Kgs" ,"50kgs bag",
            "90kg bags" ,"bags of 90kgs","sacks" , "50kgs bags", "90kgs bags","Half bag",
            "have a bag" ,"50kg  per acre" , "Bucket" , "bag (50kgs)", "heads","60kg bag",
            "100 kg bag", "bags of 90kg","90 kgs bags" , "20 kg Buckets", "bales or rolls","buckets",
            "bags of 90 kgs","kasuku" ,"90 kg bags","bucket","Debe" , "sacks per half acre",
            "90 kg bag","tonnes","Crates" ,"pickup" )

newvals2<-c(90,90,90,50,50,'',
            50,50,50,90,20,1,90,20,20,90,50,
            90,90,90,90,1,20,
            1/3,1/3,50,50,1000,90,
            90,90,90,50,90,45,
            45,50,20,50,1/3,60,
            90,90,90,20,1000,20,
            90,2,90,20,20,90,
            90,1000,20,1000)

c_harvest_short$kg_eq_short<-as.numeric(newvals2[match(c_harvest_short$Q32_1_UnitsOfMeasureLong, oldvals2)])
c_harvest_short<-c_harvest_short[c("HH_ID_Number","Q38_ProductionQuantityShortCrop","Q38_1_UnitsOfMeasureShort","kg_eq_short")]
c_harvest_short$harvest_short<-c_harvest_short$Q38_ProductionQuantityShortCrop*c_harvest_short$kg_eq_short
colnames(c_harvest_short)
c_harvest_long
c_harvest_short

#--yield calculation-----
yield_short<-c_harvest_short%>%left_join(cult_area, by="HH_ID_Number")
colnames(yield_short)
yield_short<-yield_short[c( "HH_ID_Number", "kg_eq_short","harvest_short","total_cult_area")]
yield_short$yield_short<-yield_short$harvest_short/yield_short$total_cult_area
#yield_short<-na.omit(yield_short)
#New AI hh dataframe
yield_hh <- TNCCETRAD%>%group_by(HH_ID_Number)
yield_hh
yield_short<-full_join(yield_short,yield_hh, by="HH_ID_Number")
colnames(yield_short)
yield_short<-yield_short[c("HH_ID_Number","Q38_ProductionQuantityShortCrop","Q38_1_UnitsOfMeasureShort",
                           "kg_eq_short","harvest_short","total_cult_area","yield_short")]
yield_short[ yield_short == "Inf"] <- 0
yield_short[ yield_short == ""] <- 0
#yield_short<-na.omit(yield_short)
#View(yield_short)
#spreadsheet with UENB spreadsheet information
#write.csv(yield_short,"./data/HH_maize_yield_data_sr_KE.csv", row.names = FALSE)

# Total Potato yield
pot_yield<-full_join(yield_short,yield_long, by="HH_ID_Number")
colnames(pot_yield)
View(pot_yield)
pot_yield$pot_harvest<-rowMeans(pot_yield[c( "harvest_short","harvest_long")],na.rm=T)
pot_yield$pot_yield<-rowMeans(pot_yield[c( "yield_short","yield_long")],na.rm=T)
pot_yield<-as.data.frame(pot_yield)
pot_yield[ pot_yield == "Inf"] <- 0
pot_yield[ pot_yield == ""] <- 0
as.data.frame(colSums(is.na(pot_yield[,-c(1)])))
#View(maize_yield)
# NA replacement by 0
pot_yield$pot_yield <-pot_yield$pot_yield%>%replace(is.na(.), 0)
colnames(pot_yield)
pot_yield<-pot_yield[c("HH_ID_Number","yield_short",
                           "yield_long","pot_harvest","pot_yield")]
as.data.frame(colSums(is.na(pot_yield)))
pot_yield<-as.data.frame(pot_yield)
colnames(pot_yield)
mean(pot_yield$pot_yield)
sd(pot_yield$pot_yield)
write.csv(pot_yield,"./data/pot_yield_KE.csv", row.names = FALSE)
pot_yield$pot_yield<-mib(pot_yield$pot_yield)
pot_yield
#---Livestock-------
livestock<-read.csv("./data/HH_imp_LCI_KE.csv")
colnames(livestock)
livestock<-livestock[c("HH_ID_Number","Livestock_holding")]
livestock$Livestock_holding<-mib(livestock$Livestock_holding)
livestock<-as.data.frame(livestock)
livestock
#--Land under cultivation------------
cult_land<-read.csv("./data/tnc_cetrad_cp.csv")
colnames(cult_land)
cult_land<-cult_land[c("HH_ID_Number","Q6_CultivatedLandAreaOwned")]
cult_land$Q6_CultivatedLandAreaOwned<-mib(cult_land$Q6_CultivatedLandAreaOwned)
cult_land<-as.data.frame(cult_land)
names(cult_land)[2]<-'Land_cultivated'
cult_land

#--ecosystem services indicators for SOM and ML models-----
es<-soils%>%left_join(maize_yield, 
    by="HH_ID_Number")%>%left_join(pot_yield, by="HH_ID_Number")%>%left_join(cult_land, by="HH_ID_Number")
colnames(es)
es<-es[c("HH_ID_Number","Ca_indicator", "Mg_indicator","K_indicator",            
         "P_indicator","S_indicator", "soil_acidity", "soil_fertility_indicator",
         "C_deficit_indicator", "Nutrient_supply","Soil_water_storage",
         "Maize_yield","pot_yield")]
as.data.frame(colSums(is.na(es[,-c(1)])))
es<-na.omit(es)
View(es)
dim(es)
write.csv(es,"./data/HH_es_data_KE.csv", row.names = FALSE)

