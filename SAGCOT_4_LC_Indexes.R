#SIIL Capital quantification                                   ##
#Weighted Entropy Indeces for capital asset  level indicators ##
###############################################################
#Household Livelihood Resilience Approach (HLRA) methodology described by Quandt (2018)
#Measuring livelihood resilience: the household livelihood resilience approach (HLRA)
#---Setups---------
getwd()
rm(list=ls(all=TRUE))  #Remove objects in memory
#Open libraries
#install.packages("rlang")
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
#reMOving row with eplot NA & 55 & 0
VS_MergingFile<-filter(VS_MergingFile, eplot_no != 55 & eplot_no != "NA" & eplot_no != 0 )
VS_MergingFile
#Dropping rows of 5 inexistant household 
VS_MergingFile<-VS_MergingFile[!(VS_MergingFile$hh_refno =="TZA-L10-H25" | VS_MergingFile$hh_refno =="TZA-L10-H09"| 
                                   VS_MergingFile$hh_refno =="TZA-L18-H29"| VS_MergingFile$hh_refno=="TZA-L19-H11"| VS_MergingFile$hh_refno  =="TZA-L19-H19"),]
colnames(VS_MergingFile)
dim(VS_MergingFile)

#Min-max Normalization
norm<-function(x){
  (x- min(x)) /(max(x)-min(x))}
#geometric mean function
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
#standardizing functions
lib<-function(x){
  y<-1-((x-min(x))/(max(x)-min(x)))
  return(y)
}
mib<-function(x){
  y<-(x-min(x))/(max(x)-min(x))
  return(y)
}

#imputed data
#normalized imputed data
imputed.data.norm<-read.csv(paste('./8_Analyses/Datasets/HH_masterdata_knnimp_norm.csv', sep=""))
colnames(imputed.data.norm)
imputed.data.norm

#------Standardization -----------------------------
imputed.data<-read.csv(paste('./8_Analyses/Datasets/HH_masterdata_knnimp.csv', sep=""))
colnames(imputed.data)
dim(imputed.data)
as.data.frame(colSums(is.na(imputed.data[,-c(1:4)])))
write.csv(imputed.data,'./8_Analyses/Datasets/HH_imputed_data_final.csv',row.names = FALSE)
#max(imputed.data$CIF)
summary(imputed.data)
colnames(imputed.data)
imputed.data$lands_eplot<-paste(imputed.data$landscape_no,"_",imputed.data$eplot_no)
imputed.data.std<-imputed.data[c("hh_refno","landscape_no", "eplot_no","ave_precip","dist_water_bodies","Shannon_tree",
              "dist_forest","richness_tree","farmproc_clay_icraf",
               "Male_Female_ratio", "hhmembers","hh_dep_ratio","literacy_rate","leveledu",            
                "hh_labor_days","Info_network","market_network","ag_implements","water_inf",           
             "farm_structures","comm_inf","dist_cell_wifi_tower","transp_inf","dist_main_roads",     
           "dist_any_roads","time_field_travel","wage_entry_n","liv_sale_number","house_tenure_n",      
           "house_walls_n","house_roof_n","house_floor_n","house_san_n","house_electricity_n", 
       "house_water_n","house_fuel_cook_n","house_fuel_light_n","hh_microf_n","landsize","fields",
       "CIF","CIIF_adapted","CPF","CPF_adapted")]
colnames(imputed.data.std)
imputed.data.std$ave_precip<-mib(imputed.data.std$ave_precip)
imputed.data.std$dist_water_bodies<-lib(imputed.data.std$dist_water_bodies)
imputed.data.std$Shannon_tree<-mib(imputed.data.std$Shannon_tree)
imputed.data.std$richness_tree<-mib(imputed.data.std$Shannon_tree)
imputed.data.std$farmproc_clay_icraf<-1-((abs(imputed.data.std$farmproc_clay_icraf-20) -
abs(20-min(imputed.data.std$farmproc_clay_icraf)))/(abs(max(imputed.data.std$farmproc_clay_icraf)-20)-
abs(20-min(imputed.data.std$farmproc_clay_icraf))))
imputed.data.std$dist_forest<-lib(imputed.data.std$dist_forest)
imputed.data.std$Male_Female_ratio<-mib(imputed.data.std$Male_Female_ratio)
imputed.data.std$hhmembers<-mib(imputed.data.std$hhmembers)
imputed.data.std$hh_dep_ratio<-mib(imputed.data.std$hh_dep_ratio)
imputed.data.std$literacy_rate<-mib(imputed.data.std$literacy_rate)
imputed.data.std$leveledu<-mib(imputed.data.std$leveledu)
imputed.data.std$hh_labor_days<-mib(imputed.data.std$hh_labor_days)
imputed.data.std$Info_network<-mib(imputed.data.std$Info_network)
imputed.data.std$market_network<-mib(imputed.data.std$market_network)
imputed.data.std$ag_implements<-mib(imputed.data.std$ag_implements)
imputed.data.std$water_inf<-mib(imputed.data.std$water_inf)
imputed.data.std$farm_structures<-mib(imputed.data.std$farm_structures)
imputed.data.std$comm_inf<-mib(imputed.data.std$comm_inf)
imputed.data.std$dist_cell_wifi_tower<-lib(imputed.data.std$dist_cell_wifi_tower)
imputed.data.std$transp_inf<-mib(imputed.data.std$transp_inf)
imputed.data.std$dist_main_roads<-lib(imputed.data.std$dist_main_roads)
imputed.data.std$dist_any_roads<-lib(imputed.data.std$dist_any_roads)
imputed.data.std$time_field_travel<-lib(imputed.data.std$time_field_travel)
imputed.data.std$wage_entry_n<-mib(imputed.data.std$wage_entry_n)
imputed.data.std$house_tenure_n<-mib(imputed.data.std$house_tenure_n)
imputed.data.std$house_walls_n<-mib(imputed.data.std$house_walls_n)
imputed.data.std$house_floor_n<-mib(imputed.data.std$house_floor_n)
imputed.data.std$house_roof_n<-mib(imputed.data.std$house_roof_n)
imputed.data.std$house_san_n<-mib(imputed.data.std$house_san_n)
imputed.data.std$house_electricity_n<-mib(imputed.data.std$house_electricity_n)
imputed.data.std$house_water_n<-mib(imputed.data.std$house_water_n)
imputed.data.std$house_fuel_cook_n<-mib(imputed.data.std$house_fuel_cook_n)
imputed.data.std$house_fuel_light_n<-mib(imputed.data.std$house_fuel_light_n)
imputed.data.std$hh_microf_n<-mib(imputed.data.std$hh_microf_n)
imputed.data.std$landsize<-mib(imputed.data.std$landsize)
imputed.data.std$hh_microf_n<-mib(imputed.data.std$hh_microf_n)
imputed.data.std$fields<-mib(imputed.data.std$fields)
imputed.data.std$CIIF_adapted<-mib(imputed.data.std$CIIF_adapted)
imputed.data.std$CPF_adapted<-lib(imputed.data.std$CPF_adapted)
imputed.data.std$CIF<-mib(imputed.data.std$CIF)
imputed.data.std$CPF<-mib(imputed.data.std$CPF)
colnames(imputed.data.std)
# imputed.data.std$NC<-rowSums(imputed.data.std[c("ave_precip","dist_water_bodies","Shannon_tree",
#                                                 "dist_forest","richness_tree","farmproc_clay_icraf")])
# imputed.data.std$HC<-rowSums(imputed.data.std[c("Male_Female_ratio", "hhmembers",
#                                                 "leveledu")])
# imputed.data.std$SC<-rowSums(imputed.data.std[c("Info_network","market_network")])
# imputed.data.std$PC<-rowSums(imputed.data.std[c("ag_implements","water_inf",           
#                                                 "farm_structures","comm_inf","dist_cell_wifi_tower",
#                                                 "transp_inf","dist_main_roads")])
# imputed.data.std$FC<-rowSums(imputed.data.std[c("house_tenure_n",
#                                                 'landsize')])
# imputed.data.std$HDC<-rowSums(imputed.data.std[c("HC","SC","PC","FC")])
# dim(imputed.data.std)
dim(imputed.data.std)
write.csv(imputed.data.std,'./8_Analyses/Datasets/HH_LCA_DM_norm.csv',row.names = FALSE)
#--AIS proportions-----------
dim(imputed.data)
#CIF
sum_cif<-c()
sum_cif<-imputed.data%>%count(CIF)
sum_cif<-as.data.frame(sum_cif)
sum_cif$percent<-sum_cif$n/210*100
sum_cif<-as.data.frame(sum_cif)
sum_cif
#CPF
sum_cpf<-c()
sum_cpf<-imputed.data%>%count(CPF)
sum_cpf<-as.data.frame(sum_cpf)
sum_cpf$percent<-sum_cpf$n/210*100
sum_cpf<-as.data.frame(sum_cpf)
sum_cpf
#----Indeces -------------------------
#These indeces only account for indicators for which there are a significant difference between the groups
#aggregating raw imputed data  at the e-plot level
lci<-read.csv(paste('./8_Analyses/Datasets/HH_LCA_DM_norm.csv', sep=""))
colnames(lci)
dim(lci)
View(lci)
library(creditmodel)
id_data<-lci[c("hh_refno","landscape_no","eplot_no")]
#Natural capital index
colnames(lci)
nc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("ave_precip","Shannon_tree","richness_tree"),
                      neg_vars = c("dist_water_bodies","dist_forest","farmproc_clay_icraf"))
nc_ew
nc_si<-c()
nc_si<-((nc_ew$Weight[1]*(lci$ave_precip))+(nc_ew$Weight[2]*(lci$Shannon_tree))
        +(nc_ew$Weight[3]*(lci$richness_tree)) +(nc_ew$Weight[4]*(lci$dist_water_bodies))+
         (nc_ew$Weight[5]*(lci$dist_forest))+(nc_ew$Weight[6]*(lci$farmproc_clay_icraf)))
nc_si
nc_si<-as.data.frame(nc_si)
colnames(nc_si)
nc_si<-cbind(id_data[,1],nc_si)
names(nc_si)[1]<-'hh_refno'
nc_si

#Human capital index
colnames(lci)
hc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("Male_Female_ratio","hhmembers","leveledu", "literacy_rate","hh_labor_days"),
                      neg_vars = c("hh_dep_ratio"))
hc_ew
hc_si<-c()
hc_si<-((hc_ew$Weight[1]*(lci$Male_Female_ratio))+
          (hc_ew$Weight[2]*(lci$hhmembers))+
        (hc_ew$Weight[3]*(lci$leveledu))+
  (hc_ew$Weight[4]*(lci$literacy_rate))+
  (hc_ew$Weight[5]*(lci$hh_labor_days))+ 
    (hc_ew$Weight[6]*(lci$hh_dep_ratio)))
hc_si
hc_si<-as.data.frame(hc_si)
colnames(hc_si)
hc_si<-cbind(id_data[,1],hc_si)
names(hc_si)[1]<-'hh_refno'
hc_si

#Social capital index
colnames(lci)
sc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("Info_network","market_network"),
                      neg_vars = c())
sc_ew
sc_si<-c()
sc_si<-((sc_ew$Weight[1]*(lci$Info_network))+
          (hc_ew$Weight[2]*(lci$market_network)))
sc_si
sc_si<-as.data.frame(sc_si)
colnames(sc_si)
sc_si<-cbind(id_data[,1],sc_si)
names(sc_si)[1]<-'hh_refno'
sc_si

#Physical capital index
colnames(lci)
pc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("ag_implements",
                                   "water_inf",
                                   "farm_structures",
                                   "comm_inf",
                                   "transp_inf"),
                      neg_vars = c("dist_main_roads","dist_cell_wifi_tower","dist_any_roads","time_field_travel"))
pc_ew
pc_si<-c()
pc_si<-((pc_ew$Weight[1]*(lci$ag_implements))+
          (pc_ew$Weight[2]*(lci$water_inf))+
          (pc_ew$Weight[3]*(lci$farm_structures))+
          (pc_ew$Weight[4]*(lci$comm_inf))+
          (pc_ew$Weight[5]*(lci$transp_inf))+
          (pc_ew$Weight[6]*(lci$dist_main_roads)+
             (pc_ew$Weight[7]*(lci$dist_cell_wifi_tower)+
                (pc_ew$Weight[8]*(lci$dist_any_roads)+
             (pc_ew$Weight[9]*(lci$time_field_travel))))))
pc_si
pc_si<-as.data.frame(pc_si)
colnames(pc_si)
pc_si<-cbind(id_data[,1],pc_si)
names(pc_si)[1]<-'hh_refno'
pc_si

#Financial capital index
colnames(lci)
fc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("house_tenure_n","landsize","wage_entry_n","liv_sale_number","hh_microf_n"),
                      neg_vars = c())
fc_ew
fc_si<-c()
fc_si<-((fc_ew$Weight[1]*(lci$house_tenure_n))+
          (fc_ew$Weight[2]*(lci$landsize))+
          (fc_ew$Weight[3]*(lci$wage_entry_n))+
          (fc_ew$Weight[4]*(lci$liv_sale_number))+
          (fc_ew$Weight[5]*(lci$hh_microf_n)))

fc_si
fc_si<-as.data.frame(fc_si)
colnames(fc_si)
fc_si<-cbind(id_data[,1],fc_si)
names(fc_si)[1]<-'hh_refno'
fc_si

#Indeces
indeces<-nc_si%>%left_join(hc_si,
                           by="hh_refno")%>%left_join(sc_si,
                 by="hh_refno")%>%left_join(pc_si,
                 by="hh_refno")%>%left_join(fc_si,by="hh_refno")

indeces

#HDC with only significant variables
colnames(indeces)
indeces$nc_si_std<-mib(indeces$nc_si)
indeces$hc_si_std<-mib(indeces$hc_si)
indeces$sc_si_std<-mib(indeces$sc_si)
indeces$pc_si_std<-mib(indeces$pc_si)
indeces$fc_si_std<-mib(indeces$fc_si)
hdc_ew<-entropy_weight(dat = indeces,
                       pos_vars = c( 
                                     "hc_si_std",
                                     "sc_si_std",
                                     "pc_si_std",
                                    "fc_si_std"),
                       neg_vars = c())
hdc_ew
hdc_si<-c()
hdc_si<-((hdc_ew$Weight[1]*(indeces$hc_si))+
           (hdc_ew$Weight[2]*(indeces$sc_si))+
           (hdc_ew$Weight[3]*(indeces$pc_si))+
           (hdc_ew$Weight[4]*(indeces$fc_si)))

hdc_si
hdc_si<-as.data.frame(hdc_si)
colnames(hdc_si)
hdc_si<-cbind(id_data[,1],hdc_si)
names(hdc_si)[1]<-'hh_refno'
hdc_si
hdc_si$hdc_si_std<-mib(hdc_si$hdc_si)
#bringing all together
#loading cluster data
#loading cluster data
cluster<-read.csv(paste('./8_Analyses/Datasets/som_all_data.csv',sep=""))
colnames(cluster)
cluster<-cluster[c(1,5:11)]
indeces_data<-indeces%>%left_join(hdc_si, by='hh_refno')%>%left_join(id_data, 
              by='hh_refno')%>%left_join(cluster, by='hh_refno')
indeces_data<-na.omit(indeces_data)
names(indeces_data)[22]<-'ESC'
dim(indeces_data)
colnames(indeces_data)
write.csv(indeces_data,'./8_Analyses/Datasets/HH_indeces.csv',row.names = FALSE)
indeces_data

#--Statistical analyses--------
#--Mean and standard deviation of data-----
#loading raw livelihoods and md data
raw.data<-read.csv(paste('./8_Analyses/Datasets/HH_imputed_data_final.csv', sep=""))
dim(raw.data)
colnames(raw.data)
#loading cluster data
cluster<-read.csv(paste('./8_Analyses/Datasets/som_all_data.csv',sep=""))
colnames(cluster)
cluster<-cluster[c(1,5:11)]
names(cluster)[8]<-'ESC'
#data
data<-raw.data%>%left_join(cluster,by=c('hh_refno'))
colnames(data)
data<-na.omit(data)
dim(data)
#View(data)
colnames(data)
# Group by mean of multiple columns
data_mean<-data%>%group_by(ESC)%>% 
  summarise(
    mean_precipitation=mean(ave_precip),
    mean_water_bodies=mean(dist_water_bodies),
    mean_shannon=mean(Shannon_tree),
    mean_richness=mean(richness_tree),
    mean_clay=mean(farmproc_clay_icraf),
    mean_dist_forest=mean(dist_forest),
    mean_MF_ratio=mean(Male_Female_ratio),
    mean_hhmembers=mean(hhmembers),
    mean_hh_depratio=mean(hh_dep_ratio),
    mean_lit_rate=mean(literacy_rate),
    mean_leveledu=mean(leveledu),
    mean_labordays=mean(hh_labor_days),
    mean_infonet=mean(Info_network),
    mean_mark=mean(market_network),
    mean_agimp=mean(ag_implements),
    mean_waterinf=mean(water_inf),
    mean_farminf=mean(farm_structures),
    mean_comm=mean(comm_inf),
    mean_wifi_dist=mean(dist_cell_wifi_tower),
    mean_transp = mean(transp_inf),
    mean_dist_road_main = mean(dist_main_roads),
    mean_wage = mean(wage_entry_n),
    mean_livestock = mean(liv_sale_number),
    mean_house_tenure = mean(house_tenure_n),
    mean_microf = mean(hh_microf_n),
    mean_landsize = mean(landsize),
    mean_farm_type = mean(farm_type),
    mean_cpf = mean(CPF),
    mean_cif = mean(CIIF),
    .groups = 'drop')%>%as.data.frame()
data_mean
# Group by SD of multiple columns
data_sd<-data%>%group_by(ESC)%>% 
  summarise(
    sd_precipitation=sd(ave_precip),
    sd_water_bodies=sd(dist_water_bodies),
    sd_shannon=sd(Shannon_tree),
    sd_richness=sd(richness_tree),
    sd_clay=sd(farmproc_clay_icraf),
    sd_dist_forest=sd(dist_forest),
    sd_MF_ratio=sd(Male_Female_ratio),
    sd_hhmembers=sd(hhmembers),
    sd_hh_depratio=sd(hh_dep_ratio),
    sd_lit_rate=sd(literacy_rate),
    sd_leveledu=sd(leveledu),
    sd_labordays=sd(hh_labor_days),
    sd_infonet=sd(Info_network),
    sd_mark=sd(market_network),
    sd_agimp=sd(ag_implements),
    sd_waterinf=sd(water_inf),
    sd_farminf=sd(farm_structures),
    sd_comm=sd(comm_inf),
    sd_wifi_dist=sd(dist_cell_wifi_tower),
    sd_transp = sd(transp_inf),
    sd_dist_road_main = sd(dist_main_roads),
    sd_wage = sd(wage_entry_n),
    sd_livestock = sd(liv_sale_number),
    sd_house_tenure =sd(house_tenure_n),
    sd_microf = sd(hh_microf_n),
    sd_landsize = sd(landsize),
    sd_farm_type = sd(farm_type),
    sd_cpf = sd(CPF),
    sd_cif= sd(CIIF),
    .groups = 'drop')%>%as.data.frame()
data_sd
#--Kruskal-wilis test for all indicators-----------------------
#first confirming non-normality
colnames(data)
shapiro.test(data$ave_precip)
shapiro.test(data$dist_water_bodies)
shapiro.test(data$Shannon_tree)
shapiro.test(data$richness_tree)
shapiro.test(data$farmproc_clay_icraf)
shapiro.test(data$dist_forest)
shapiro.test(data$Male_Female_ratio)
shapiro.test(data$hhmembers)
shapiro.test(data$hh_dep_ratio)
shapiro.test(data$literacy_rate)
shapiro.test(data$leveledu)
shapiro.test(data$hh_labor_days)
shapiro.test(data$Info_network)
shapiro.test(data$market_network)
shapiro.test(data$ag_implements)
shapiro.test(data$water_inf)
shapiro.test(data$farm_structures)
shapiro.test(data$comm_inf)
shapiro.test(data$dist_cell_wifi_tower)
shapiro.test(data$transp_inf)
shapiro.test(data$dist_main_roads)
shapiro.test(data$wage_entry_n)
shapiro.test(data$liv_sale_number)
shapiro.test(data$house_tenure_n)
shapiro.test(data$hh_microf_n)
shapiro.test(data$landsize)
shapiro.test(data$CIIF_adapted)
shapiro.test(data$CPF_adapted)
#all variables are non-normal                  
#krusal test
kruskal.test(ave_precip ~ ESC, data = data)
library(ggstatsplot)
ggbetweenstats(
  data = data,
  x = ESC,
  y = ave_precip,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(dist_water_bodies ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = dist_water_bodies,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(Shannon_tree ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = Shannon_tree,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(richness_tree ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = richness_tree,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(farmproc_clay_icraf~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = farmproc_clay_icraf,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(dist_forest ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = dist_forest,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(Male_Female_ratio ~ ESC, data = data)
kruskal.test(hhmembers ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = hhmembers,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(hh_dep_ratio ~ ESC, data = data)
kruskal.test(literacy_rate ~ ESC, data = data)
kruskal.test(leveledu ~ ESC, data = data)
kruskal.test(hh_labor_days ~ ESC, data = data)
kruskal.test(Info_network ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = Info_network,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(market_network ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = market_network,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(ag_implements ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = ag_implements,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(water_inf ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = water_inf,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(farm_structures ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = farm_structures,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(comm_inf ~ ESC, data = data)
kruskal.test(dist_cell_wifi_tower ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = dist_cell_wifi_tower,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(transp_inf ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = transp_inf,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(dist_main_roads ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = dist_main_roads,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(wage_entry_n ~ ESC, data = data)
kruskal.test(liv_sale_number ~ ESC, data = data)
kruskal.test(house_tenure_n ~ ESC, data = data)
kruskal.test(hh_microf_n ~ ESC, data = data)
kruskal.test(landsize ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = landsize,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(farm_type ~ ESC, data = data)
kruskal.test(CIF ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = CIF,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(CIIF_adapted ~ ESC, data = data)
kruskal.test(CPF ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = CPF,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(CPF_adapted ~ ESC, data = data)
ggbetweenstats(
  data = data,
  x = ESC,
  y = CPF_adapted,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
#----Statistical analyses indeces -----
indeces_mean<-indeces_data%>%group_by(ESC)%>% 
  summarise(
    mean_nc=mean(nc_si),
    mean_hc=mean(hc_si),
    mean_sc=mean(sc_si),
    mean_pc=mean(pc_si),
    mean_fc=mean(fc_si),
    mean_hdc=mean(hdc_si),
    .groups = 'drop')%>%as.data.frame()
indeces_mean

indeces_sd<-indeces_data%>%group_by(ESC)%>% 
  summarise(
    sd_nc=sd(nc_si),
    sd_hc=sd(hc_si),
    sd_sc=sd(sc_si),
    sd_pc=sd(pc_si),
   sd_fc=sd(fc_si),
    sd_hdc=sd(hdc_si),
    .groups = 'drop')%>%as.data.frame()
indeces_sd

kruskal.test(nc_si ~ ESC, data = indeces_data)
kruskal.test(hc_si ~ ESC, data = indeces_data)
kruskal.test(sc_si ~ ESC, data = indeces_data)
kruskal.test(pc_si ~ ESC, data = indeces_data)
kruskal.test(fc_si ~ ESC, data = indeces_data)
kruskal.test(hdc_si ~ ESC, data = indeces_data)

