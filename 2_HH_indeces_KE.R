#SIIL Capital quantification                                   ##
#Weighted Entropy Indeces for capital asset  level indicators ##
###############################################################
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

#library(caret)
library("psych") 
#----loading master data------
#imputed data
imputed.data<-read.csv(paste("./Data/HH_imp_LCI_KE.csv", sep=""))
colnames(imputed.data)
#LCIs
lci_data<-imputed.data[c("HH_ID_Number", "Freshwater_access", "Tree_cover", "Tree_diversity", "Rainfall","Soil",    
                          "HH_members", "Head_gender","Head_age","Head_education","Labor_hire", "Ag_trainings", "Info_network",     
                          "Market_network", "Implements","Comm_infra","Transport_mode","Road_state","Land_holding","Land_tenure","Crop_salepoint",
                         "Loan_access","Debt_status")]
dim(lci_data)
as.data.frame(colSums(is.na(lci_data[,-c(1)])))
#lci<-na.omit(lci_data)
# calculate the mean and standard deviation for each variable
library(dplyr)
lci<-lci_data
lci_stat<-lci[,-c(1)]
lci_stat
#mean
apply(lci_stat,2,mean)
#sd
apply(lci_stat,2,sd)

#---standardizing data------------
lci_data$Freshwater_access<-mib(lci_data$Freshwater_access)
lci_data$Tree_cover<-mib(lci_data$Tree_cover)
lci_data$Tree_diversity<-mib(lci_data$Tree_diversity)
lci_data$Rainfall<-mib(lci_data$Rainfall)
lci_data$Soil<-1-((abs(lci_data$Soil-20) - abs(20-min(lci_data$Soil)))/(abs(max(lci_data$Soil)-20) -abs(20-min(lci_data$Soil))))
lci_data$HH_members<-mib(lci_data$HH_members)
lci_data$Head_gender<-mib(lci_data$Head_gender)
lci_data$Head_age<-lib(lci_data$Head_age)
lci_data$Head_education<-mib(lci_data$Head_education)
lci_data$Labor_hire<-mib(lci_data$Labor_hire)
lci_data$Ag_trainings<-mib(lci_data$Ag_trainings)
lci_data$Info_network<-mib(lci_data$Info_network)
lci_data$Market_network<-mib(lci_data$Market_network)
lci_data$Implements<-mib(lci_data$Implements)
lci_data$Comm_infra<-mib(lci_data$Comm_infra)
lci_data$Transport_mode<-mib(lci_data$Transport_mode)
lci_data$Road_state<-mib(lci_data$Road_state)
lci_data$Land_holding<-mib(lci_data$Land_holding)
lci_data$Land_tenure<-mib(lci_data$Land_tenure)
lci_data$Loan_access<-mib(lci_data$Loan_access)
lci_data$Debt_status<-mib(lci_data$Debt_status)
lci_data

#----Indeces
#These indeces only account for indicators for which there are a significant difference between the groups
#aggregating raw imputed data  at the e-plot level
lci<-lci_data
colnames(lci)
library(creditmodel)
id_data<-lci[c("HH_ID_Number")]
#Natural capital index
colnames(lci)
nc_ew<-entropy_weight(dat = lci,
                      pos_vars = c( "Freshwater_access","Tree_cover", "Tree_diversity",  
                                   "Rainfall"),
                      neg_vars = c("Soil"))
nc_ew
nc_si<-c()
nc_si<-((nc_ew$Weight[1]*(lci$Freshwater_access))+
          (nc_ew$Weight[2]*(lci$Tree_cover))+
          (nc_ew$Weight[3]*(lci$Tree_diversity))+
          (nc_ew$Weight[4]*(lci$Rainfall))+
          (nc_ew$Weight[5]*(lci$Soil)))
nc_si
nc_si<-as.data.frame(nc_si)
colnames(nc_si)
nc_si<-cbind(id_data[,1],nc_si)
names(nc_si)[1]<-'HH_ID_Number'
nc_si$nc_si<-mib(nc_si$nc_si)

#Human capital index
colnames(lci)
hc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("HH_members","Head_gender","Head_age","Head_education",
                                   "Labor_hire","Ag_trainings"),
                      neg_vars = c())
hc_ew
hc_si<-c()
hc_si<-((hc_ew$Weight[1]*(lci$HH_members))+
          (hc_ew$Weight[2]*(lci$Head_gender))+
          (hc_ew$Weight[3]*(lci$Head_age))+
          (hc_ew$Weight[4]*(lci$Head_education))+
           (hc_ew$Weight[5]*(lci$Labor_hire))+
          (hc_ew$Weight[6]*(lci$Ag_trainings)))
hc_si
hc_si<-as.data.frame(hc_si)
colnames(hc_si)
hc_si<-cbind(id_data[,1],hc_si)
names(hc_si)[1]<-'HH_ID_Number'
hc_si

#Social capital index
colnames(lci)
sc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("Info_network","Market_network"),
                      neg_vars = c())
sc_ew
sc_si<-c()
sc_si<-((hc_ew$Weight[1]*(lci$Info_network))+(hc_ew$Weight[2]*(lci$Market_network)))
sc_si
sc_si<-as.data.frame(sc_si)
colnames(sc_si)
sc_si<-cbind(id_data[,1],sc_si)
names(sc_si)[1]<-'HH_ID_Number'
sc_si

#Physical capital index
colnames(lci)
pc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("Implements","Comm_infra","Transport_mode","Road_state"),
                      neg_vars = c())
pc_ew
pc_si<-c()
pc_si<-((pc_ew$Weight[1]*(lci$Implements))+
          (pc_ew$Weight[2]*(lci$Comm_infra))+
          (pc_ew$Weight[3]*(lci$Transport_mode))+
          (pc_ew$Weight[4]*(lci$Road_state)))
pc_si
pc_si<-as.data.frame(pc_si)
colnames(pc_si)
pc_si<-cbind(id_data[,1],pc_si)
names(pc_si)[1]<-'HH_ID_Number'
pc_si

#Financial capital index
colnames(lci)
fc_ew<-entropy_weight(dat = lci,
                      pos_vars = c("Land_holding","Land_tenure","Crop_salepoint",
                                   "Loan_access","Debt_status"),
                      neg_vars = c())
fc_ew
fc_si<-c()
fc_si<-((fc_ew$Weight[1]*(lci$Land_holding))+
          (fc_ew$Weight[2]*(lci$Land_tenure))+
          (fc_ew$Weight[3]*(lci$Crop_salepoint))+
          (fc_ew$Weight[4]*(lci$Loan_access))+
          (fc_ew$Weight[4]*(lci$Debt_status)))

fc_si
fc_si<-as.data.frame(fc_si)
colnames(fc_si)
fc_si<-cbind(id_data[,1],fc_si)
names(fc_si)[1]<-'HH_ID_Number'
fc_si

#Indeces
indeces<-nc_si%>%left_join(hc_si,
           by="HH_ID_Number")%>%left_join(sc_si,
          by="HH_ID_Number")%>%left_join(pc_si,
          by="HH_ID_Number")%>%left_join(fc_si,by="HH_ID_Number")
indeces

#HDC
colnames(indeces)
indeces$nc_si_std<-norm(indeces$nc_si)
indeces$hc_si_std<-norm(indeces$hc_si)
indeces$sc_si_std<-norm(indeces$sc_si)
indeces$pc_si_std<-norm(indeces$pc_si)
indeces$fc_si_std<-norm(indeces$fc_si)
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
names(hdc_si)[1]<-'HH_ID_Number'
hdc_si$hdc_si<-mib(hdc_si$hdc_si)
hdc_si
indeces_data<-indeces%>%left_join(hdc_si, by='HH_ID_Number')
indeces_data
dim(indeces_data)
write.csv(indeces_data,'./Data/HH_indeces.csv',row.names = FALSE)
#----Statistical analyses by cluster-----
#bringing all together with cluster data, capital data and raw data
#loading cluster data
cluster<-read.csv(paste('./Data/som_ES_data.csv',sep=""))
cluster<-cluster[c("HH_ID_Number","code_vector")]
names(cluster)[2]<-'ESC'
# cluster$rank<-ifelse(cluster$ESC==2,1,
#                ifelse(cluster$ESC==3,2,
#               ifelse(cluster$ESC==1,3,"")))
# cluster
raw_data<-read.csv(paste("./data/HH_imp_LCI_KE.csv", sep=""))
mi_data<-read.csv("./Data/HH_AII_KE.csv")
colnames(mi_data)
mi_data<-mi_data[c("HH_ID_Number","CIF","CPF")]
indeces_data<-raw_data%>%left_join(indeces_data, 
              by='HH_ID_Number')%>%left_join(mi_data, 
              by='HH_ID_Number')%>%left_join(cluster, by='HH_ID_Number')
indeces_data<-na.omit(indeces_data)
dim(indeces_data)
colnames(indeces_data)
#write.csv(indeces_data,'./Data/HH_indeces.csv',row.names = FALSE)

indeces_mean<-indeces_data%>%group_by(ESC)%>% 
  summarise(
    mean_Freshwater_access=mean(Freshwater_access),
    mean_Tree_cover=mean(Tree_cover),
    mean_Tree_diversity=mean(Tree_diversity),
    mean_Tree_diversity=mean(Tree_diversity),
    mean_Rainfall=mean(Rainfall),
    mean_Soil=mean(Soil),
    mean_HH_members=mean(HH_members),
    mean_Head_gender=mean(Head_gender),
    mean_Head_age=mean(Head_age),
    mean_Head_education=mean(Head_education),
    mean_Labor_hire=mean(Labor_hire),
    mean_Ag_trainings=mean(Ag_trainings),
    mean_Info_network=mean(Info_network),
    mean_Market_network=mean(Market_network),
    mean_Implements=mean(Implements),
    mean_Comm_infra=mean(Comm_infra),
    mean_Transport_mode=mean(Transport_mode),
    mean_Road_state=mean(Road_state),
    mean_Livestock_holding=mean(Livestock_holding),
    mean_Land_holding=mean(Land_holding),
    mean_Land_tenure=mean(Land_tenure),
    mean_Crop_salepoint=mean(Crop_salepoint),
    mean_Loan_access=mean(Loan_access),
    mean_Debt_status=mean(Debt_status),
    mean_CIF=mean(CIF),
    mean_CPF=mean(CPF),
    mean_nc_si=mean(nc_si),
    mean_hc_si=mean(hc_si),
    mean_sc_si=mean(sc_si),
    mean_pc_si=mean(pc_si),
    mean_fc_si=mean(fc_si),
    mean_hdc_si=mean(hdc_si),
    .groups = 'drop')%>%as.data.frame()
indeces_mean

indeces_sd<-indeces_data%>%group_by(ESC)%>% 
  summarise(
    sd_Freshwater_access=sd(Freshwater_access),
    sd_Tree_cover=sd(Tree_cover),
    sd_Tree_diversity=sd(Tree_diversity),
    sd_Tree_diversity=sd(Tree_diversity),
    sd_Rainfall=sd(Rainfall),
    sd_Soil=sd(Soil),
    sd_HH_members=sd(HH_members),
    sd_Head_gender=sd(Head_gender),
    sd_Head_age=sd(Head_age),
    sd_Head_education=sd(Head_education),
    sd_Labor_hire=sd(Labor_hire),
    sd_Ag_trainings=sd(Ag_trainings),
    sd_Info_network=sd(Info_network),
    sd_Market_network=sd(Market_network),
    sd_Implements=sd(Implements),
    sd_Comm_infra=sd(Comm_infra),
    sd_Transport_mode=sd(Transport_mode),
    sd_Road_state=sd(Road_state),
    sd_Livestock_holding=sd(Livestock_holding),
    sd_Land_holding=sd(Land_holding),
    sd_Land_tenure=sd(Land_tenure),
    sd_Crop_salepoint=sd(Crop_salepoint),
    sd_Loan_access=sd(Loan_access),
    sd_Debt_status=sd(Debt_status),
    sd_CIF=sd(CIF),
    sd_CPF=sd(CPF),
    sd_nc_si=sd(nc_si),
    sd_hc_si=sd(hc_si),
    sd_sc_si=sd(sc_si),
    sd_pc_si=sd(pc_si),
    sd_fc_si=sd(fc_si),
    sd_hdc_si=sd(hdc_si),
    .groups = 'drop')%>%as.data.frame()
indeces_sd

kruskal.test(Freshwater_access ~ ESC, data = indeces_data)
kruskal.test(Tree_cover ~ ESC, data = indeces_data)
kruskal.test(Tree_diversity ~ ESC, data = indeces_data)
kruskal.test(Rainfall ~ ESC, data = indeces_data)
library(tidyverse)
library(dplyr)
library(ggstatsplot)
data
ggbetweenstats(
  data = indeces_data,
  x = ESC,
  y = Rainfall,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test( Soil~ ESC, data = indeces_data)
ggbetweenstats(
  data = indeces_data,
  x = ESC,
  y = Soil,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test( HH_members~ ESC, data = indeces_data)
kruskal.test(Head_gender ~ ESC, data = indeces_data)
kruskal.test(Head_age ~ ESC, data = indeces_data)
kruskal.test(Head_education ~ ESC, data = indeces_data)
kruskal.test(Labor_hire ~ ESC, data = indeces_data)
kruskal.test(Ag_trainings ~ ESC, data = indeces_data)
kruskal.test(Info_network ~ ESC, data = indeces_data)
kruskal.test(Market_network ~ ESC, data = indeces_data)
ggbetweenstats(
  data = indeces_data,
  x = ESC,
  y = Market_network,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(Implements ~ ESC, data = indeces_data)
kruskal.test(Comm_infra ~ ESC, data = indeces_data)
kruskal.test(Transport_mode ~ ESC, data = indeces_data)
kruskal.test(Road_state ~ ESC, data = indeces_data)
kruskal.test(Livestock_holding ~ ESC, data = indeces_data)
kruskal.test(Land_holding ~ ESC, data = indeces_data)
kruskal.test(Land_tenure ~ ESC, data = indeces_data)
kruskal.test(Crop_salepoint ~ ESC, data = indeces_data)
kruskal.test(Loan_access ~ ESC, data = indeces_data)
kruskal.test(Debt_status ~ ESC, data = indeces_data)
kruskal.test(nc_si ~ ESC, data = indeces_data)
kruskal.test(hc_si ~ ESC, data = indeces_data)
ggbetweenstats(
  data = indeces_data,
  x = ESC,
  y = hc_si,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(sc_si ~ ESC, data = indeces_data)
ggbetweenstats(
  data = indeces_data,
  x = ESC,
  y = sc_si,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(pc_si ~ ESC, data = indeces_data)
kruskal.test(fc_si ~ ESC, data = indeces_data)
ggbetweenstats(
  data = indeces_data,
  x = ESC,
  y = fc_si,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)
kruskal.test(hdc_si ~ ESC, data = indeces_data)
kruskal.test(CIF ~ ESC, data = indeces_data)
kruskal.test(CPF ~ ESC, data = indeces_data)






