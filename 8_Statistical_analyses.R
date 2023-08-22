####RQ1: ES & NC associations######
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

#---Spearman rank correlations for all possible pairs of ES in each landscape type ------
#install.packages("ggcorrplot")
#Loading all es data raw
all.data<-read.csv(paste('./8_Analyses/Datasets/es_data_all_ls_knnimp_norm.csv', sep=""))
all.data$lands_eplot<-paste(all.data$landscape_no,"_",all.data$eplot_no)
all.data
library(dplyr)
lsn.data<-filter(all.data, landscape_no == 'L0')
lsn.data
dim(lsn.data)
colnames(lsn.data)
#Soil water storage
lsn.data$Clay_icraf<-mib(lsn.data$Clay_icraf)
lsn.data$Bulk_density<-opt_bd(lsn.data$Bulk_density)
lsn.data$Depth_to_bedrock<-mib(lsn.data$Depth_to_bedrock)
lsn.data$Water_storage<-rowMeans(lsn.data[c("Clay_icraf","soil_fertility_indicator",
                                            "Bulk_density","Depth_to_bedrock")])
colnames(lsn.data)
names(lsn.data)[5]<-'Soil_fertility'
names(lsn.data)[6]<-'Carbon_storage'
lsn.data<-lsn.data[c("landscape_no","eplot_no","Carbon_storage","Water_storage")]
colnames(lsn.data)
#---Calling Ag farms data-----------
#loading eplot scale ag fields
agl.data<-read.csv(paste('./8_Analyses/Datasets/es_data_agl_final.csv', sep=""))
colnames(agl.data)
agl.data1<-agl.data[c("landscape_no", "eplot_no","Carbon_storage","Water_storage")]
agl.data2<-agl.data[c("hh_refno", "Nutrient_supply")]
dim(agl.data2)
#calling Cereal yield data----
cereal.data<-read.csv(paste('./8_Analyses/Datasets/cereal_agl_final.csv', sep=""))
cereal.data<-cereal.data[c(1,2,3,5,7)]
dim(cereal.data)
colnames(cereal.data)
#Calling LC data
lc<-read.csv(paste('./8_Analyses/Datasets/HH_indeces.csv',sep=""))
colnames(lc)
lc<-lc[c("hh_refno","nc_si_std","hdc_si_std")]
names(lc)[2]<-'NC'
names(lc)[3]<-'HDC'
lc
#calling MI data
#loading raw livelihoods and md data
mi<-read.csv(paste('./8_Analyses/Datasets/HH_imputed_data_final.csv', sep=""))
dim(mi)
colnames(mi)
mi<-mi[c("hh_refno","CIIF","CPF")]
mi$CIIF<-mib(mi$CIIF)
mi$CPF<-mib(mi$CPF)
#loading cluster assignment data
cluster<-read.csv(paste('./8_Analyses/Datasets/som_all_data.csv', sep=""))
colnames(cluster)
cluster<-cluster[c(1,4,11)]
names(cluster)[3]<-'ESC'
dim(cluster)
colnames(cluster)
#View(cluster)
#merging by landscape no and eplot for SN fields
cor_data1<-rbind(lsn.data,agl.data1)
dim(cor_data1)
#merging the rest by hhid 
cor_data2<-cluster%>%left_join(cereal.data, 
          by="hh_refno")%>%left_join(agl.data2,
           by="hh_refno")%>%left_join(lc,
          by="hh_refno")%>%left_join(mi, by="hh_refno")
dim(cor_data2)
colnames(cor_data2)
View(cor_data2)
colnames(cor_data2)
#cor_data2<-cor_data2[,-c(1)]
#Final merging with lands-eplot data
cor_data<-cor_data1%>%full_join(cor_data2,by=c('landscape_no','eplot_no'))
dim(cor_data)
cor_data$hh_refno[is.na(cor_data$hh_refno)] <-runif(sum(is.na(cor_data$hh_refno)),
                                                                  min = 1, max = 2000)
cor_data<-cor_data[!duplicated(cor_data$hh_refno), ]
colnames(cor_data)
cor_data<-cor_data[c("landscape_no","eplot_no","ESC","Carbon_storage","Water_storage",
                     "Maize_yield_s","Rice_yield_s","Nutrient_supply","NC",             
                     "HDC","CIIF","CPF")]
dim(cor_data)
oldvals <- c(1,2,3,4)
newvals <- c('ESC1','ESC2','ESC3','ESC4')
cor_data$ESC<-newvals[match(cor_data$ESC, oldvals)]
cor_data$ESC<-cor_data$ESC%>%replace(is.na(.), 'SN')
colnames(cor_data)
dim(cor_data)
View(cor_data)
#final merging
names(cor_data)[4]<-'CS'
names(cor_data)[5]<-'WS'
names(cor_data)[6]<-'MP'
names(cor_data)[7]<-'RP'
names(cor_data)[8]<-'NS'
names(cor_data)[9]<-'NC'
names(cor_data)[10]<-'HDC'
names(cor_data)[11]<-'CII'
names(cor_data)[12]<-'SMI'
colnames(cor_data)
library(ggplot2)  
library(GGally)
# p<-ggpairs(cor_data2, columns = 5:13,
#         upper = list((method = "spearman")),
#         ggplot2::aes(colour=as.character(landscape_no)))
# p +theme(text = element_text(size=12))
#plot 1 ---ES only
# cor_data_es<-cor_data[,c(1:8)]
# colnames(cor_data_es)
# ggpairs(cor_data, columns = 4:8,
#         aes(color = cor_data_es$ESC),                             # Separate data by levels of vs
#         upper = list((method = "spearman"),
#                      continuous = wrap('cor', size = 3)),
#         lower = list(combo = wrap("facethist", bins = 30, size = 5)),
#         diag = list(continuous = wrap("densityDiag",alpha = 0.5)))+
#         theme(text = element_text(size=12))
#plot 2 --- Realized yield & HDC + NC
cor_data_yield_lc<-filter(cor_data, 
                          landscape_no == 'L03'|
                            landscape_no == 'L10'|
                            landscape_no == 'L11'|
                            landscape_no == 'L18'|
                            landscape_no == 'L19'|
                            landscape_no == 'L20'|
                            landscape_no == 'L22')
dim(cor_data_yield_lc)
colnames(cor_data_yield_lc)
#cor_data_yield_lc<-cor_data[,c(1:3,6:7,9:12)]
colnames(cor_data_yield_lc)
ggpairs(cor_data_yield_lc, columns = 4:12,
        #aes(color = cor_data_yield_lc$ESC),                             # Separate data by levels of vs
        upper = list((method = "spearman"),
                     continuous = wrap('cor', size = 3)),
        lower = list(combo = wrap("facethist", bins = 30, size = 5)),
        diag = list(continuous = wrap("densityDiag",alpha = 0.5)))+
  theme(text = element_text(size=12))
#cor_data2 <-as.numeric(unlist(cor_data2))
#important source 
#https://pascal-martin.netlify.app/post/nicer-scatterplot-in-gggally/
