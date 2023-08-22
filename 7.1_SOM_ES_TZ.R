#---setups-----
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

#Loading VS merging file **confidential
#VS_MergingFile<-read.csv("/Users/tvenort/Desktop/VS_MergingFile.csv")
VS_MergingFile<-read.csv("/Users/taishavenort/Desktop/VS_MergingFile.csv")
#VS_MergingFile<-VS_MergingFile[c(3,2,4,5)]
VS_MergingFile$lands_eplot<-paste(VS_MergingFile$landscape_no,"_",VS_MergingFile$eplot_no)
VS_MergingFile
#reomving row with eplot NA & 55 & 0
library("dplyr")
VS_MergingFile<-filter(VS_MergingFile, eplot_no != 55 & eplot_no != "NA" & eplot_no != 0 )
#View(VS_MergingFile)
#Dropping rows of 5 inexistant household 
VS_MergingFile<-VS_MergingFile[!(VS_MergingFile$hh_refno =="TZA-L10-H25" | VS_MergingFile$hh_refno =="TZA-L10-H09"| 
               VS_MergingFile$hh_refno =="TZA-L18-H29"| VS_MergingFile$hh_refno=="TZA-L19-H11"| VS_MergingFile$hh_refno  =="TZA-L19-H19"),]
colnames(VS_MergingFile)
dim(VS_MergingFile)
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

#---Data-----------------------------
#loading es data
es.data<-read.csv(paste("./8_Analyses/Datasets/es_data_agl_final.csv", sep=""))
dim(es.data)
es.data$lands_eplot<-paste(es.data$landscape_no,"_",es.data$eplot_no)
colnames(es.data)
#loading  maize yield data
yield<-read.csv(paste(output_directory,"/Yield/yield_data_hh.csv", sep=""))
colnames(yield)
yield_m<-filter(yield, crop_name == 'Maize')
names(yield_m)[6]<-'Maize_yield'
colnames(yield_m)
yield_m<-yield_m[c(2,6)]
yield_m
#New TOP hh dataframe
yield_hh<- VS_MergingFile%>%group_by(hh_refno)
yield_hh
library(dplyr)
yield_m<-full_join(yield_hh, yield_m, by ='hh_refno')
colnames(yield_m)
yield_m<-yield_m[c(5,2,4,12)]
dim(yield_m)
#yield_m<-yield_m[!duplicated(yield_m$hh_refno), ]
yield_m$Maize_yield<-yield_m$Maize_yield%>%replace(is.na(.), 0)
yield_m
yield_m$Maize_yield<-as.numeric(yield_m$Maize_yield)
yield_m$Maize_yield_s<-mib(yield_m$Maize_yield)
#loading rice yield data
yield<-read.csv(paste(output_directory,"/Yield/yield_data_hh.csv", sep=""))
colnames(yield)
yield_r<-filter(yield, crop_name == 'Paddy')
yield_r
names(yield_r)[6]<-'Rice_yield'
colnames(yield_r)
yield_r<-yield_r[c(2,6)]
library(dplyr)
yield_r<-full_join(yield_hh, yield_r, by ='hh_refno')
colnames(yield_r)
yield_r<-yield_r[c(5,12)]
#yield_r<-yield_r[!duplicated(yield_r$hh_refno), ]
yield_r$Rice_yield<-yield_r$Rice_yield%>%replace(is.na(.), 0)
yield_r$Rice_yield<-as.numeric(yield_r$Rice_yield)
yield_r$Rice_yield_s<-mib(yield_r$Rice_yield)
yield_r
cereal<-yield_m%>%full_join(yield_r, by='hh_refno')
colnames(cereal)
write.csv(cereal,'./8_Analyses/Datasets/cereal_agl_final.csv',row.names = FALSE)
cereal_mean<-cereal%>%group_by(landscape_no)%>% 
  summarise(
    mean_maize=mean(Maize_yield),
    mean_rice=mean(Rice_yield),
    .groups = 'drop')%>%as.data.frame()
cereal_mean
cereal<-cereal[c(1,4:7)]
cereal

#loading capital indicator data that has been newly normalized
lc_data<-read.csv(paste("./8_Analyses/Datasets/HH_LCA_DM_norm.csv", sep=""))
dim(lc_data)
lc_data<-lc_data[,-c(2:3)]
colnames(lc_data)
#Merging the two datasets
library('dplyr')
data<-es.data%>%left_join(lc_data,by="hh_refno")%>%left_join(cereal,by="hh_refno")
dim(data)
colnames(data)
data<- data[!duplicated(data$hh_refno), ]
colnames(data)
#View(data)
data<-na.omit(data)
#data<-na.omit(data1)
data<-data[c("hh_refno",'landscape_no','eplot_no',"lands_eplot",
             "Maize_yield_s","Rice_yield_s", 
            "Carbon_storage","Water_storage",
             "Nutrient_supply")]
dim(data)
head(data)
#---SOM Implementation------------
dim(data)
str(data)
X<-data[,-c(1:4)]
X<-as.matrix(X)
#View(X)
dim(X)
colnames(X)
#https://www.r-bloggers.com/2021/04/self-organizing-maps-in-r-supervised-vs-unsupervised/
#https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
#m=110
library(aweSOM)
### RNG Seed (for reproducibility)
set.seed(1465)
### Initialization (PCA grid)
init <- somInit(X, 6, 6)
# build model
## Train SOM
sec.som <- kohonen::som(X, grid = kohonen::somgrid(6,6), radius = c(2.65,-2.65), 
                        dist.fcts = "sumofsquares", init = init)
plot(sec.som, type='codes',palette.name = rainbow)
plot(sec.som, type = "mapping")
#plot(sec.som)
somQuality(sec.som,X)

#Choosing number of superclasses
# aweSOMscreeplot(sec.som,nclass = 4,
#                 method = c("hierarchical", "pam"),
#                 hmethod = c("complete", "ward.D2", "ward.D", "single",
#                             "average", "mcquitty", "median",
#                             "centroid"))
# aweSOMscreeplot(sec.som,nclass = 4,
#                 method = c( "hierarchical","pam"))
#pam clustering
aweSOMscreeplot(som = sec.som, method = c( "hierarchical","pam"), nclass = 4)
superclust_pam <- cluster::pam(sec.som$codes[[1]], 4)
superclasses_pam <- superclust_pam$clustering
superclasses_pam
#silhouette plot for 4 clusters
aweSOMsilhouette(sec.som, superclasses_pam)
#hierarchical clustering
superclust_hclust <- hclust(dist(sec.som$codes[[1]]), "complete")
superclust_hclust
superclasses_hclust <- cutree(superclust_hclust, 4)
aweSOMdendrogram(clust = superclust_hclust, nclass = 4)
# ## Barplot (numeric variables)
aweSOMplot(som = sec.som, type = 'Circular', data = X,
           variables= c("Maize_yield_s","Rice_yield_s", "Carbon_storage",
                        "Water_storage",
                        "Nutrient_supply"), 
           superclass = superclasses_pam)
#Linking code vectors to dataset
code_vectors<-as.data.frame(sec.som$unit.classif)
names(code_vectors)[1]<-'code_vector'
code_vectors
#dataset with code vectors
cv_data<-cbind(code_vectors,data)
colnames(cv_data)
cv_data<-cv_data[c(2,3,4,5,1)]
cv_data
#linking code vectors to superclasses
codes<-as.data.frame(sec.som$codes)
codes
clusters<-as.data.frame(superclasses_pam )
clusters
cvc<-cbind(codes,clusters)
cvc$code_vector<-1:nrow(cvc)
cvc
#matching code vectors to superclasses
library(dplyr)
cv_data<-cv_data%>%left_join(cvc,by='code_vector')
#View(cv_data)
dim(cv_data)
coord<-read.csv(paste('./8_Analyses/Spatial_analyses/Eplots_coordinates.csv', sep=""))
colnames(coord)
cord_data<-coord%>%left_join(cv_data,by='lands_eplot')
#cord_data<-na.omit(cord_data)
dim(cord_data)
write.csv(cv_data,'./8_Analyses/Datasets/som_ES_data.csv',row.names = FALSE)
write.csv(cord_data,'./8_Analyses/Spatial_analyses/som_ES_data.csv',row.names = FALSE)
dim(cv_data)
head(cv_data)
#clusters
clusters<-table(cv_data[c('superclasses_pam')])
clusters
clusters_percent <- (clusters /210)*100   # Create percentage table
clusters_percent
#-----Distribution of esbs in landscapes---------
#Landscape 3
cv_l3<-filter(cv_data, landscape_no == 'L03')
cv_l3
cv_l3<-cv_l3%>%count(superclasses_pam)
cv_l3
#Landscape 10
cv_l10<-filter(cv_data, landscape_no == 'L10')
cv_l10
cv_l10<-cv_l10%>%count(superclasses_pam)
cv_l10
#Landscape 11
cv_l11<-filter(cv_data, landscape_no == 'L11')
cv_l11
cv_l11<-cv_l11%>%count(superclasses_pam)
cv_l11
# #Landscape 18
cv_l18<-filter(cv_data, landscape_no == 'L18')
cv_l18
cv_l18<-cv_l18%>%count(superclasses_pam)
cv_l18
#Landscape 19
cv_l19<-filter(cv_data, landscape_no == 'L19')
cv_l19
cv_l19<-cv_l19%>%count(superclasses_pam)
cv_l19
#Landscape 20
cv_l20<-filter(cv_data, landscape_no == 'L20')
cv_l20
cv_l20<-cv_l20%>%count(superclasses_pam)
cv_l20
#Landscape 22
cv_l22<-filter(cv_data, landscape_no == 'L22')
cv_l22
cv_l22<-cv_l22%>%count(superclasses_pam)
cv_l22
#---Data prep for lc & dm boxplots-----
colnames(cv_data)
head(cv_data)
oldvals <- c(1,2,3,4)
newvals <- c('ESC1','ESC2','ESC3','ESC4')
cv_data$ESC<-newvals[match(cv_data$superclasses_pam, oldvals)]
colnames(cv_data)
lc_dm<-cv_data[c("ESC","Maize_yield_s","Rice_yield_s","Carbon_storage",
                 "Water_storage",
                 "Nutrient_supply")]
dim(lc_dm)
lc_dm<-as.data.frame(t(lc_dm))
colnames(lc_dm)
lc_dm<-as.data.frame(lc_dm)
lc_dm$Variable<-row.names(lc_dm) 
lc_dm$ESC<-row.names(lc_dm)
lc_dm<-as.data.frame(lc_dm)
colnames(lc_dm)
dim(lc_dm)
lc_dm

#Maize yield
my<-filter(lc_dm,Variable =='Maize_yield_s'| ESC =='ESC')
my<-as.data.frame(t(my))
my$Variable<-'Maize production'
my<-my[-c(211:212),]
my$land_eplot<-cv_data$land_eplot
names(my)[2]<-'Value'
dim(my)
my$Value<-as.numeric(my$Value)

#Rice yield
ry<-filter(lc_dm,Variable =='Rice_yield_s'| ESC =='ESC')
ry<-as.data.frame(t(ry))
ry$Variable<-'Rice production'
ry<-ry[-c(211:212),]
ry$land_eplot<-cv_data$land_eplot
names(ry)[2]<-'Value'
dim(ry)
ry$Value<-as.numeric(ry$Value)

#carbon storage
cs<-filter(lc_dm,Variable =='Carbon_storage'|ESC =='ESC')
cs<-as.data.frame(t(cs))
cs$Variable<-'Soil carbon storage'
cs<-cs[-c(211:212),]
cs$land_eplot<-cv_data$land_eplot
names(cs)[2]<-'Value'
dim(cs)
cs$Value<-as.numeric(cs$Value)
#Water retention
ws<-filter(lc_dm,Variable =='Water_storage'| ESC =='ESC')
ws<-as.data.frame(t(ws))
ws$Variable<-'Soil water storage'
ws<-ws[-c(211:212),]
ws$land_eplot<-cv_data$land_eplot
names(ws)[2]<-'Value'
dim(ws)
ws$Value<-as.numeric(ws$Value)

#Nutrient supply
nr<-filter(lc_dm,Variable =='Nutrient_supply'| ESC =='ESC')
nr<-as.data.frame(t(nr))
nr$Variable<-'Soil nutrient supply'
nr<-nr[-c(211:212),]
nr$land_eplot<-cv_data$land_eplot
names(nr)[2]<-'Value'
nr$Value<-as.numeric(nr$Value)
dim(nr)
#rowbind
all<-rbind(my,ry,cs,ws,nr)
all<- all[!duplicated(as.list(all))]
all$Value<-as.numeric(all$Value)
#View(all)
colnames(all)
unique(all$Variable)

oldvals <- c("Soil water storage","Soil carbon storage", 
            "Soil nutrient supply",
             "Maize production","Rice production")
newvals <- c("Soil water storage","Soil carbon storage", 
             "Soil nutrient supply",
             "Crop production-Maize","Crop production-Rice")
all$Ecosystem_services<-newvals[match(all$Variable, oldvals)]
#View(all)

#---Barplot ES contribution----
all
library('ggplot2')
lc_bar_es<-ggplot(all, aes(x = ESC , y = Value, fill=Ecosystem_services))+ scale_fill_brewer(palette = "Set2")+
  geom_bar(position="stack",stat="identity")+ggtitle("SAGCOT Ecosystem services clusters")+coord_flip()
lc_bar_es
#----ESC boxplots------------------------
dim(cv_data)
cluster<-cv_data[c("hh_refno","ESC")]
cluster
cereal<-cereal%>%left_join(cluster, by="hh_refno")
cereal
#install.packages("cowplot")        # Install cowplot package
library("cowplot") 
library("ggplot2")
#install.packages("ggpur")
library("ggpubr")
#Maize
yieldm_plot<-ggplot(cereal, aes(ESC, y=Maize_yield)) + 
  geom_boxplot()+ ylab("Maize yield (kg/ha)")+ggtitle("Maize production")
yieldm_plot
library(ggstatsplot)
my_kr<-ggbetweenstats(
  data = cereal,
  x = ESC,
  y = Maize_yield,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
my_kr
cereal_mean<-cereal%>%group_by(ESC)%>% 
  summarise(
    mean_maize=mean(Maize_yield),
    mean_rice=mean(Rice_yield),
    .groups = 'drop')%>%as.data.frame()
cereal_mean
#Rice
yieldr_plot<-ggplot(cereal, aes(ESC, y=Rice_yield)) + 
  geom_boxplot()+ ylab("Rice yield (kg/ha)")+ggtitle("Rice production")
yieldr_plot
library(ggstatsplot)
ry_kr<-ggbetweenstats(
  data = cereal,
  x = ESC,
  y = Rice_yield,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
ry_kr

colnames(data)
cs<-data[c("hh_refno","Carbon_storage")]
ws<-data[c("hh_refno","Water_storage")]
ns<-data[c("hh_refno","Nutrient_supply")]
cs<-cs%>%left_join(cluster, by="hh_refno")
ws<-ws%>%left_join(cluster, by="hh_refno")
ns<-ns%>%left_join(cluster, by="hh_refno")
#View(cs)
#carbon storage
cs_plot<-ggplot(cs, aes(ESC, y=Carbon_storage)) + 
  geom_boxplot()+ ylab("Level")+
  ggtitle("Soil carbon storage")
cs_plot
library(ggstatsplot)
cs_kr<-ggbetweenstats(
  data = cs,
  x = ESC,
  y = Carbon_storage,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
cs_kr

#water storage
ws_plot<-ggplot(ws, aes(ESC, y=Water_storage)) + 
  geom_boxplot(outlier.shape=NA)+ ylab("Level")+
   ggtitle("Soil water storage")
ws_plot
library(ggstatsplot)
ws_kr<-ggbetweenstats(
  data = ws,
  x = ESC,
  y = Water_storage,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = ,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
ws_kr

#nutrient supply
ns_plot<-ggplot(ns, aes(ESC, y=Nutrient_supply)) + 
  geom_boxplot(outlier.shape=NA)+ ylab("Level")+
  ggtitle("Soil nutrient supply")
ns_plot
library(ggstatsplot)
ns_kr<-ggbetweenstats(
  data = ns,
  x = ESC,
  y = Nutrient_supply,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  #pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
  
)
ns_kr
# plot_grid(yieldm_plot,yieldr_plot,cs_plot,ws_plot,ns_plot,             # Draw grid of plots
#           ncol = 2,
#           nrow = 3)
kr_graphs<-plot_grid(my_kr,ry_kr,cs_kr,ws_kr,ns_kr,            # Draw grid of plots
                    ncol = 3,
                    nrow = 2)
kr_graphs

# figure_box_all<-ggarrange(my_kr+ ylim(0, 1),
#                           ry_kr + theme(axis.text.y = element_blank(),
#                                         axis.ticks.y = element_blank(),
#                                         axis.title.y = element_blank()),
#                           cs_kr+ theme(axis.text.y = element_blank(),
#                                             axis.ticks.y = element_blank(),
#                                             axis.title.y = element_blank()),
#                           ws_kr+ theme(axis.text.y = element_blank(),
#                                        axis.ticks.y = element_blank(),
#                                        axis.title.y = element_blank()),
#                           ns_kr+ theme(axis.text.y = element_blank(),
#                                        axis.ticks.y = element_blank(),
#                                        axis.title.y = element_blank()),
#                          labels = c(), common.legend = TRUE,ncol = 2, nrow = 3)
# figure_box_all

# kr_graph_prov<-plot_grid(my_kr,ry_kr,          # Draw grid of plots
#                      ncol = 2,
#                      nrow = 1)
# kr_graph_rs<-plot_grid(cs_kr,ws_kr,ns_kr,            # Draw grid of plots
#                      ncol = 3,
#                      nrow = 1)
ggsave("es_boxplot_all.jpg", kr_graphs,
       width =20, height=32, units =c("cm"),dpi=300,path = "./8_Analyses/Plots")
# ggsave("es_boxplot_prov.pdf", kr_graph_prov, path = "./8_Analyses/Plots")
# ggsave("es_boxplot_rs.pdf", kr_graph_rs, path = "./8_Analyses/Plots")