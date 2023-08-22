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

#---Data-----------------------------
#--ES Datasets gathering
#calling es data
es<-read.csv("./Data/HH_es_data_KE.csv")
colnames(es)
es<-es[c("HH_ID_Number","Maize_yield","pot_yield","Nutrient_supply",
         "C_deficit_indicator","Soil_water_storage")]
View(es)
dim(es)
data<-es
# #--LC-DM-Datasets gathering
# #Calling LC data
# lc<-read.csv("./Data/HH_LCA_KE.csv")
# colnames(lc)
# #Merging the two datasets
# library('dplyr')
# data<-lc%>%left_join(es,by='HH_ID_Number')
# colnames(data)
as.data.frame(colSums(is.na(data[,-c(1)])))
#data<-na.omit(data)
#View(data)
dim(data)
#---SOM Implementation------------
data<-na.omit(data)
dim(data)
str(data)
X<-data[,-c(1)]
X<-as.matrix(X)
#View(X)
dim(X)
#----SOM Implementation----
#https://www.r-bloggers.com/2021/04/self-organizing-maps-in-r-supervised-vs-unsupervised/
#https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
#m=5*sqrt(n)
#----SOM Implementation 
library(aweSOM)
### RNG Seed (for reproducibility)
set.seed(1465)
### Initialization (PCA grid)
init <- somInit(X, 3,1)
# build model
## Train SOM
sec.som <- kohonen::som(X, grid = kohonen::somgrid(3,1), radius = c(2.65,-2.65), 
                        dist.fcts = "sumofsquares", init = init)
plot(sec.som, type='codes',palette.name = rainbow)
plot(sec.som, type = "mapping")
#plot(sec.som)
somQuality(sec.som,X)
#no need for superclasses
sec.som$codes[1]
sec.som$unit.classif
sec.som$codes
#Choosing number of superclasses
# aweSOMscreeplot(sec.som,nclass = 2,
#                 method = c("hierarchical", "pam"),
#                 hmethod = c("complete", "ward.D2", "ward.D", 
#                             "single", "average", "mcquitty", "median",
#                             "centroid"))
# # #pam clustering
# aweSOMscreeplot(som = sec.som, method = "pam", nclass = 2)
# superclust_pam <- cluster::pam(sec.som$codes[[1]], 2)
# superclasses_pam <- superclust_pam$clustering
# superclasses_pam
# #silhouette plot for 4 clusters
# aweSOMsilhouette(sec.som, superclasses_pam)
# #hierarchical clustering
# superclust_hclust <- hclust(dist(sec.som$codes[[1]]), "complete")
# superclust_hclust
# superclasses_hclust <- cutree(superclust_hclust, 3)
# aweSOMdendrogram(clust = superclust_hclust, nclass = 3)
# ## Barplot (numeric variables)
# colnames(data)
# aweSOMplot(som = sec.som, type = 'Circular', data = X,
#            variables= c("Maize_yield","soil_fertility_indicator","Nutrient_supply",
#                         "C_deficit_indicator","Soil_water_storage",
#                         superclass = superclasses_pam))


#Linking code vectors to dataset
code_vectors<-as.data.frame(sec.som$unit.classif)
names(code_vectors)[1]<-'code_vector'
code_vectors
#dataset with code vectors
cv_data<-cbind(code_vectors,data)
colnames(cv_data)
cv_data<-cv_data[c(1:2)]
cv_data<-cv_data[c(2,1)]
cv_data
#linking code vectors to superclasses
codes<-as.data.frame(sec.som$codes)
codes
#clusters<-as.data.frame(superclasses_pam)
#clusters
#cvc<-cbind(codes,cv_data)
codes$code_vector<-1:nrow(codes)
codes
#matching code vectors to superclasses
#install.packages('rlang')
#install.packages('tidyverse')
library(tidyverse)
library(dplyr)
cv_data<-cv_data%>%full_join(codes,by='code_vector')
colnames(cv_data)
View(cv_data)
#calling hh coord
coord<-read.csv("./Data/HH-coords.csv")
cv_data<-coord%>%left_join(cv_data, by='HH_ID_Number')
cv_data$aez_zones<-ifelse(cv_data$zones == 2, "sub_humid",
                   ifelse(cv_data$zones == 3, "semi_humid",
                   ifelse(cv_data$zones == 4, "semi_humid_to_arid","")))
colnames(cv_data)
cv_data<-na.omit(cv_data)
dim(cv_data)
write.csv(cv_data,'./Data/som_ES_data.csv',row.names = FALSE)
write.csv(cv_data,'./GIS/som_ES_data.csv',row.names = FALSE)
colnames(cv_data)
unique(cv_data$aez_zones)
#clusters----
#**Summary statistics of variables in clusters**
clusters<-table(cv_data[c('code_vector')])
clusters
clusters_percent <- (clusters /500)*100   # Create percentage table
clusters_percent
#-----Distribution of esbs in landscapes---------
#Sub-humid
subh<-filter(cv_data, aez_zones == 'sub_humid')
subh
subh<-subh%>%count(code_vector)
subh
#Semi-humid
semh<-filter(cv_data, aez_zones == 'semi_humid')
semh
semh<-semh%>%count(code_vector)
semh
#Semi to sub-humid
sts<-filter(cv_data, aez_zones == 'semi_humid_to_arid')
sts
sts<-sts%>%count(code_vector)
sts
#---Data prep for lc & dm boxplots-----
colnames(cv_data)
dim(cv_data)
head(cv_data)
oldvals <- c(1,2,3)
newvals <- c('ESC1','ESC2','ESC3')
cv_data$ESC<-newvals[match(cv_data$code_vector, oldvals)]
colnames(cv_data)
lc_dm<-cv_data[c("ESC", "Maize_yield","pot_yield","Nutrient_supply",
                 "C_deficit_indicator","Soil_water_storage")]
dim(lc_dm)
lc_dm<-as.data.frame(t(lc_dm))
colnames(lc_dm)
dim(lc_dm)
lc_dm<-as.data.frame(lc_dm)
lc_dm$Variable<-row.names(lc_dm) 
lc_dm$ESC<-row.names(lc_dm)
lc_dm<-as.data.frame(lc_dm)
colnames(lc_dm)
dim(lc_dm)
colnames(lc_dm)
#View(lc_dm)

#maize production
my<-filter(lc_dm,Variable =='Maize_yield'| ESC =='ESC')
my<-as.data.frame(t(my))
my$Variable<-'Maize production'
my<-my[-c(501:502),]
my$HH_ID_number<-c(1:500)
names(my)[2]<-'Value'
dim(my)
my$Value<-as.numeric(my$Value)

#potatoes production
py<-filter(lc_dm,Variable =='pot_yield'| ESC =='ESC')
py<-as.data.frame(t(py))
py$Variable<-'Potato production'
py<-py[-c(501:502),]
py$HH_ID_number<-c(1:500)
names(py)[2]<-'Value'
dim(py)
py$Value<-as.numeric(py$Value)

#carbon storage
cs<-filter(lc_dm,Variable =='C_deficit_indicator'|ESC =='ESC')
cs<-as.data.frame(t(cs))
cs$Variable<-'Soil carbon storage'
cs<-cs[-c(501:502),]
cs$HH_ID_number<-c(1:500)
names(cs)[2]<-'Value'
dim(cs)
cs$Value<-as.numeric(cs$Value)
#Water retention
ws<-filter(lc_dm,Variable =='Soil_water_storage'| ESC =='ESC')
ws<-as.data.frame(t(ws))
ws$Variable<-'Soil water storage'
ws<-ws[-c(501:502),]
ws$HH_ID_number<-c(1:500)
names(ws)[2]<-'Value'
dim(ws)
ws$Value<-as.numeric(ws$Value)
#Nutrient supply
nr<-filter(lc_dm,Variable =='Nutrient_supply'| ESC =='ESC')
nr<-as.data.frame(t(nr))
nr$Variable<-'Soil nutrient supply'
nr<-nr[-c(501:502),]
nr$HH_ID_number<-c(1:500)
names(nr)[2]<-'Value'
nr$Value<-as.numeric(nr$Value)
dim(nr)
#rowbind
all<-rbind(my,py,cs,ws,nr)
all<- all[!duplicated(as.list(all))]
all$Value<-as.numeric(all$Value)
all<-all[,-c(4)]
View(all)
#View(all)
oldvals <- c( "Soil nutrient supply",
             "Soil carbon storage", "Soil water storage",
             "Maize production","Potato production")
newvals <- c( "Soil nutrient supply",
              "Soil carbon storage", "Soil water storage",
              "Crop production-Maize","Crop production-Potato")
all$Ecosystem_services<-newvals[match(all$Variable, oldvals)]
all

#View(all_dm)
#---Barplot LC contribution----
library(ggplot2)
library(ggpubr)
library(viridis)
library(RColorBrewer)
library(wesanderson)
es_bar<-ggplot(all, aes(x = ESC , y = Value, fill=Ecosystem_services)) + scale_fill_brewer(palette = "Set2")+
  geom_bar(position="stack",stat="identity")+ggtitle("Upper Ewaso Ng'iro Ecosystem service clusters")+coord_flip()
es_bar

##Characterizing the clusters with raw data
##Analysis of raw maize data and other indicators
maize_yield<-read.csv(paste("./data/maize_yield_KE.csv", sep=""))
pot_yield<-read.csv(paste("./data/pot_yield_KE.csv", sep=""))
colnames(maize_yield)
dim(maize_yield)
dim(pot_yield)
colnames(pot_yield)
es<-es[c("HH_ID_Number","Nutrient_supply",
         "C_deficit_indicator","Soil_water_storage")]
maize_yield<-maize_yield[c("HH_ID_Number","Maize_yield")]
maize_yield
pot_yield<-pot_yield[c("HH_ID_Number","pot_yield")]
#cluster data
cluster<-read.csv(paste('./Data/som_ES_data.csv', sep=""))
colnames(cluster)
cluster
names(cluster)[5]<-'ESC'
cluster<-cluster[c(1,5)]
oldvals <- c(1,2,3)
newvals <- c("ESC1","ESC2","ESC3")
cluster$ESC<-newvals[match(cluster$ESC, oldvals)]
cluster$ESC<-as.factor(cluster$ESC)
es.all<-es%>%left_join(maize_yield, by='HH_ID_Number')%>%left_join(pot_yield,
                      by='HH_ID_Number')%>%left_join(cluster, by='HH_ID_Number')
colnames(es.all)
names(es.all)[2]<-'Nutrient_supply'
names(es.all)[3]<-'Carbon_storage'
names(es.all)[4]<-'Water_storage'
names(es.all)[5]<-'Maize_yield'
names(es.all)[6]<-'Potato_yield'
es.all
colnames(es.all)
#--ESB-Boxplots
library(ggplot2)
library("cowplot") 
library("ggplot2")
#install.packages("ggpur")
library("ggpubr")
#Maize
yieldm_plot<-ggplot(es.all, aes(ESC, y=Maize_yield)) + 
  geom_boxplot()+ ylab("Maize yield (kg/ha)")+ggtitle("Maize production")
yieldm_plot+stat_compare_means()
#install.packages('tidyverse')
library(tidyverse)
library(ggstatsplot)
mz_k<-ggbetweenstats(
  data = es.all,
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
mz_k
#maize mean accross clusters
library(dplyr)
es.all %>%
  group_by(ESC) %>%
  summarise_at(vars(Maize_yield), list(name = mean))

#Potato
yieldp_plot<-ggplot(es.all, aes(ESC, y=Potato_yield)) + 
  geom_boxplot()+ ylab("Potato yield (kg/ha)")+ggtitle("Potato production")
yieldp_plot+stat_compare_means()
pot_k<-ggbetweenstats(
  data = es.all,
  x = ESC,
  y = Potato_yield,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
pot_k
#potato mean accross clusters
library(dplyr)
es.all %>%
  group_by(ESC) %>%
  summarise_at(vars(Potato_yield), list(name = mean))
#ES
cs<-es.all[c("HH_ID_Number","Carbon_storage","ESC")]
ws<-es.all[c("HH_ID_Number","Water_storage","ESC")]
ns<-es.all[c("HH_ID_Number","Nutrient_supply","ESC")]
es.all$ESC<-as.factor(es.all$ESC)
#View(cs)
#carbon storage
cs_plot<-ggplot(cs, aes(ESC, y=Carbon_storage)) + 
  geom_boxplot()+ ylab("Level")+
  ggtitle("Soil carbon storage")
cs_plot+stat_compare_means()
cs_k<-ggbetweenstats(
  data = es.all,
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
cs_k
#water storage
ws_plot<-ggplot(ws, aes(ESC, y=Water_storage)) + 
  geom_boxplot()+ylab("Level")+
  ggtitle("Soil water storage")
ws_plot +stat_compare_means()
ws_k<-ggbetweenstats(
  data = es.all,
  x = ESC,
  y = Water_storage,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
ws_k
#nutrient supply
ns_plot<-ggplot(ns, aes(ESC, y=Nutrient_supply)) + 
  geom_boxplot()+ ylab("Level")+
  ggtitle("Soil nutrient supply")
ns_plot +stat_compare_means()
ns_k<-ggbetweenstats(
  data = es.all,
  x = ESC,
  y = Nutrient_supply,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
ns_k
library("cowplot") 
library("ggplot2")
plot_grid(yieldm_plot,yieldp_plot,cs_plot,ws_plot,ns_plot,             # Draw grid of plots
          ncol = 3,
          nrow = 2, 
          align = "v")
kr_graph1<-plot_grid(mz_k,pot_k,         # Draw grid of plots
          ncol = 2,
          nrow = 1, 
          align = "v")
kr_graph1
kr_graph2<-plot_grid(cs_k,ws_k,ns_k,          # Draw grid of plots
                     ncol = 3,
                     nrow = 2, 
                     align = "v")
kr_graph<-plot_grid(mz_k,pot_k,cs_k,ws_k,ns_k,          # Draw grid of plots
                     ncol = 3,
                     nrow = 2, 
                     align = "v")
kr_graph
#ggsave("es_boxplot_prov.pdf", kr_graph1, path = "./Plots")
#ggsave("es_boxplot_rs.pdf", kr_graph2, path = "./Plots")
#ggsave("es_boxplot_all.pdf", kr_graph, path = "./Plots")
ggsave("es_boxplot_all.jpg", kr_graph,
       width =22, height=28, units =c("cm"),dpi=400,path = "./Plots")
