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

#--LC-DM-Datasets gathering
#Calling LC data
lc<-read.csv('./Data/HH_indeces.csv')
colnames(lc)
View(lc)
dim(lc)
#calling Mi data
mi_data<-read.csv("./Data/HH_AII_KE.csv")
colnames(mi_data)
mi_data<-mi_data[c("HH_ID_Number","CIF","CPF")]
mi_data$CIF<-norm(mi_data$CIF)
mi_data$CPF<-norm(mi_data$CPF)
dim(mi_data)
mi_data
as.data.frame(colSums(is.na(mi_data[,-c(1)])))
library('dplyr')
#merge
lc<-lc%>%full_join(mi_data,by="HH_ID_Number")
colnames(lc)
dim(lc)
as.data.frame(colSums(is.na(lc[,-c(1)])))
#impute 
#KNN imputation
library(bnstruct)
colnames(lc)
imputed_lc<-c()
imputed_lc[]<- lapply(lc, function(x) as.numeric(as.numeric(x)))
imputed_lc<-as.matrix(lc)
str(imputed_lc)
imputed_lc<-knn.impute(
  imputed_lc[,-1],
  k = 22,
  #cat.var = 1:ncol(imputed_FC[,-1]),
  to.impute = 1:nrow(imputed_lc),
  using = 1:nrow(imputed_lc[,-1])
)
imputed_lc<-as.data.frame(imputed_lc)
imputed_lc<-cbind(lc$HH_ID_Number,imputed_lc)
names(imputed_lc)[1]<-'HH_ID_Number'
as.data.frame(colSums(is.na(imputed_lc[,-c(1)])))
lc<-imputed_lc
lc<-na.omit(lc)
dim(lc)
#View(imputed_lc)
#View(lc)
lc1<-lc[c("HH_ID_Number","nc_si","hc_si","sc_si","pc_si","fc_si","hdc_si","CIF","CPF")]
lc<-lc[c("HH_ID_Number","nc_si","hdc_si","CIF","CPF")]
#Merging the two datasets
library('dplyr')
data<-lc
colnames(data)
names(data)[2]<-'NC'
names(data)[3]<-'HDC'
names(data)[4]<-'CII'
names(data)[5]<-'SMI'
data
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
init <- somInit(X, 4,4)
# build model
## Train SOM
sec.som <- kohonen::som(X, grid = kohonen::somgrid(4,4), radius = c(2.65,-2.65), 
                        dist.fcts = "sumofsquares", init = init)
plot(sec.som, type='codes',palette.name = rainbow)
plot(sec.som, type = "mapping")
#plot(sec.som)
somQuality(sec.som,X)
#no need for superclasses
sec.som$codes[1]
sec.som$unit.classif
sec.som$codes
#aweSOMscreeplot(som = sec.som, method = "pam", nclass = 3)
aweSOMscreeplot(som = sec.som, method = "pam", nclass = 3)
superclust_pam <- cluster::pam(sec.som$codes[[1]], 3)
superclasses_pam <- superclust_pam$clustering
superclasses_pam
#silhouette plot for 4 clusters
aweSOMsilhouette(sec.som, superclasses_pam)
#hierarchical clustering
superclust_hclust <- hclust(dist(sec.som$codes[[1]]), "complete")
superclust_hclust
superclasses_hclust <- cutree(superclust_hclust, 3)
aweSOMdendrogram(clust = superclust_hclust, nclass = 3)
# ## Barplot (numeric variables)
colnames(data)
aweSOMplot(som = sec.som, type = 'Circular', data = X,
           variables= c("NC","HDC",'CII','SMI'),
           superclass = superclasses_pam)

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
clusters<-as.data.frame(superclasses_pam)
clusters
cvc<-cbind(codes,clusters)
cvc$code_vector<-1:nrow(cvc)
cvc
#matching code vectors to superclasses
cv_data<-cv_data%>%full_join(cvc,by='code_vector')
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
write.csv(cv_data,'./Data/som_LC_data.csv',row.names = FALSE)
write.csv(cv_data,'./GIS/som_LC_data.csv',row.names = FALSE)

#clusters----
#**Summary statistics of variables in clusters**
library("dplyr")
#Clusters
clusters<-table(cv_data[c('superclasses_pam')])
clusters
clusters_percent <- (clusters /500)*100   # Create percentage table
clusters_percent
#---Data prep for lc & dm boxplots-----
colnames(cv_data)
dim(cv_data)
head(cv_data)
oldvals <- c(1,2,3)
newvals <- c('LSC1','LSC2','LSC3')
cv_data$LSC<-newvals[match(cv_data$superclasses_pam, oldvals)]
colnames(cv_data)
lc_dm<-cv_data[c("LSC","NC","HDC","CII","SMI")]
dim(lc_dm)
lc_dm<-as.data.frame(t(lc_dm))
colnames(lc_dm)
dim(lc_dm)
lc_dm<-as.data.frame(lc_dm)
lc_dm$Variable<-row.names(lc_dm) 
lc_dm$LSC<-row.names(lc_dm)
lc_dm<-as.data.frame(lc_dm)
colnames(lc_dm)
dim(lc_dm)
colnames(lc_dm)
#View(lc_dm)
#NC
nc<-filter(lc_dm,Variable =='NC'| LSC== "LSC")
nc<-as.data.frame(t(nc))
nc$Variable<-'Natural capital'
nc<-nc[-c(501:502),]
nc$HH_ID_Number<-cv_data$HH_ID_Number
names(nc)[2]<-'Value'
nc$Value<-as.numeric(nc$Value)
dim(nc)

#HC
hdc<-filter(lc_dm,Variable =='HDC'| LSC== "LSC")
hdc<-as.data.frame(t(hdc))
hdc$Variable<-'Human-derived capital'
hdc<-hdc[-c(501:502),]
hdc$HH_ID_Number<-cv_data$HH_ID_Number
names(hdc)[2]<-'Value'
hdc$Value<-as.numeric(hdc$Value)
dim(hdc)


#CPF
cpf<-filter(lc_dm,Variable =='SMI'| LSC== "LSC")
cpf<-as.data.frame(t(cpf))
cpf$Variable<-'Soil management intensity'
cpf<-cpf[-c(501:502),]
cpf$HH_ID_Number<-cv_data$HH_ID_Number
names(cpf)[2]<-'Value'
cpf$Value<-as.numeric(cpf$Value)
dim(cpf)

#CII
cif<-filter(lc_dm,Variable=='CII'| LSC== "LSC")
cif<-as.data.frame(t(cif))
cif$Variable<-'Combined input intensity'
cif<-cif[-c(501:502),]
cif$HH_ID_Number<-cv_data$HH_ID_Number
names(cif)[2]<-'Value'
cif$Value<-as.numeric(cif$Value)
dim(cif)

all<-rbind(nc,hdc,cif,cpf)
all<- all[!duplicated(as.list(all))]
all$Value<-as.numeric(all$Value)
colnames(all)
unique(all$Variable)

# #---Barplot LC contribution----
all
oldvals <- c("Natural capital","Human-derived capital","Combined input intensity","Soil management intensity")
newvals <- c("Natural capital","Human-derived capital","Combined input intensity","Soil management intensity")
all$Capitals_Intensity<-newvals[match(all$Variable, oldvals)]

library(ggplot2)
library(ggpubr)
library(viridis)
library(RColorBrewer)
library(wesanderson)
#LC plot
lc_bar_lc<-ggplot(all,aes(x = LSC , y = Value, fill=Capitals_Intensity)) + scale_fill_brewer(palette = "Dark2")+
  geom_bar(position="stack",stat="identity")+ggtitle("Livelihood/intensification strategy clusters")+coord_flip()
lc_bar_lc

#------Overlap analysis 
#loading LC clusters data
lc_clusters<-read.csv(paste('./Data/som_LC_data.csv', sep=""))
colnames(lc_clusters)
lc_clusters<-lc_clusters[c("HH_ID_Number","superclasses_pam")]
names(lc_clusters)[2]<-'LSC'
oldvals <- c(1,2,3)
newvals <- c('LSC1','LSC2','LSC3')
lc_clusters$LSC<-newvals[match(lc_clusters$LSC, oldvals)]
#loading ESB cluster data
es_clusters<-read.csv(paste('./Data/som_ES_data.csv', sep=""))
colnames(es_clusters)
es_clusters<-es_clusters[c("HH_ID_Number","code_vector")]
names(es_clusters)[2]<-'ESC'
es_clusters
both_clusters<-lc_clusters%>%left_join(es_clusters, by="HH_ID_Number")
dim(both_clusters)
both_clusters
oldvals <- c(1,2,3)
newvals <- c('ESC1','ESC2','ESC3')
both_clusters$ESC<-newvals[match(both_clusters$ESC, oldvals)]
both_clusters
both_clusters$combinations<- paste0(both_clusters$LSC, "_", both_clusters$ESC)
library(tidyverse)
n_distinct(data.frame(both_clusters$LSC, both_clusters$ESC))
type<-table(both_clusters[c('combinations')])
type
type_percent <- (type/500)*100   # Create percentage table
type_percent

#--NC & HDC & MI -Boxplots
library("cowplot") 
library("ggplot2")
colnames(lc1)
colnames(mi_data)
colnames(lc_clusters)
lc_plot<-lc1%>%left_join(lc_clusters,by = "HH_ID_Number")
colnames(lc_plot)
lc_plot<-lc_plot[c("HH_ID_Number","nc_si","hdc_si","CIF","CPF","LSC")]
lc_plot$LSC<-as.factor(lc_plot$LSC)
colnames(lc_plot)
names(lc_plot)[2]<-'NC'
names(lc_plot)[3]<-'HDC'
names(lc_plot)[4]<-'CII'
names(lc_plot)[5]<-'SMI'
colnames(lc_plot)
#NC
nc_plot<-ggplot(lc_plot, aes(LSC, y=NC)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Natural capital")
nc_plot+stat_compare_means()
library(ggstatsplot)
nc_k<-ggbetweenstats(
  data = lc_plot,
  x = LSC,
  y = NC,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
nc_k

#HDC
hdc_plot<-ggplot(lc_plot, aes(LSC, y=HDC)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Human-derived capital")
hdc_plot+stat_compare_means()
library(ggstatsplot)
hdc_k<-ggbetweenstats(
  data = lc_plot,
  x = LSC,
  y = HDC,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
hdc_k

#CII
cii_plot<-ggplot(lc_plot, aes(LSC, y=CII)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Combined input intensity")
cii_plot+stat_compare_means()
library(ggstatsplot)
cii_k<-ggbetweenstats(
  data = lc_plot,
  x = LSC,
  y = CII,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
cii_k

#SMI
smi_plot<-ggplot(lc_plot, aes(LSC, y=SMI)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Soil management intensity")
smi_plot+stat_compare_means()
library(ggstatsplot)
smi_k<-ggbetweenstats(
  data = lc_plot,
  x = LSC,
  y = SMI,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE
)
smi_k
nc_k
k_graph<-plot_grid(nc_k,hdc_k,cii_k,smi_k,         # Draw grid of plots
                     ncol = 2,
                     nrow = 2, 
                     align = "v")
k_graph
# k_graph_1<-plot_grid(nc_k,hdc_k,          # Draw grid of plots
#           ncol = 1,
#           nrow = 2, 
#           align = "v")
# k_graph_2<-plot_grid(cii_k,smi_k,           # Draw grid of plots
#                      ncol = 1,
#                      nrow = 2, 
#                      align = "v")
# k_graph_2
ggsave("lis_boxplot_all.pdf", k_graph, path = "./Plots")
# ggsave("lis_boxplot_lc.pdf", k_graph_1, path = "./Plots")
# ggsave("lis_boxplot_id.pdf", k_graph_2, path = "./Plots")
ggsave("lis_boxplot_all.jpg", k_graph,
       width =22, height=28, units =c("cm"),dpi=400,path = "./Plots")

#tables for mi
mi_data<-read.csv("./Data/HH_AII_KE.csv")
colnames(mi_data)
mi_data<-mi_data[c("HH_ID_Number","CIF","CPF")]
mi<-mi_data%>%left_join(lc_clusters, by='HH_ID_Number')
mi
lsc1<-filter(mi,LSC =='LSC1')
lsc1
lsc2<-filter(mi,LSC =='LSC2')
lsc3<-filter(mi,LSC =='LSC3')
lsc1 <- lsc1 %>% group_by(CIF) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
lsc1
lsc2 <- lsc2 %>% group_by(CIF) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
lsc2
lsc3 <- lsc3 %>% group_by(CIF) %>% 
  summarise(total_count=n(),
            .groups = 'drop')
lsc3

#+stat_compare_means()

