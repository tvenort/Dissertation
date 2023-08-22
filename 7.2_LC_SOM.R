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
# #loading es data
# es.data<-read.csv(paste("./8_Analyses/Datasets/es_data_agl_final.csv", sep=""))
# dim(es.data)
#loading capital indeces data
lc_data<-read.csv(paste("./8_Analyses/Datasets/HH_indeces.csv", sep=""))
dim(lc_data)
colnames(lc_data)
lc_data
#loading intensity data
mi<-read.csv(paste('./8_Analyses/Datasets/HH_imputed_data_final.csv', sep=""))
dim(mi)
mi
colnames(mi)
mi<-mi[c("hh_refno","CIF","CPF")]
mi$CIF<-norm(mi$CIF)
mi$CPF<-norm(mi$CPF)
#Merging the two datasets
library('dplyr')
data<-lc_data%>%left_join(mi,by=c("hh_refno"))
colnames(data)
dim(data)
data<-na.omit(data)
# data<-data[c("hh_refno","landscape_no","eplot_no","nc_si",
#              "hc_si","sc_si","pc_si","fc_si","CIIF","CPF")]
data<-data[c("hh_refno","landscape_no","eplot_no","nc_si_std",
             "hdc_si_std","CIF","CPF")]
colnames(data)
dim(data)
data
# data$nc_si<-mib(data$nc_si)
# data$hc_si<-mib(data$hc_si)
# data$sc_si<-mib(data$sc_si)
# data$pc_si<-mib(data$pc_si)
# data$fc_si<-mib(data$fc_si)
View(data)
#---SOM Implementation------------
dim(data)
str(data)
X<-data[,-c(1:3)]
X<-as.matrix(X)
#View(X)
dim(X)
colnames(X)
#----SOM Implementation----
#https://www.r-bloggers.com/2021/04/self-organizing-maps-in-r-supervised-vs-unsupervised/
#https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
#m=110
#----SOM Implementation 
library(aweSOM)
### RNG Seed (for reproducibility)
set.seed(1465)
### Initialization (PCA grid)
init <- somInit(X, 4, 4)
# build model
## Train SOM
sec.som <- kohonen::som(X, grid = kohonen::somgrid(4,4), radius = c(2.65,-2.65), 
                        dist.fcts = "sumofsquares", init = init)
plot(sec.som, type='codes',palette.name = rainbow)
plot(sec.som, type = "mapping")
#plot(sec.som)
somQuality(sec.som,X)
aweSOMscreeplot(som = sec.som, method = c("pam"), nclass = 3)
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
aweSOMplot(som = sec.som, type = 'Circular', data = X,
           variables= c("nc_si_std",
                        "hdc_si_std","CIF","CPF"), 
           superclass = superclasses_pam)
#Linking code vectors to dataset
code_vectors<-as.data.frame(sec.som$unit.classif)
names(code_vectors)[1]<-'code_vector'
code_vectors
#dataset with code vectors
cv_data<-cbind(code_vectors,data)
colnames(cv_data)
cv_data<-cv_data[c(2,3,4,1)]
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
library(dplyr)
cv_data<-cv_data%>%left_join(cvc,by='code_vector')
#View(cv_data)
dim(cv_data)
colnames(cv_data)
names(cv_data)[9]<-'LSC'
#coord<-read.csv(paste('./8_Analyses/Spatial_analyses/Eplots_coordinates.csv', sep=""))
#colnames(coord)
# cord_data<-coord%>%left_join(cv_data,by='hh_refno')
# #cord_data<-na.omit(cord_data)
# dim(cord_data)
# write.csv(cv_data,'./8_Analyses/Datasets/som_lcdm_data.csv',row.names = FALSE)
 write.csv(cv_data,'./8_Analyses/Spatial_analyses/som_lcdm_data.csv',row.names = FALSE)
# dim(cv_data)
# head(cv_data)
#clusters
clusters<-table(cv_data[c('LSC')])
clusters
clusters_percent <- (clusters /210)*100   # Create percentage table
clusters_percent

#---Data prep for lc & dm boxplots-----
colnames(cv_data)
head(cv_data)
oldvals <- c(1,2,3)
newvals <- c('LSC1','LSC2','LSC3')
cv_data$LSC<-newvals[match(cv_data$LSC, oldvals)]
colnames(cv_data)
lc_dm<-cv_data[c("LSC","nc_si_std","hdc_si_std", "CIF","CPF")]
dim(lc_dm)
lc_dm<-as.data.frame(t(lc_dm))
colnames(lc_dm)
lc_dm<-as.data.frame(lc_dm)
lc_dm$Variable<-row.names(lc_dm) 
lc_dm$LSC<-row.names(lc_dm)
lc_dm<-as.data.frame(lc_dm)
colnames(lc_dm)
dim(lc_dm)
View(lc_dm)

#NC
nc<-filter(lc_dm,Variable =='nc_si_std'| LSC== "LSC")
nc<-as.data.frame(t(nc))
nc$Variable<-'Natural capital'
nc<-nc[-c(211:212),]
nc$hh_refno<-cv_data$hh_refno
names(nc)[2]<-'Value'
nc$Value<-as.numeric(nc$Value)
dim(nc)

#HC
hdc<-filter(lc_dm,Variable =='hdc_si_std'| LSC== "LSC")
hdc<-as.data.frame(t(hdc))
hdc$Variable<-'Human-derived capital'
hdc<-hdc[-c(211:212),]
hdc$hh_refno<-cv_data$hh_refno
names(hdc)[2]<-'Value'
hdc$Value<-as.numeric(hdc$Value)
dim(hdc)

# #SC
# sc<-filter(lc_dm,Variable =='sc_si_std'| LC== "LC")
# sc<-as.data.frame(t(sc))
# sc$Variable<-'Social capital'
# sc<-sc[-c(211:212),]
# sc$hh_refno<-cv_data$hh_refno
# names(sc)[2]<-'Value'
# sc$Value<-as.numeric(sc$Value)
# dim(sc)
# 
# #PC
# pc<-filter(lc_dm,Variable =='pc_si_std'| LC== "LC")
# pc<-as.data.frame(t(pc))
# pc$Variable<-'Physical capital'
# pc<-pc[-c(211:212),]
# pc$hh_refno<-cv_data$hh_refno
# names(pc)[2]<-'Value'
# pc$Value<-as.numeric(pc$Value)
# dim(pc)
# 
# #FC
# fc<-filter(lc_dm,Variable =='sc_si_std'| LC== "LC")
# fc<-as.data.frame(t(fc))
# fc$Variable<-'Financial capital'
# fc<-fc[-c(211:212),]
# fc$hh_refno<-cv_data$hh_refno
# names(fc)[2]<-'Value'
# fc$Value<-as.numeric(fc$Value)
# dim(fc)

#CPF
cpf<-filter(lc_dm,Variable =='CPF'| LSC== "LSC")
cpf<-as.data.frame(t(cpf))
cpf$Variable<-'Soil management intensity'
cpf<-cpf[-c(211:212),]
cpf$hh_refno<-cv_data$hh_refno
names(cpf)[2]<-'Value'
cpf$Value<-as.numeric(cpf$Value)
dim(cpf)

#CII
cif<-filter(lc_dm,Variable=='CIF'| LSC== "LSC")
cif<-as.data.frame(t(cif))
cif$Variable<-'Combined input intensity'
cif<-cif[-c(211:212),]
cif$hh_refno<-cv_data$hh_refno
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
oldvals <- c("Natural capital","Human-derived capital",
             "Combined input intensity","Soil management intensity")
newvals <- c("Natural capital","Human-derived capital",
             "Combined input intensity","Soil management intensity")
all$Capitals_Intensity<-newvals[match(all$Variable, oldvals)]
all
# #Separation
# oldvals <- c("Input intensity","Soil management intensity")
# newvals <- c("Input intensity","Soil management intensity")
# all$Category_MI<-newvals[match(all$Variable, oldvals)]
# View(all)
# all_lc<-all[,-c(4,6)]
# all_lc<-filter(all_lc,Variable%in% c("Natural capital","Human capital","Social capital",           
#                                      "Physical capital","Financial capital"))
# all_lc
# all_mi<-all[,-c(4,5)]
# all_mi<-filter(all_mi,Variable%in% c("Input intensity","Soil management intensity"))
# all_mi
library(ggplot2)
library(ggpubr)
library(viridis)
library(RColorBrewer)
library(wesanderson)
#all plot 
lc_bar_lc<-ggplot(all,aes(x = LSC , y = Value, fill=Capitals_Intensity)) + scale_fill_brewer(palette = "Dark2")+
  geom_bar(position="stack",stat="identity")+ggtitle("SAGCOT Livelihood/intensification strategy clusters")+coord_flip()
lc_bar_lc

#--LSC boxplots --------
#loading LC clusters data
lc_clusters<-read.csv(paste('./8_Analyses/Spatial_analyses/som_lcdm_data.csv', sep=""))
colnames(lc_clusters)
lc_clusters<-lc_clusters[c("hh_refno","LSC")]
oldvals <- c(1,2,3)
newvals <- c('LSC1','LSC2','LSC3')
lc_clusters$LSC<-newvals[match(lc_clusters$LSC, oldvals)]
#merging
data<-lc_clusters%>%left_join(lc_data, by="hh_refno")%>%left_join(mi, by="hh_refno")
colnames(data)
data$LSC<-as.factor(data$LSC)
names(data)[3]<-'NC'
names(data)[10]<-'HDC'
names(data)[20]<-'CII'
names(data)[21]<-'SMI'
colnames(data)
#NC
nc_plot<-ggplot(data, aes(LSC, y=NC)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Natural capital")
nc_plot+stat_compare_means()
library(ggstatsplot)
nc_kr<-ggbetweenstats(
  data = data,
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
nc_kr
#HDC
hdc_plot<-ggplot(data, aes(LSC, y=HDC)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Human-derived capital")
hdc_plot+stat_compare_means()
library(ggstatsplot)
hdc_kr<-ggbetweenstats(
  data = data,
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
hdc_kr
# #SC
# sc_plot<-ggplot(data, aes(LC, y=sc_si_std)) + 
#   geom_boxplot()+ ylab("Level")+ggtitle("Social capital")
# sc_plot+stat_compare_means()
# #FC
# fc_plot<-ggplot(data, aes(LC, y=fc_si_std)) + 
#   geom_boxplot()+ ylab("Level")+ggtitle("Financial capital")
# fc_plot+stat_compare_means()
# #PC
# pc_plot<-ggplot(data, aes(LC, y=pc_si_std)) + 
#   geom_boxplot()+ ylab("Level")+ggtitle("Physical capital")
# pc_plot+stat_compare_means()
#+stat_compare_means()
#CII
cii_plot<-ggplot(data, aes(LSC, y=CII)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Combined input intensity")
cii_plot+stat_compare_means()
library(ggstatsplot)
library(ggplot2)
cii_kr<-ggbetweenstats(
  data = data,
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
cii_kr
#+stat_compare_means()
#SMI
smi_plot<-ggplot(data, aes(LSC, y=SMI)) + 
  geom_boxplot()+ ylab("Level")+ggtitle("Soil management intensity")
smi_plot+stat_compare_means()
library(ggstatsplot)
smi_kr<-ggbetweenstats(
  data = data,
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
smi_kr
#+stat_compare_means()
library(ggplot2)
kr_graphs<-plot_grid(nc_kr,hdc_kr,cii_kr,smi_kr,         # Draw grid of plots
          ncol = 2,
          nrow = 2,
          align = "v")
kr_graphs
#ggsave("lis_boxplot_reg.pdf", kr_graphs, path = "./8_Analyses/Plots")
ggsave("lis_boxplot_all.jpg", kr_graphs,
       width =22, height=28, units =c("cm"),dpi=400,path = "./8_Analyses/Plots")
#--cii numbers in clusters
#loading intensity data
mi<-read.csv(paste('./8_Analyses/Datasets/HH_imputed_data_final.csv', sep=""))
mi
mi<-mi[c("hh_refno","CIF","CPF")]
mi<-mi%>%left_join(lc_clusters, by='hh_refno')
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
#----Overlap analysis--------
#loading LC clusters data
lc_clusters<-read.csv(paste('./8_Analyses/Spatial_analyses/som_lcdm_data.csv', sep=""))
colnames(lc_clusters)
lc_clusters<-lc_clusters[c("hh_refno","LSC")]
#loading ESB cluster data
es_clusters<-read.csv(paste('./8_Analyses/Datasets/som_all_data.csv', sep=""))
colnames(es_clusters)
es_clusters<-es_clusters[c("hh_refno","superclasses_pam")]
names(es_clusters)[2]<-'ESC'
es_clusters
both_clusters<-lc_clusters%>%left_join(es_clusters, by="hh_refno")
dim(both_clusters)
oldvals <- c(1,2,3)
newvals <- c('LSC1','LSC2','LSC3')
both_clusters$LSC<-newvals[match(both_clusters$LSC, oldvals)]
oldvals <- c(1,2,3,4)
newvals <- c('ESC1','ESC2','ESC3','ESC4')
both_clusters$ESC<-newvals[match(both_clusters$ESC, oldvals)]
both_clusters
both_clusters$combinations<- paste0(both_clusters$LSC, "_", both_clusters$ESC)
library(tidyverse)
n_distinct(data.frame(both_clusters$LSC, both_clusters$ESC))
type<-table(both_clusters[c('combinations')])
type
type_percent <- (type/210)*100   # Create percentage table
type_percent