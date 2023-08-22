#install.packages("terra")
#install.packages("rgdal")
#installed.packages("raster")
#https://frodriguezsanchez.net/post/accessing-data-from-large-online-rasters-with-cloud-optimized-geotiff-gdal-and-terra-r-package/
#https://gitlab.com/openlandmap/africa-soil-and-agronomy-data-cube/-/tree/master
#https://medium.com/nerd-for-tech/soil-and-agronomy-data-cube-for-africa-at-30-m-spatial-resolution-591e04bfc372
#https://zenodo.org/search?page=1&size=20&q=iSDAsoil&sort=mostviewed
#https://gitlab.com/openlandmap/africa-soil-and-agronomy-data-cube
#library(rgdal)
#library(raster)
library(terra)
# read household gps 
#hh1.coords <- data.frame(lon = 37.13379057, lat = 0.03303533)
hh_coords<-read.csv("./data/hh-coords.csv")
hh_coords<-as.data.frame(hh_coords[,-1])
hh_coords
#f<-readOGR("./Laikipia_GISLayers/Farm_selection.kmz")
tif.ts.cog<-paste0("/vsicurl/https://s3.eu-central-1.wasabisys.com/africa-soil/layers30m/", 
                   c("sol_bdr_md_30m_0..200cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_clay_tot_psa_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_db_od_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.al_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.c_tot_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.ca_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.ecec.f_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.fe_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.k_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.mg_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.n_tot_ncs_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.oc_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.p_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif",
                     "sol_log.s_mehlich3_m_30m_0..20cm_2001..2017_africa_epsg4326_v0.1.tif"))

tif.ss.cog = paste0("/vsicurl/https://s3.eu-central-1.wasabisys.com/africa-soil/layers30m/", 
                    c("sol_log.wpg2_md_30m_20..50cm_2001..2017_africa_epsg4326_v0.1.tif",
                   "sol_log.zn_mehlich3_md_30m_20..50cm_2001..2017_africa_epsg4326_v0.1.tif",
                   "sol_ph_h2o_md_30m_20..50cm_2001..2017_africa_epsg4326_v0.1.tif",
                  "sol_sand_tot_psa_md_30m_20..50cm_2001..2017_africa_epsg4326_v0.1.tif",
                   "sol_silt_tot_psa_md_30m_20..50cm_2001..2017_africa_epsg4326_v0.1.tif",
                   "sol_texture.class_m_30m_20..50cm_2001..2017_africa_epsg4326_v0.1.tif"))
tif.ts.cog
tif.ss.cog
library(terra)
ras.ts1 <- rast(tif.ts.cog)
ras.ts2<- rast(tif.ss.cog)
cog.data1 <- terra::extract(ras.ts1, hh_coords)
cog.data2<-terra::extract(ras.ts2, hh_coords)
colnames(cog.data1)
colnames(cog.data2)
library('dplyr')
cog.data<-cog.data1%>%left_join(cog.data2,by = "ID")
colnames(cog.data)
names(cog.data)[1]<-"HH_ID_Number"
names(cog.data)[2]<-"Depth_to_bedrock"
names(cog.data)[3]<-"Clay_content"
names(cog.data)[4]<-"Bulk_density"
names(cog.data)[5]<-"Aluminum_extractable"
names(cog.data)[6]<-"Carbon_total"
names(cog.data)[7]<-"Calcium_extractable"
names(cog.data)[8]<-"E_CEC"
names(cog.data)[9]<-"Iron"
names(cog.data)[10]<-"Potassium"
names(cog.data)[11]<-"Magnesium"
names(cog.data)[12]<-"Total_Nitrogen"
names(cog.data)[13]<-"Organic Carbon"
names(cog.data)[14]<-"Phosphorous"
names(cog.data)[15]<-"Sulfur"
names(cog.data)[16]<-"Stone_content"
names(cog.data)[17]<-"Zinc_extractable"
names(cog.data)[18]<-"pH"
names(cog.data)[19]<-"Sand_content"
names(cog.data)[20]<-"Silt_content"
names(cog.data)[21]<-"Texture_class"
View(cog.data)
write.csv(cog.data,"./Data/HH_Soil_properties.csv",row.names = FALSE)

