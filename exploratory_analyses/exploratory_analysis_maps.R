
library(rgdal)
library(raster)
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(mgcv)
library(spdep)
library(tidyverse)
library(tmap)
library(ggpubr)

dep <- readOGR("./colombia_mapping_files/MGN_DPTO_POLITICO_MOD.shp")
dep_sf<-st_as_sf(dep)
dep_centroid <- st_centroid(dep_sf)
dep_sf$lon <- sf::st_coordinates(dep_centroid)[,1] 
dep_sf$lat <- sf::st_coordinates(dep_centroid)[,2]
dep_sf$title <- "Department Distribution"

reg <- readOGR("./colombia_mapping_files/cOLOMBIA_REGIONS.shp") 
reg_sf<-st_as_sf(reg)
reg_centroid <- st_centroid(reg_sf)
reg_sf$lon <- sf::st_coordinates(reg_centroid)[,1] 
reg_sf$lat <- sf::st_coordinates(reg_centroid)[,2]
reg_sf$title <- "Region Distribution"

wld <- readOGR("./colombia_mapping_files/countries.shp")
wld_def<-st_as_sf(wld)

co_dep<- tm_shape(dep) + tm_polygons("DPTO_CNMBR", legend.show = F) + tm_layout(panel.show=TRUE, panel.labels= "Departments Colombia",frame=FALSE,panel.label.size=0.8) +tm_scale_bar(position = c("right", "top"),text.size = 0.5) + tm_compass(position = c("left", "top"),size = 2.5, text.size = 0.5) + tm_text("DPTO_CNMBR", size = 1/2)
co_reg<-tm_shape(reg) + tm_polygons("REGION", legend.show = F) + tm_layout(panel.show=TRUE, panel.labels= "Regions Colombia",frame=FALSE,panel.label.size=0.8) +tm_scale_bar(position = c("right", "top"),text.size = 0.5) + tm_compass(position = c("left", "top"),size = 2.5, text.size = 0.5) + tm_text("REGION", size = 1/2)

tmap_arrange(co_dep, co_reg, ncol=2)

dep_plt<-ggplot(data=wld_def) + geom_sf(fill = "darkgray") + geom_sf(data = dep_sf, aes(fill = DPTO_CNMBR),show.legend = FALSE) +
  geom_text(data = dep_sf, aes(x = lon, y = lat, label =DPTO_CNMBR ), size=2) +
  xlab("lon") + annotation_north_arrow(location = "tl", height = unit(1, "cm"),width = unit(0.5, "cm"),style = north_arrow_fancy_orienteering) + annotation_scale(location = "tr") +
  coord_sf(xlim = c(-79, -67), ylim = c(-4, 15),expand = FALSE) + 
  theme(panel.background = element_rect(fill = "white")) + facet_grid(. ~ title)


reg_plt<-ggplot(data=wld_def) + geom_sf(fill = "darkgray") + geom_sf(data = reg_sf, aes(fill = REGION),show.legend = FALSE) +
  geom_text(data = reg_sf, aes(x = lon, y = lat, label =REGION ), size=3) +
  xlab("lon") + annotation_north_arrow(location = "tl", height = unit(1, "cm"),width = unit(0.5, "cm"),style = north_arrow_fancy_orienteering) + annotation_scale(location = "tr") +
  coord_sf(xlim = c(-79, -67), ylim = c(-4, 15),expand = FALSE) + 
  theme(panel.background = element_rect(fill = "white")) + facet_grid(. ~ title) #   ggtitle("Regions Distribution") 

ggarrange(dep_plt,reg_plt,ncol=2,nrow=1)