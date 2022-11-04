
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
library(ggspatial)
library(tmap)

dep <- readOGR("C:/Users/linab/PycharmProjects/geopandas/MGN2018_00_COLOMBIA/MGN_DPTO_POLITICO_MOD.shp")  

reg <- readOGR("C:/Users/linab/PycharmProjects/geopandas/MGN2018_00_COLOMBIA/cOLOMBIA_REGIONS.shp")  

wld <- readOGR("C:/Users/linab/Documents/THESIS_DEMAND/qgis/countries.shp") 

eq<- as.data.frame(read_xlsx("C:/Users/linab/PycharmProjects/Thesis/XM/OR_Department.xlsx", sheet = "All", col_names=c('operator_code','name','department','region','year'),skip=1))

census<- as.data.frame(read_xlsx("C:/Users/linab/Documents/THESIS_DEMAND/Census_data_2018_Final.xlsx", sheet = "Sheet2"))

hourly_demand <- as.data.frame(read_xlsx("C:/Users/linab/PycharmProjects/Thesis/XM/Demanda_por_OR_2018.xlsx",col_names=c('date','operator_code','hr.0','hr.1','hr.2','hr.3','hr.4','hr.5','hr.6','hr.7','hr.8','hr.9','hr.10','hr.11','hr.12','hr.13','hr.14','hr.15','hr.16','hr.17','hr.18','hr.19','hr.20','hr.21','hr.22','hr.23','version'),skip=3))

drop<-c("version")

hourly_final <- hourly_demand[,!(names(hourly_demand) %in% drop)]

hourly_final$yr<-year(hourly_final$date)

hourly_final$mt<-month(hourly_final$date)

hourly_final$dy<-day(hourly_final$date)

long_hourly<-as.data.frame(hourly_final %>% gather(hour, electricity_demand_kwh, hr.0:hr.23)) #wide to long

long_hourly<-as.data.frame(long_hourly %>% group_by(yr,operator_code,hour) %>% summarize(value = mean(electricity_demand_kwh)))

long_hourly$hour<-str_pad(gsub('[^0-9]','',long_hourly$hour), 2, pad = "0")

keeps<-c("hour","operator_code","value")

or_df<-long_hourly[keeps]

or_hour<-left_join(or_df,eq)

drop<-c("name","year")

or_hr<- or_hour[,!(names(or_hour) %in% drop)]

dem_or<-or_hr[complete.cases(or_hr),] #removing NA

unique(dem_or["department"])

length(unique(dem_or$department))

dem_dep<-aggregate(dem_or$value,by=list(department=dem_or$department,dem_or$hour),FUN=sum)

names(dem_dep)<-c("department","hour","demand_kwh")

dem_reg<-aggregate(dem_or$value,by=list(department=dem_or$region,dem_or$hour),FUN=sum)

names(dem_reg)<-c("region","hour","demand_kwh")

# Region analysis

dem_or_final<-as.data.frame(dem_reg%>% spread(hour,demand_kwh)) 

miss_reg<-c("GUAINIA","VICHADA","AMAZONAS","VAUPES","SAN ANDRES")

for(j in 1:5){
  dem_or_final[nrow(dem_or_final)+1,1] <- miss_reg[j]
}

target<-c("THC","SUR","ANTIOQUIA","CENTRO","VAUPES","AMAZONAS","VICHADA","COSTA ATLANTICA","VALLE","SAN ANDRES","GUAINIA","CQR","CHOCO","ORIENTE","GUAVIARE")

dem_or_all<-dem_or_final[match(target,dem_or_final$region),]

row.names(dem_or_all)<-0:14

#GAM
  
names(dem_or_all)<-c("region","hr0","hr1","hr2","hr3","hr4","hr5","hr6","hr7","hr8","hr9","hr10","hr11","hr12","hr13","hr14","hr15","hr16","hr17","hr18","hr19","hr20","hr21","hr22","hr23")
dem_reg<-dem_or_all %>% mutate(dem_tot = dplyr::select(.,hr0:hr23) %>% rowSums(na.rm = TRUE))
dem_sum<-dem_reg[,c("region","dem_tot")]
cen_dem_reg<-cbind(dem_sum,census[,-1])
row.names(cen_dem_reg)<-0:14
st_dem <- SpatialPolygonsDataFrame(reg, cen_dem_reg)

st_dem_sub<-st_dem[-10,]

print(st_dem_sub,n=14) 
nb <- poly2nb(st_dem_sub) #ids<-sapply(slot(st_dem, "polygons"), function(x) slot(x, "ID"))
names(nb) <- attr(nb, "region.id")
nb[1:15]
plot(st_dem_sub ,col = sf.colors(14, categorical = TRUE)) #(st_geometry(st_dem_sub)
plot(nb, coordinates(st_dem_sub), add=TRUE, col="blue")

st_dem_sub@data$dem_tot_pct<-(st_dem_sub@data$dem_tot/sum(st_dem_sub@data$dem_tot,rm=TRUE))*100

sf_dem_sub <- st_as_sf(st_dem_sub)
centroid <- st_centroid(sf_dem_sub)

st_dem_sub$lon <- sf::st_coordinates(centroid)[,1] #x
st_dem_sub$lat <- sf::st_coordinates(centroid)[,2] #y

par(mfrow=c(1,1))
tm_shape(st_dem_sub) + tm_polygons("region", legend.show = F) + tm_layout(panel.show=TRUE, panel.labels= "Centroids",frame=FALSE,panel.label.size=0.8) +tm_scale_bar(position = c("right", "top"),text.size = 0.5) + tm_compass(position = c("left", "top"),size = 2.5, text.size = 0.5) + tm_text("region", size = 1/2)

st_dem_sub@data$ID <- ifelse(st_dem_sub$region=='THC', 0,
                          ifelse(st_dem_sub$region=='SUR', 1,
                                 ifelse(st_dem_sub$region=='ANTIOQUIA', 2,
                                        ifelse(st_dem_sub$region=='CENTRO', 3,
                                               ifelse(st_dem_sub$region=='VAUPES', 4,
                                                      ifelse(st_dem_sub$region=='AMAZONAS', 5,
                                                             ifelse(st_dem_sub$region=='VICHADA', 6,
                                                                    ifelse(st_dem_sub$region=='COSTA ATLANTICA', 7,
                                                                           ifelse(st_dem_sub$region=='VALLE', 8,
                                                                                  ifelse(st_dem_sub$region=='SAN ANDRES', 9,
                                                                                         ifelse(st_dem_sub$region=='GUAINIA', 10,
                                                                                                ifelse(st_dem_sub$region=='CQR', 11,
                                                                                                       ifelse(st_dem_sub$region=='CHOCO', 12,
                                                                                                              ifelse(st_dem_sub$region=='ORIENTE', 13,
                                                                                                                     ifelse(st_dem_sub$region == 'GUAVIARE', 14, 100)
                                                                                                              ))))))))))))))

mydf <- data.frame(id = 1:8, 
                   lat_1 = c(9.5742947,9.5742947, 6.9227964, 6.9227964, 6.9227964, 6.9227964, 6.3424282, 5.9463360), 
                   lon_1 = c(-74.27204, -74.27204, -75.56499, -75.56499, -75.56499, -75.56499, -72.33466, -76.93573), 
                   lat_2 = c(6.9227964, 6.3424282, 5.9463360, 5.1382279, 6.3424282, 3.6625109, 3.6625109, 5.1382279), 
                   lon_2 = c(-75.56499, -72.33466, -76.93573, -75.51146, -72.33466, -73.20818, -73.20818, -75.51146))

wld_def<-st_as_sf(wld)

nc<-st_as_sf(st_dem_sub) #world[!world$name_long %in% c("Colombia"),]
ggplot(data=wld_def) + geom_sf(fill = "darkgray") + geom_sf(data = nc, aes(fill = region)) +
           geom_text(data = nc, aes(x = lon-0.4, y = lat, label = ID)) +
           geom_point(data = nc, aes(x = lon, y = lat),col="green",size=1.5) +
           geom_segment(data = mydf, aes(x = lon_1, y = lat_1, xend = lon_2, yend = lat_2), color = "yellow", size = 0.8, alpha = 0.8) +
           xlab("lon") + annotation_north_arrow(location = "tl", height = unit(1, "cm"),width = unit(0.5, "cm")) + annotation_scale(location = "tr") +
           ggtitle("Neighborhood Structure - Markov Random Fields", subtitle = "Colombia Regions") +
           coord_sf(xlim = c(-79, -67), ylim = c(-4, 15),expand = FALSE) + 
           theme(panel.background = element_rect(fill = "white"))

st_dem_sub@data$ID<-as.factor(names(nb))
gam3 <- gam(dem_tot_pct ~ s(pop,bs="cr",k=3) + s(ID, bs="mrf",xt=list(nb=nb),k=3),data=st_dem_sub,method="REML") 
summary(gam3)

gam3$aic
AIC(gam1,gam3)

cv_wh<-mgcv::concurvity(gam3, full=TRUE) #whole model concurvity
cv_wh # correlation of covariates

cv_pw<-concurvity(gam3, full=FALSE)[["estimate"]] # pairwise concurvity
diag(cv_pw) <- NA
cv_pw[lower.tri(cv_pw)]<-NA

layout(matrix(1:2, ncol=2), widths=c(2,0.3))
par(mar=c(6, 6, 5, 0) + 0.1)
image(z=cv_pw, x=1:ncol(cv_pw), y=1:nrow(cv_pw), ylab="", xlab="",axes=FALSE, asp=1, zlim=c(0,1), col=hcl.colors(5, palette = "BluYl", rev = TRUE, fixup = TRUE))
axis(1, at=1:ncol(cv_pw), labels = colnames(cv_pw), las=2)
axis(2, at=1:nrow(cv_pw), labels = rownames(cv_pw), las=2)

par(mar=c(6, 0, 5, 4))
image(t(matrix(rep(seq(0, 1, len=100), 2), ncol=2)),x=1:3, y=1:101, zlim=c(0,1), axes=FALSE, xlab="", ylab="", col=hcl.colors(5, palette = "BluYl", rev = TRUE, fixup = TRUE))
axis(4, at=seq(1,101,len=5), labels = round(seq(0,1,len=5),1), las=2)

mtext("Pairwise Estimated Concurvity", outer=TRUE,  cex=1.5, line=-2.5)

gam4 <- gam(dem_tot_pct ~ s(pop,bs="cr",k=3) + lat + lon,data=st_dem_sub,method="REML") 
summary(gam4)

cv_wh<-mgcv::concurvity(gam4, full=TRUE) #whole model concurvity
cv_wh # correlation of covariates

gam5 <- gam(dem_tot_pct ~ s(pop,bs="cr",k=3) + lat*lon ,data=st_dem_sub,method="REML") 
summary(gam5)

#test for spatial autocorrelation in the residuals 

#spatial weigths list B binary true or false
nbw<-nb2listw(nb, style="B")
nbw

#https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html

moran(st_dem_sub$dem_tot,nbw,n=length(nbw$neighbours),S0=Szero(nbw))

moran.test(st_dem_sub$dem_tot,nbw, randomisation=FALSE)

moran.mc(st_dem_sub$dem_tot, nbw, nsim=99)



