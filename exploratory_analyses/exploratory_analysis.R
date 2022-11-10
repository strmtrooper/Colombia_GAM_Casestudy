library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(splines)
library(gam)
library(lubridate)

# 0. Getting the data for that year
hourly_demand <- as.data.frame(read_xlsx("./colombia_demand_data/Demanda_por_OR_2018.xlsx",col_names=c('date','operator_code','hr.0','hr.1','hr.2','hr.3','hr.4','hr.5','hr.6','hr.7','hr.8','hr.9','hr.10','hr.11','hr.12','hr.13','hr.14','hr.15','hr.16','hr.17','hr.18','hr.19','hr.20','hr.21','hr.22','hr.23','version'),skip=3))

# 1. Dropping the operator information because we dont need it
drop<-c("operator_code","version")

hourly_final = hourly_demand[,!(names(hourly_demand) %in% drop)]

# 2. Aggregating operator information by date
by_date <- hourly_final %>% group_by(date)

# 3. Transforming data from wide to long
sum_hourly_demand<-by_date %>% summarise_at(vars(hr.0:hr.23), sum, na.rm = TRUE)

long_hourly<-as.data.frame(sum_hourly_demand %>% gather(hour, electricity_demand_kwh, hr.0:hr.23))

long_hourly$hour_final<-str_pad(gsub('[^0-9]','',long_hourly$hour), 2, pad = "0")

long_hourly$date_time<-as.POSIXct(paste(long_hourly$date,paste(as.character(long_hourly$hour_final),"00","00",sep=":"),sep=" "),tz="UTC")

keeps<-c("date_time","electricity_demand_kwh")

hourly_demand_df<-long_hourly[keeps]

hourly_demand_df$yr<-year(hourly_demand_df$date_time)

hourly_demand_df$mt<-month(hourly_demand_df$date_time)

hourly_demand_df$dy<-day(hourly_demand_df$date_time)

hourly_demand_df$hr<-hour(hourly_demand_df$date_time)

hourly_demand_df$yday<-yday(hourly_demand_df$date_time)

hourly_demand_df$dy_lb<-wday(hourly_demand_df$date_time, label=TRUE)

hourly_demand_df$dy_nb<-wday(hourly_demand_df$date_time, week_start=1)

hourly_demand_df$wk<-as.factor(ifelse(hourly_demand_df$dy_nb>=6,"wknd","wk"))

#APRIL 2018
par(mfrow = c(2,4)) #(4,8) c(4,4)
for(i in 2:8) {  #1 to 16 then 17-30
  
  df_sub<-hourly_demand_df[hourly_demand_df$yr==2018 & hourly_demand_df$mt==4 & hourly_demand_df$dy==i,]
  
  dem<-data.frame(Time=c(1:dim(df_sub)[1]),demand=df_sub$electricity_demand_kwh/1000)
  
  fc<-unique(date(df_sub$date_time))
  fcl<-paste(as.character(wday(fc, label=TRUE)),":",fc)
  
  plot(dem$Time,dem$demand,xlab="Hours",ylab="Demand MW",main=fcl,ylim=c(5000,9500))
  lines(dem$Time,dem$demand,col='blue','l')
}


check<-as.data.frame(sum_hourly_demand[sum_hourly_demand$date<="2018-04-30" & sum_hourly_demand$date>="2018-04-01",])

check_mw<-check[, 2:ncol(check)]/1000
check_mw

# JULY 2018
par(mfrow = c(2,4)) #(4,8) c(4,4)
for(i in 2:8) {  #1 to 16 then 17-31
  
  df_sub<-hourly_demand_df[hourly_demand_df$yr==2018 & hourly_demand_df$mt==7 & hourly_demand_df$dy==i,]
  
  dem<-data.frame(Time=c(1:dim(df_sub)[1]),demand=df_sub$electricity_demand_kwh/1000)
  
  fc<-unique(date(df_sub$date_time))
  fcl<-paste(as.character(wday(fc, label=TRUE)),":",fc)
  
  plot(dem$Time,dem$demand,xlab="Hours",ylab="Demand MW",main=fcl,ylim=c(5000,9500))
  lines(dem$Time,dem$demand,col='red','l')
}


check<-as.data.frame(sum_hourly_demand[sum_hourly_demand$date<="2018-07-31" & sum_hourly_demand$date>="2018-07-01",])

check_mw<-check[, 2:ncol(check)]/1000
check_mw

# Whole year 2018

hourly_demand_sb <-hourly_demand_df %>% group_by(yday, yr) %>% summarise(demand = sum(electricity_demand_kwh))

par(mfrow = c(1,1),mar=c(4,6,2,2))
 plot(hourly_demand_sb$yday,hourly_demand_sb$demand/1000,xlab="Day Year", ylab="",main="2018")
 lines(hourly_demand_sb$yday,hourly_demand_sb$demand/1000,col='darkmagenta','l')
 axis(2,at=pretty(hourly_demand_sb$demand),labels=format(pretty(hourly_demand_sb$demand),big.mark=",", scientific=FALSE),las=1)
 mtext(text="Demand MW", side=2, line=3)
 
 library(ggplot2)
 ggplot(hourly_demand_df[hourly_demand_df$mt==7 & hourly_demand_df$dy %in% 2:8,],aes(x=hr,y=electricity_demand_kwh/1000))+
 geom_ribbon(aes(ymin = electricity_demand_kwh/1000 - 1000, ymax = electricity_demand_kwh/1000 + 1000), fill = "red",alpha=0.3)+
  geom_point(color="black") + geom_line(color="firebrick3",size=1)+
   labs(title="July 2018 - Dry Season", subtitle="Weekly Patterns",
        y="Demand MW",
        x="Day") + scale_y_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE)) +
   facet_wrap(~dy_lb) 
 
 
 library(ggplot2)
 ggplot(hourly_demand_df[hourly_demand_df$mt==4 & hourly_demand_df$dy %in% 2:8,],aes(x=hr,y=electricity_demand_kwh/1000))+
   geom_ribbon(aes(ymin = electricity_demand_kwh/1000 - 1000, ymax = electricity_demand_kwh/1000 + 1000), fill = "slateblue4",alpha=0.3)+
   geom_point(color="black") + geom_line(color="steelblue4",size=1)+
   labs(title="April 2018 - Rainy Season", subtitle="Weekly Patterns",
        y="Demand MW",
        x="Day") + scale_y_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE)) +
   facet_wrap(~dy_lb) 

 
 
 
 



