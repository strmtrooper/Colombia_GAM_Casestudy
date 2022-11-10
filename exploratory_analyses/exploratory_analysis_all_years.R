library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(splines)
library(gam)
library(lubridate)

yr<-2013:2018

yrly_dem<-function(yr){

  # 0. Getting the data for that year
  hourly_demand <- as.data.frame(read_xlsx(paste0("C:/Users/linab/PycharmProjects/Thesis/XM/Demanda_por_OR_",yr,".xlsx"),col_names=c('date','operator_code','hr.0','hr.1','hr.2','hr.3','hr.4','hr.5','hr.6','hr.7','hr.8','hr.9','hr.10','hr.11','hr.12','hr.13','hr.14','hr.15','hr.16','hr.17','hr.18','hr.19','hr.20','hr.21','hr.22','hr.23','version'),skip=3))
  
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
 
  hourly_demand_df$date<-ymd(paste(hourly_demand_df$yr, str_pad(hourly_demand_df$mt, 2, pad = "0"), str_pad(hourly_demand_df$dy, 2, pad = "0"), sep= '-'))
   
  return(hourly_demand_df)}

appended_hourly_demand<-do.call(rbind, lapply(yr,yrly_dem))

appended_demand_sb <-appended_hourly_demand %>% group_by(date) %>% summarise(demand = sum(electricity_demand_kwh))

par(mfrow = c(1,1),mar=c(4,6,2,2))
plot(appended_demand_sb$date,appended_demand_sb$demand/1000,xlab="Day Year", ylab="",main="All years")
lines(appended_demand_sb$date,appended_demand_sb$demand/1000,col='slateblue','l')
axis(2,at=pretty(appended_demand_sb$demand),labels=format(pretty(appended_demand_sb$demand),big.mark=",", scientific=FALSE),las=1)
mtext(text="Demand MW", side=2, line=3)

