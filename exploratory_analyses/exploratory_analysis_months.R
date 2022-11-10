library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(splines)
library(gam)
library(lubridate)

# 0. Getting the data for that year
hourly_demand <- as.data.frame(read_xlsx("C:/Users/linab/PycharmProjects/Thesis/XM/Demanda_por_OR_2018.xlsx",col_names=c('date','operator_code','hr.0','hr.1','hr.2','hr.3','hr.4','hr.5','hr.6','hr.7','hr.8','hr.9','hr.10','hr.11','hr.12','hr.13','hr.14','hr.15','hr.16','hr.17','hr.18','hr.19','hr.20','hr.21','hr.22','hr.23','version'),skip=3))

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

appended_demand_sb <-as.data.frame(hourly_demand_df %>% group_by(date) %>% summarise(demand = sum(electricity_demand_kwh)))

appended_demand_sb$dy<-day(appended_demand_sb$date)

appended_demand_sb$mt<-as.integer(month(appended_demand_sb$date))

appended_demand_sb$mt_lb<-lubridate::month(appended_demand_sb$date,label=TRUE)

appended_demand_sb$season<-ifelse(appended_demand_sb$mt %in% c(4,5,10,11),"Rainy",
                            ifelse(appended_demand_sb$mt %in% c(12,1,7,8),"Dry","NA"))

par(mfrow = c(3,4)) #(4,8) c(4,4)
for(i in 1:12) {  #1 to 16 then 17-30
  
  df_sub<-appended_demand_sb[appended_demand_sb$mt==i,]
  
  dem<-data.frame(day=df_sub$dy,demand=df_sub$demand/1000)
  
  fc<-unique(lubridate::month(df_sub$date,label=TRUE))
  
  plot(dem$day,dem$demand,xlab="Days",ylab="Demand MW",main=fc,ylim=c(140000,190000))
  lines(dem$day,dem$demand,col='darkmagenta','l')
  mtext("2018", side = 3, line = -2, outer = TRUE, cex=2)
}

library(ggplot2)
ggplot(appended_demand_sb,aes(x=dy,y=demand/1000))+
  geom_rect(data = subset(appended_demand_sb,season == "Dry"),aes(fill = season),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,alpha = 0.3)+
  geom_rect(data = subset(appended_demand_sb,season == "Rainy"),aes(fill = season),xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,alpha = 0.3)+
  geom_point(color="black") + geom_line(color="darkmagenta",size=1)+
  labs(title="2018", subtitle="Monthly Patterns",
       y="Demand MW",
       x="Day") + scale_y_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE)) +
  facet_wrap(~mt_lb) 


 
 



