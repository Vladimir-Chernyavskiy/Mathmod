setwd("/Users/VladimirChernyavskiy/Desktop/rfk")
getwd()
library(tidyverse)
library(rnoaa)
station_data = ghcnd_stations()
write.csv(station_data,"station_data2020.csv")
station_data = read.csv("station_data2020.csv")
smol = data.frame(id="SMOLENSK", latitude= 54.75, longitude=32.0667)
smol_around = meteo_nearby_stations(lat_lon_df = smol, station_data = station_data,
                                    var = c("TAVG"), year_min = 2005, year_max = 2012,
                                    radius = 240)
smol_id = smol_around[["SMOLENSK"]][["id"]][1]
summary(smol_id)
smol_table = smol_around[["SMOLENSK"]][["id"]][1:26]
summary(smol_table)
smol_stations = smol_table
str(smol_stations)
all_smol_data = meteo_tidy_ghcnd(stationid = smol_id)
summary(all_smol_data)

all_i = data.frame()
all_tula_meteodata = data.frame()
for(i in 1:26){
  all_i = meteo_tidy_ghcnd(stationid = smol_around[["SMOLENSK"]][["id"]][i], var="tavg", date_min = "2005-01-01", date_max = "2012-12-31")
  all_tula_meteodata=rbind(all_tula_meteodata, all_i)
}
write.csv(all_tula_meteodata, "all_tula_meteodata.csv")


setwd("/Users/VladimirChernyavskiy/Desktop/rfk")
all_tula_meteodata = read.csv("all_tula_meteodata.csv")
str(all_tula_meteodata)
library("tidyverse")
library("lubridate")
y = year(all_tula_meteodata$date)
y
all_tula_meteodata[,"year"] = year(all_tula_meteodata$date)
all_tula_meteodata[,"month"] = month(all_tula_meteodata$date)
all_tula_meteodata[,"day_of_the_year"] = yday(all_tula_meteodata$date)
str(all_tula_meteodata)
years_tula_meteodata = filter(all_tula_meteodata, all_tula_meteodata$year >= 2005 & all_tula_meteodata$year <= 2007)
str(years_tula_meteodata)
summary(years_tula_meteodata)
years_tula_meteodata[,"tavg"] = years_tula_meteodata$tavg / 10
summary(years_tula_meteodata)
years_tula_meteodata[is.na(years_tula_meteodata$tavg), "tavg"] = 0
years_tula_meteodata[years_tula_meteodata$tavg<5, "tavg"] = 0
summary(years_tula_meteodata)

alldays = group_by(years_tula_meteodata,id,year,month)
sumT_alldays_tula = summarize(alldays,tsum=sum(tavg))
summary(sumT_alldays_tula)
groups_Tula_months = group_by(sumT_alldays_tula,month)
groups_Tula_months
sumT_months = summarize(groups_Tula_months,mean=mean(tsum))
sumT_months
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
Fi = afi + bfi * 1.0 * sumT_months
Fi

Yi = 1000000 * ((Fi*di*300) / (1600*2.2*(100-25)))
Yi
Yield = sum(Yi)
Yield