library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)
library(FNN)
library(mapview)
library(gifski)
library(gganimate)
library(RSocrata)

setwd("/Users/inordia/Desktop/UPenn搞起来/505/505_final")


## Crime
crime <- read.socrata(
  "https://data.cityofnewyork.us/resource/5uac-w243.json$cmplnt_fr_dt>2021-01-01T00:00:00.000",
  app_token = "MeA3EtY1biAOplejk56gcIUy9",
  email     = "wangran@upenn.edu",
  password  = "73519@Hp"
)

crime <- read.csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv")
crime$CMPLNT_FR_DT <- mdy(crime$CMPLNT_FR_DT)

crime <-crime%>%
  mutate(year = floor_date(CMPLNT_FR_DT, unit = "year"))%>%
  filter(year== "2021-01-01")%>%
  filter(PREM_TYP_DESC == "TRANSIT - NYC SUBWAY")%>%
  mutate(week = week(CMPLNT_FR_DT),
         dotw = wday(CMPLNT_FR_DT, label=TRUE))

crime <- crime[!(is.na(crime$STATION_NAME) | crime$STATION_NAME==""), ]

## COVID

zip <- st_read("Modified Zip Code Tabulation Areas (MODZCTA).geojson")
covid<- read.csv("/Users/inordia/Desktop/UPenn搞起来/505/505_final/covid.csv", check.names=FALSE)


