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
library(RSocrata)
library(leaflet)
library(osmdata)

setwd("/Users/inordia/Desktop/UPenn搞起来/505/505_final")

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

st_c <- st_coordinates

## MTA

mta <- read.csv("mta.csv", check.names = FALSE)

mta<-mta%>%
  gather(key="week",value="recovery", 3:29)%>%
  na.omit()

remote_complex <- read.csv("remoteID_complex.csv")%>%
  select(remote, complex_id, line_name)

mta <- mta%>%
  left_join(remote_complex, by=c("Remote_ID"="remote"))

mta <- mta %>% 
  group_by(complex_id, week) %>% 
  mutate(recover = mean(recovery)) %>% 
  ungroup()%>%
  select(-Remote_ID, -recovery)%>% 
  distinct(complex_id, week, .keep_all = TRUE)

station <- read.csv("station.csv")%>%
  select(Complex.ID, Borough, Structure, GTFS.Latitude, GTFS.Longitude, ADA, ADA.Notes)

mta <- mta %>% 
  left_join(station, by = c("complex_id"="Complex.ID")) %>% 
  distinct(complex_id, week, .keep_all = TRUE)%>%
  st_as_sf(coords = c("GTFS.Longitude", "GTFS.Latitude"))%>%
  st_set_crs(4326) %>%
  st_transform('EPSG:2263')

mta.geo <- mta%>%
  select(complex_id, geometry) %>% 
  distinct()


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

crime <- crime%>%
  st_as_sf(coords = c("Longitude", "Latitude"))%>%
  st_set_crs(4326) %>%
  st_transform('EPSG:2263')

crime.agg <-
  crime%>%
  mutate(number = 1)%>%
  group_by(STATION_NAME, week)%>%
  summarise(Freq = sum(number))

#aggregate(number ~ STATION_NAME + week, FUN = sum, na.rm = TRUE)

crime.geo <- crime.agg%>%
  select(STATION_NAME, geometry)%>%
  distinct()

mta.buffer <- st_buffer(mta.geo, 600)
  
crime.table<- st_join(mta.buffer, crime.geo)%>%
  st_drop_geometry() %>% 
  distinct()%>%
  na.omit()

crime.agg.complex <- crime.agg%>%
  left_join(crime.table, by = "STATION_NAME")%>%
  st_drop_geometry() %>% 
  group_by(complex_id, week)%>%
  summarise(Freq = sum(Freq))%>%
  filter(week > 22)

crime.agg.complex$week <- as.character(crime.agg.complex$week)


mta <- mta%>%
  left_join(crime.agg.complex, by = c("complex_id"= "complex_id", "week"="week"))%>%
  rename(crime = Freq)%>%
  mutate(crime = replace_na(crime, 0))
  
## COVID

zip <- st_read("Modified Zip Code Tabulation Areas (MODZCTA).geojson")
covid<- read.csv("/Users/inordia/Desktop/UPenn搞起来/505/505_final/covid.csv", check.names=FALSE)
covid <- tail(covid, -6)
covid <- covid %>% 
  rename(zip = week_ending)%>%
  gather(key="date",value="covid", 2:86)

covid$date <- mdy(covid$date)

covid <- covid %>% 
  mutate(week = week(date))%>%
  left_join(zip%>%select(modzcta, pop_est, geometry), by = c("zip"="modzcta"))

covid <- covid %>% 
  st_as_sf()%>%
  st_transform('EPSG:2263')

covid.geo <- covid %>% 
  select(zip, week, covid, geometry) %>% 
  st_join(mta.geo)

covid.geo$week <- as.character(covid.geo$week)

covid.geo <- covid.geo %>% 
  st_drop_geometry() %>% 
  select(week, covid, complex_id) %>% 
  distinct()

mta <- mta %>% 
  left_join(covid.geo, by = c("complex_id"= "complex_id", "week"="week"))%>%
  distinct(complex_id, week, .keep_all = TRUE)

## Parks

park <- st_read("Open Space (Parks).geojson") %>% 
  st_transform('EPSG:2263')

mta <- mta %>% 
  mutate(park3_nn=nn_function(st_c(mta),st_c(st_centroid(park)),3),
         park1_nn=nn_function(st_c(mta),st_c(st_centroid(park)),1))

## OSM

q0 <- opq(bbox = c(-74.26013414099346,40.49536834449909,-73.6976408489098,40.919585276931876))

hospital <- add_osm_feature(opq = q0, key = 'amenity', value = "hospital") %>%
  osmdata_sf(.)

hospital.sf <- st_geometry(hospital$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., hospital$osm_points$name) %>%
  rename(NAME = hospital.osm_points.name)%>%
  st_transform('EPSG:2263')%>%
  dplyr::select(geometry)

mta <- mta %>% 
  mutate(hospital_nn=nn_function(st_c(mta),st_c(hospital.sf),1))

mta$housing <-
  st_buffer(mta, 8000) %>%
  aggregate(mutate(hospital.sf, counter = 1),., sum) %>% 
  pull(counter)

mta <- mta %>% 
   mutate(housing = replace_na(housing, 0))

