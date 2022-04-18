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
library(corrplot)
library(stringr)
library(car)
library(stargazer)

setwd("/Users/inordia/Desktop/UPenn搞起来/505/505_final")

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

st_c <- st_coordinates

palette5 <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")

palette4 <- c("#a1dab4","#41b6c4","#2c7fb8","#253494")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],3),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]],
                                  c(.01,.2,.4,.6,.8), na.rm=T), digits = 3))
  }
}


q5 <- function(variable) {as.factor(ntile(variable, 5))}

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

mta$hospital <-
  st_buffer(mta, 8000) %>%
  aggregate(mutate(hospital.sf, counter = 1),., sum) %>% 
  pull(counter)

mta <- mta %>% 
   mutate(hospital = replace_na(hospital, 0))


office <- add_osm_feature(opq = q0, key = 'office', value = "company") %>%
  osmdata_sf(.)

office.sf <- st_geometry(office$osm_points) %>%
  st_transform(4326) %>%
  st_sf() %>%
  cbind(., office$osm_points$name) %>%
  rename(NAME = office.osm_points.name)%>%
  st_transform('EPSG:2263')%>%
  dplyr::select(geometry)

mta$office <-
  st_buffer(mta, 8000) %>%
  aggregate(mutate(office.sf, counter = 1),., sum) %>% 
  pull(counter)

mta <- mta %>% 
  filter(recover < 1000) %>% 
  mutate(line = str_length(line_name))

## Census


#Demographics
demographics_ny <- get_acs(geography = "tract", variables = c(total_pop = "B01001_001",
                                                              male = "B01001_002",
                                                              female = "B01001_026",
                                                              white = "B02001_002",
                                                              black = "B02001_003",
                                                              american_indian = "B02001_004",
                                                              asian = "B02001_005",
                                                              pi = "B02001_006",
                                                              other = "B02001_007",
                                                              mixed = "B02001_008",
                                                              hispanic = "B03002_012",
                                                              means_trans_total = "B08301_001",
                                                              commute_transit = "B08301_010E",
                                                              commute_bike = "B08301_018E",
                                                              commute_walk = "B08301_019E",
                                                              commute_drivealone = "B08301_003E",
                                                              commute_carpool = "B08301_004E",
                                                              commute_WFH = "B08301_021E",
                                                              median_inc = "B19013_001",
                                                              median_inc_f = "B19113_001",
                                                              commute_total = "B08134_001",
                                                              commute_10 = "B08134_002",
                                                              commute_10_14 = "B08134_003",
                                                              commute_15_19 = "B08134_004",
                                                              commute_20_24 = "B08134_005",
                                                              commute_25_29 = "B08134_006",
                                                              commute_30_34 = "B08134_007",
                                                              commute_35_44 = "B08134_008",
                                                              commute_45_59 = "B08134_009",
                                                              commute_60 = "B08134_010",
                                                              occupation_total = "B08124_001",
                                                              mbas = "B08124_002", #management, business, art, and science
                                                              service = "B08124_003",
                                                              sale_office = "B08124_004",
                                                              ncm = "B08124_005", #natural resources, construction, maintain
                                                              ptmm = "B08124_006", #production, transportation
                                                              military = "B08124_007"
), 
year=2019, state=36, county=061, geometry=T, output="wide", survey = "acs5")


demographics_ny$pct_m = demographics_ny$maleE / demographics_ny$total_popE
demographics_ny$pct_f = demographics_ny$femaleE / demographics_ny$total_popE
demographics_ny$pct_w = demographics_ny$whiteE / demographics_ny$total_popE
demographics_ny$pct_b = demographics_ny$blackE / demographics_ny$total_popE
demographics_ny$pct_ai = demographics_ny$american_indianE / demographics_ny$total_popE
demographics_ny$pct_a = demographics_ny$asianE / demographics_ny$total_popE
demographics_ny$pct_pi = demographics_ny$piE / demographics_ny$total_popE
demographics_ny$pct_other = demographics_ny$otherE / demographics_ny$total_popE
demographics_ny$pct_mix = demographics_ny$mixedE / demographics_ny$total_popE
demographics_ny$pct_his = demographics_ny$hispanicE / demographics_ny$total_popE
demographics_ny$pct_transit = demographics_ny$commute_transit / demographics_ny$means_trans_totalE
demographics_ny$pct_commute_20 = (demographics_ny$commute_10E + demographics_ny$commute_10_14E + demographics_ny$commute_15_19E ) / demographics_ny$commute_totalE
demographics_ny$pct_commute_45 = (demographics_ny$commute_20_24E + demographics_ny$commute_25_29E + demographics_ny$commute_30_34E + demographics_ny$commute_35_44E) / demographics_ny$commute_totalE
demographics_ny$pct_commute_45plus = (demographics_ny$commute_45_59E + demographics_ny$commute_60E) / demographics_ny$commute_totalE
demographics_ny$pct_mbas = demographics_ny$mbasE / demographics_ny$occupation_totalE
demographics_ny$pct_service = demographics_ny$serviceE / demographics_ny$occupation_totalE
demographics_ny$pct_sale_office = demographics_ny$sale_officeE / demographics_ny$occupation_totalE
demographics_ny$pct_ncm = demographics_ny$ncmE / demographics_ny$occupation_totalE
demographics_ny$pct_ptmm = demographics_ny$ptmmE / demographics_ny$occupation_totalE
demographics_ny$pct_military = demographics_ny$militaryE / demographics_ny$occupation_totalE

demographics_ki <- get_acs(geography = "tract", variables = c(total_pop = "B01001_001",
                                                              male = "B01001_002",
                                                              female = "B01001_026",
                                                              white = "B02001_002",
                                                              black = "B02001_003",
                                                              american_indian = "B02001_004",
                                                              asian = "B02001_005",
                                                              pi = "B02001_006",
                                                              other = "B02001_007",
                                                              mixed = "B02001_008",
                                                              hispanic = "B03002_012",
                                                              means_trans_total = "B08301_001",
                                                              commute_transit = "B08301_010E",
                                                              commute_bike = "B08301_018E",
                                                              commute_walk = "B08301_019E",
                                                              commute_drivealone = "B08301_003E",
                                                              commute_carpool = "B08301_004E",
                                                              commute_WFH = "B08301_021E",
                                                              median_inc = "B19013_001",
                                                              median_inc_f = "B19113_001",
                                                              commute_total = "B08134_001",
                                                              commute_10 = "B08134_002",
                                                              commute_10_14 = "B08134_003",
                                                              commute_15_19 = "B08134_004",
                                                              commute_20_24 = "B08134_005",
                                                              commute_25_29 = "B08134_006",
                                                              commute_30_34 = "B08134_007",
                                                              commute_35_44 = "B08134_008",
                                                              commute_45_59 = "B08134_009",
                                                              commute_60 = "B08134_010",
                                                              occupation_total = "B08124_001",
                                                              mbas = "B08124_002", #management, business, art, and science
                                                              service = "B08124_003",
                                                              sale_office = "B08124_004",
                                                              ncm = "B08124_005", #natural resources, construction, maintain
                                                              ptmm = "B08124_006", #production, transportation
                                                              military = "B08124_007"), 
                           year=2019, state=36, county=047, geometry=T, output="wide", survey = "acs5")

demographics_ki$pct_m = demographics_ki$maleE / demographics_ki$total_popE
demographics_ki$pct_f = demographics_ki$femaleE / demographics_ki$total_popE
demographics_ki$pct_w = demographics_ki$whiteE / demographics_ki$total_popE
demographics_ki$pct_b = demographics_ki$blackE / demographics_ki$total_popE
demographics_ki$pct_ai = demographics_ki$american_indianE / demographics_ki$total_popE
demographics_ki$pct_a = demographics_ki$asianE / demographics_ki$total_popE
demographics_ki$pct_pi = demographics_ki$piE / demographics_ki$total_popE
demographics_ki$pct_other = demographics_ki$otherE / demographics_ki$total_popE
demographics_ki$pct_mix = demographics_ki$mixedE / demographics_ki$total_popE
demographics_ki$pct_his = demographics_ki$hispanicE / demographics_ki$total_popE
demographics_ki$pct_transit = demographics_ki$commute_transit / demographics_ki$means_trans_totalE
demographics_ki$pct_commute_20 = (demographics_ki$commute_10E + demographics_ki$commute_10_14E + demographics_ki$commute_15_19E ) / demographics_ki$commute_totalE
demographics_ki$pct_commute_45 = (demographics_ki$commute_20_24E + demographics_ki$commute_25_29E + demographics_ki$commute_30_34E + demographics_ki$commute_35_44E) / demographics_ki$commute_totalE
demographics_ki$pct_commute_45plus = (demographics_ki$commute_45_59E + demographics_ki$commute_60E) / demographics_ki$commute_totalE
demographics_ki$pct_mbas = demographics_ki$mbasE / demographics_ki$occupation_totalE
demographics_ki$pct_service = demographics_ki$serviceE / demographics_ki$occupation_totalE
demographics_ki$pct_sale_office = demographics_ki$sale_officeE / demographics_ki$occupation_totalE
demographics_ki$pct_ncm = demographics_ki$ncmE / demographics_ki$occupation_totalE
demographics_ki$pct_ptmm = demographics_ki$ptmmE / demographics_ki$occupation_totalE
demographics_ki$pct_military = demographics_ki$militaryE / demographics_ki$occupation_totalE


demographics_qu <- get_acs(geography = "tract", variables = c(total_pop = "B01001_001",
                                                              male = "B01001_002",
                                                              female = "B01001_026",
                                                              white = "B02001_002",
                                                              black = "B02001_003",
                                                              american_indian = "B02001_004",
                                                              asian = "B02001_005",
                                                              pi = "B02001_006",
                                                              other = "B02001_007",
                                                              mixed = "B02001_008",
                                                              hispanic = "B03002_012",
                                                              means_trans_total = "B08301_001",
                                                              commute_transit = "B08301_010E",
                                                              commute_bike = "B08301_018E",
                                                              commute_walk = "B08301_019E",
                                                              commute_drivealone = "B08301_003E",
                                                              commute_carpool = "B08301_004E",
                                                              commute_WFH = "B08301_021E",
                                                              median_inc = "B19013_001",
                                                              median_inc_f = "B19113_001",
                                                              commute_total = "B08134_001",
                                                              commute_10 = "B08134_002",
                                                              commute_10_14 = "B08134_003",
                                                              commute_15_19 = "B08134_004",
                                                              commute_20_24 = "B08134_005",
                                                              commute_25_29 = "B08134_006",
                                                              commute_30_34 = "B08134_007",
                                                              commute_35_44 = "B08134_008",
                                                              commute_45_59 = "B08134_009",
                                                              commute_60 = "B08134_010",
                                                              occupation_total = "B08124_001",
                                                              mbas = "B08124_002", #management, business, art, and science
                                                              service = "B08124_003",
                                                              sale_office = "B08124_004",
                                                              ncm = "B08124_005", #natural resources, construction, maintain
                                                              ptmm = "B08124_006", #production, transportation
                                                              military = "B08124_007"), 
                           year=2019, state=36, county=081, geometry=T, output="wide", survey = "acs5")

demographics_qu$pct_m = demographics_qu$maleE / demographics_qu$total_popE
demographics_qu$pct_f = demographics_qu$femaleE / demographics_qu$total_popE
demographics_qu$pct_w = demographics_qu$whiteE / demographics_qu$total_popE
demographics_qu$pct_b = demographics_qu$blackE / demographics_qu$total_popE
demographics_qu$pct_ai = demographics_qu$american_indianE / demographics_qu$total_popE
demographics_qu$pct_a = demographics_qu$asianE / demographics_qu$total_popE
demographics_qu$pct_pi = demographics_qu$piE / demographics_qu$total_popE
demographics_qu$pct_other = demographics_qu$otherE / demographics_qu$total_popE
demographics_qu$pct_mix = demographics_qu$mixedE / demographics_qu$total_popE
demographics_qu$pct_his = demographics_qu$hispanicE / demographics_qu$total_popE
demographics_qu$pct_transit = demographics_qu$commute_transit / demographics_qu$means_trans_totalE
demographics_qu$pct_commute_20 = (demographics_qu$commute_10E + demographics_qu$commute_10_14E + demographics_qu$commute_15_19E ) / demographics_qu$commute_totalE
demographics_qu$pct_commute_45 = (demographics_qu$commute_20_24E + demographics_qu$commute_25_29E + demographics_qu$commute_30_34E + demographics_qu$commute_35_44E) / demographics_qu$commute_totalE
demographics_qu$pct_commute_45plus = (demographics_qu$commute_45_59E + demographics_qu$commute_60E) / demographics_qu$commute_totalE
demographics_qu$pct_mbas = demographics_qu$mbasE / demographics_qu$occupation_totalE
demographics_qu$pct_service = demographics_qu$serviceE / demographics_qu$occupation_totalE
demographics_qu$pct_sale_office = demographics_qu$sale_officeE / demographics_qu$occupation_totalE
demographics_qu$pct_ncm = demographics_qu$ncmE / demographics_qu$occupation_totalE
demographics_qu$pct_ptmm = demographics_qu$ptmmE / demographics_qu$occupation_totalE
demographics_qu$pct_military = demographics_qu$militaryE / demographics_qu$occupation_totalE


demographics_br <- get_acs(geography = "tract", variables = c(total_pop = "B01001_001",
                                                              male = "B01001_002",
                                                              female = "B01001_026",
                                                              white = "B02001_002",
                                                              black = "B02001_003",
                                                              american_indian = "B02001_004",
                                                              asian = "B02001_005",
                                                              pi = "B02001_006",
                                                              other = "B02001_007",
                                                              mixed = "B02001_008",
                                                              hispanic = "B03002_012",
                                                              means_trans_total = "B08301_001",
                                                              commute_transit = "B08301_010E",
                                                              commute_bike = "B08301_018E",
                                                              commute_walk = "B08301_019E",
                                                              commute_drivealone = "B08301_003E",
                                                              commute_carpool = "B08301_004E",
                                                              commute_WFH = "B08301_021E",
                                                              median_inc = "B19013_001",
                                                              median_inc_f = "B19113_001",
                                                              commute_total = "B08134_001",
                                                              commute_10 = "B08134_002",
                                                              commute_10_14 = "B08134_003",
                                                              commute_15_19 = "B08134_004",
                                                              commute_20_24 = "B08134_005",
                                                              commute_25_29 = "B08134_006",
                                                              commute_30_34 = "B08134_007",
                                                              commute_35_44 = "B08134_008",
                                                              commute_45_59 = "B08134_009",
                                                              commute_60 = "B08134_010",
                                                              occupation_total = "B08124_001",
                                                              mbas = "B08124_002", #management, business, art, and science
                                                              service = "B08124_003",
                                                              sale_office = "B08124_004",
                                                              ncm = "B08124_005", #natural resources, construction, maintain
                                                              ptmm = "B08124_006", #production, transportation
                                                              military = "B08124_007"), 
                           year=2019, state=36, county=005, geometry=T, output="wide", survey = "acs5")

demographics_br$pct_m = demographics_br$maleE / demographics_br$total_popE
demographics_br$pct_f = demographics_br$femaleE / demographics_br$total_popE
demographics_br$pct_w = demographics_br$whiteE / demographics_br$total_popE
demographics_br$pct_b = demographics_br$blackE / demographics_br$total_popE
demographics_br$pct_ai = demographics_br$american_indianE / demographics_br$total_popE
demographics_br$pct_a = demographics_br$asianE / demographics_br$total_popE
demographics_br$pct_pi = demographics_br$piE / demographics_br$total_popE
demographics_br$pct_other = demographics_br$otherE / demographics_br$total_popE
demographics_br$pct_mix = demographics_br$mixedE / demographics_br$total_popE
demographics_br$pct_his = demographics_br$hispanicE / demographics_br$total_popE
demographics_br$pct_transit = demographics_br$commute_transit / demographics_br$means_trans_totalE
demographics_br$pct_commute_20 = (demographics_br$commute_10E + demographics_br$commute_10_14E + demographics_br$commute_15_19E ) / demographics_br$commute_totalE
demographics_br$pct_commute_45 = (demographics_br$commute_20_24E + demographics_br$commute_25_29E + demographics_br$commute_30_34E + demographics_br$commute_35_44E) / demographics_br$commute_totalE
demographics_br$pct_commute_45plus = (demographics_br$commute_45_59E + demographics_br$commute_60E) / demographics_br$commute_totalE
demographics_br$pct_mbas = demographics_br$mbasE / demographics_br$occupation_totalE
demographics_br$pct_service = demographics_br$serviceE / demographics_br$occupation_totalE
demographics_br$pct_sale_office = demographics_br$sale_officeE / demographics_br$occupation_totalE
demographics_br$pct_ncm = demographics_br$ncmE / demographics_br$occupation_totalE
demographics_br$pct_ptmm = demographics_br$ptmmE / demographics_br$occupation_totalE
demographics_br$pct_military = demographics_br$militaryE / demographics_br$occupation_totalE


demographics_ri <- get_acs(geography = "tract", variables = c(total_pop = "B01001_001",
                                                              male = "B01001_002",
                                                              female = "B01001_026",
                                                              white = "B02001_002",
                                                              black = "B02001_003",
                                                              american_indian = "B02001_004",
                                                              asian = "B02001_005",
                                                              pi = "B02001_006",
                                                              other = "B02001_007",
                                                              mixed = "B02001_008",
                                                              hispanic = "B03002_012",
                                                              means_trans_total = "B08301_001",
                                                              commute_transit = "B08301_010E",
                                                              commute_bike = "B08301_018E",
                                                              commute_walk = "B08301_019E",
                                                              commute_drivealone = "B08301_003E",
                                                              commute_carpool = "B08301_004E",
                                                              commute_WFH = "B08301_021E",
                                                              median_inc = "B19013_001",
                                                              median_inc_f = "B19113_001",
                                                              commute_total = "B08134_001",
                                                              commute_10 = "B08134_002",
                                                              commute_10_14 = "B08134_003",
                                                              commute_15_19 = "B08134_004",
                                                              commute_20_24 = "B08134_005",
                                                              commute_25_29 = "B08134_006",
                                                              commute_30_34 = "B08134_007",
                                                              commute_35_44 = "B08134_008",
                                                              commute_45_59 = "B08134_009",
                                                              commute_60 = "B08134_010",
                                                              occupation_total = "B08124_001",
                                                              mbas = "B08124_002", #management, business, art, and science
                                                              service = "B08124_003",
                                                              sale_office = "B08124_004",
                                                              ncm = "B08124_005", #natural resources, construction, maintain
                                                              ptmm = "B08124_006", #production, transportation
                                                              military = "B08124_007"), 
                           year=2019, state=36, county=085, geometry=T, output="wide", survey = "acs5")

demographics_ri$pct_m = demographics_ri$maleE / demographics_ri$total_popE
demographics_ri$pct_f = demographics_ri$femaleE / demographics_ri$total_popE
demographics_ri$pct_w = demographics_ri$whiteE / demographics_ri$total_popE
demographics_ri$pct_b = demographics_ri$blackE / demographics_ri$total_popE
demographics_ri$pct_ai = demographics_ri$american_indianE / demographics_ri$total_popE
demographics_ri$pct_a = demographics_ri$asianE / demographics_ri$total_popE
demographics_ri$pct_pi = demographics_ri$piE / demographics_ri$total_popE
demographics_ri$pct_other = demographics_ri$otherE / demographics_ri$total_popE
demographics_ri$pct_mix = demographics_ri$mixedE / demographics_ri$total_popE
demographics_ri$pct_his = demographics_ri$hispanicE / demographics_ri$total_popE
demographics_ri$pct_transit = demographics_ri$commute_transit / demographics_ri$means_trans_totalE
demographics_ri$pct_commute_20 = (demographics_ri$commute_10E + demographics_ri$commute_10_14E + demographics_ri$commute_15_19E ) / demographics_ri$commute_totalE
demographics_ri$pct_commute_45 = (demographics_ri$commute_20_24E + demographics_ri$commute_25_29E + demographics_ri$commute_30_34E + demographics_ri$commute_35_44E) / demographics_ri$commute_totalE
demographics_ri$pct_commute_45plus = (demographics_ri$commute_45_59E + demographics_ri$commute_60E) / demographics_ri$commute_totalE
demographics_ri$pct_mbas = demographics_ri$mbasE / demographics_ri$occupation_totalE
demographics_ri$pct_service = demographics_ri$serviceE / demographics_ri$occupation_totalE
demographics_ri$pct_sale_office = demographics_ri$sale_officeE / demographics_ri$occupation_totalE
demographics_ri$pct_ncm = demographics_ri$ncmE / demographics_ri$occupation_totalE
demographics_ri$pct_ptmm = demographics_ri$ptmmE / demographics_ri$occupation_totalE
demographics_ri$pct_military = demographics_ri$militaryE / demographics_ri$occupation_totalE

ny = bind_rows(demographics_ny, demographics_ki, demographics_qu, demographics_br, demographics_ri)

ny.use <- ny %>% 
  select(-ends_with("M")) %>% 
  select(total_popE, pct_transit, median_incE, median_inc_fE,
         pct_mbas, pct_service, pct_sale_office, pct_military, ptmmE, ncmE,
         pct_w, occupation_totalE, geometry) %>% 
  mutate(pct_ptmm = ptmmE / occupation_totalE,
         pct_ncm = ncmE / occupation_totalE,
         pct_work = occupation_totalE/ total_popE) %>% 
  select(-ptmmE, -ncmE, -occupation_totalE) %>% 
  st_transform('EPSG:2263')


mta <- mta %>% 
  st_join(ny.use)

## recovery map


plot(density(mta$recover, na.rm=TRUE))
plot(log(mta$recover), mta$covid)
plot(density(log(mta$recover)))

mta <- mta %>% 
  group_by(complex_id) %>% 
  mutate(ave_recover = mean(recover)) %>% 
  ungroup()

summary(mta$ave_recover)

ggplot()+
  geom_sf(data = zip,
          fill = "antiquewhite1", color = "grey75")+
  geom_sf(data = mta %>% filter(week < 29), 
          shape = 21,
          aes(
            size = ave_recover,
            fill = ave_recover
          ),alpha = 1, color = "transparent", show.legend = "point") +
  scale_size_continuous(
    range = c(1,6),
    breaks = c(0.2423, 0.3882, 0.4444, 0.4852, 0.9608),
    labels = qBr(mta, "ave_recover"),
    name = "Recovery Ratio 2021/2019")+
  scale_fill_stepsn(
    colors = palette5,
    breaks = c(0.2423, 0.3882, 0.4444, 0.4852, 0.9608),
    guide = FALSE)+
  labs(title = "Graduated Symbol Map of Average Recovery Rate", subtitle = "MTA subway stations 2019/2021")+
  mapTheme()+
  guides(size = guide_legend(override.aes = list(fill = palette5)))

mta.test <- mta %>% filter(week < 29)
summary(mta.test$recover)

ggplot()+
  geom_sf(data = zip,
          fill = "antiquewhite1", color = "grey75")+
  geom_sf(data = mta %>% filter(week < 29), 
          shape = 21,
          aes(
            size = recover,
            fill = recover
          ),alpha = 1, color = "transparent", show.legend = "point") +
  scale_size_continuous(
    range = c(1,6),
    breaks = c(0.01823, 0.33193, 0.40567, 0.46728, 7.357653),
    labels = qBr(mta, "recover"),
    name = "Recovery Ratio 2021/2019")+
  scale_fill_stepsn(
    colors = palette5,
    breaks = c(0.01823, 0.33193, 0.40567, 0.46728, 7.357653),
    guide = FALSE)+
  labs(title = "Graduated Symbol Map of Recovery Rate", subtitle = "MTA subway stations 2019/2021")+
  facet_wrap(~week)+
  mapTheme() +
  guides(size = guide_legend(override.aes = list(fill = palette4)))

 mta %>% 
  group_by(week)%>%
  summarise(rec = mean(recover)) %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x=week, y=rec, group=1))+
  geom_line()+
  ylim(0.3, 0.5)+
   labs(title="System-wide Average Ridership Recovery Rate",
        subtitle = "MTA subway stations 2019/2021",
        x="Week", y="Recovery Rate")+
  plotTheme()
  
 
 ## Lags
 
 mta <- 
   mta %>% 
   arrange(week, complex_id) %>% 
   mutate(lagrecover= dplyr::lag(recover,1),
          lag2recover= dplyr::lag(recover,2),
          lag3recover = dplyr::lag(recover,3),
          lag4recover= dplyr::lag(recover,4),
          lagcrime = dplyr::lag(crime,1),
          lag2crime = dplyr::lag(crime,2),
          lagcovid = dplyr::lag(covid,1),
          lag2covid = dplyr::lag(covid,2),
          lag3covid = dplyr::lag(covid,3),
          lag4covid = dplyr::lag(covid,4))

 plotData.lag <-
   as.data.frame(mta)%>%
   dplyr::select(starts_with("lag"), recover) %>%
   gather(Variable, Value, -recover) %>%
   mutate(Variable = fct_relevel(Variable, "lagrecover", "lag2recover",
                                 "lag3recover", "lag4recover", "lagcrime", "lag2crime",
                                 "lagcovid", "lag2covid", "lag3covid", "lag4covid"))
 correlation.lag <-
   group_by(plotData.lag, Variable) %>%
   summarize(correlation = round(cor(Value, recover, use = "complete.obs"), 2)) %>%
   kable(caption = "MTA Stations Recovery Ratio") %>%
   kable_styling("striped", full_width = F)
 
 correlation.lag

 
 ## Correlation
 
 mta.cor <- mta %>% 
   st_drop_geometry() %>% 
   select(-Station, -week, -complex_id, -line_name, -Borough, -Structure, -ADA, -ADA.Notes, 
          -ave_recover) %>% 
   na.omit() %>% 
   cor()

 corrplot(mta.cor, type="upper", order="hclust",
          tl.col="black", tl.srt=45, diag=FALSE)
 

  
 ## feature engineering
 
 plot(density(mta$crime, na.rm=TRUE))
 plot(log(mta$crime), mta$covid)
 plot(density(log(mta$crime)))
 
 mta <- mta %>% 
   mutate(crimelog = log(crime+1))

 plot(density(log(mta$covid %>% na.omit())))
 
 mta <- mta %>% 
   mutate(logcovid = log(covid+1))
 
 mta[is.na(mta)] <- 0

   
 
 ## modeling
 
 step(lm(recover ~ Borough+Structure+ADA+covid+park3+park1+
           hospital_nn+hospital+office+crimelog+lagrecover+
           lag2recover+lag3recover+lag4recover+lagcrime+
           lag2crime+lagcovid+lag2covid+lag3covid+lag4covid, data=mta),
      direction="backward")
 
 reg.base <- lm(recover ~ Borough+Structure+ADA+covid+park3+park1+
             hospital_nn+hospital+office+crimelog+lagrecover+
             lag2recover+lag3recover+lag4recover+lagcrime+
             lag2crime+lagcovid+lag2covid+lag3covid+lag4covid+line+
               total_popE+pct_transit+median_incE+median_inc_fE+
               pct_mbas+pct_service+pct_sale_office+pct_military+
               pct_w+pct_ptmm+pct_ncm+pct_work+logcovid, data=mta)
 summary(reg.base)
 
 reg1 <- lm(recover ~ Borough+Structure+logcovid+
             hospital_nn+office+lagrecover+hospital+
             lag2recover+lag3recover+lag4recover+lag4covid+
             line, data=mta %>% na.omit())
 
 summary(reg1)
 
 reg2 <- lm(recover ~ Borough+Structure+covid+
                  hospital_nn+office+hospital+
             lagrecover+lag2recover+lag3recover+lag4recover+lag4covid+
             line+park3+pct_transit+median_inc_fE+pct_w+pct_work+
             pct_mbas+pct_ptmm, data=mta %>% na.omit())
 
 summary(reg2)
 
 anova(reg1)
 anova(reg2)
 anova(reg1, reg2)
 
 step(lm(recover ~ Borough+Structure+covid+
           hospital_nn+office+hospital+
           lagrecover+lag2recover+lag3recover+lag4recover+lag4covid+
           line+park3+pct_transit+median_inc_fE+pct_w+pct_work+
           pct_mbas+pct_ptmm, data=mta %>% na.omit()),
      direction="backward")

 vif(reg2)

 reg3 <- lm(recover ~ Borough+Structure+logcovid+
              hospital_nn+office+hospital+
              lagrecover+lag2recover+lag3recover+lag4recover+lag4covid+
              line+park3+pct_transit+median_inc_fE+pct_w+pct_work+
              pct_ptmm, data=mta %>% na.omit())
 
 summary(reg3)
 anova(reg3)
 anova(reg2, reg3) 
 
 vif(reg3)
 
 step(lm(recover ~ Borough+Structure+logcovid+
           hospital_nn+office+hospital+
           lagrecover+lag2recover+lag3recover+lag4recover+lag4covid+
           line+park3+pct_transit+median_inc_fE+pct_w+pct_work+
           pct_ptmm, data=mta %>% na.omit()),
      direction="backward")
 
 reg4 <- lm(recover ~ Borough+Structure+logcovid+
              hospital_nn+office+hospital+
              lagrecover+lag2recover+lag3recover+lag4recover+lag4covid+
              line+park3+pct_transit+median_inc_fE+pct_work+
              pct_ptmm, data=mta %>% na.omit())
 summary(reg4)
 
 anova(reg4)
 anova(reg2, reg4)
 
 stargazer(reg1, reg2, reg3, reg4, type ="html", font.size = "small", single.row = TRUE)
 
 plot(reg4)
 plot(density(resid(reg4)))
 
 
 
 