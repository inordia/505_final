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


## recovery map

mta <- mta %>% 
  filter(recover < 1000)

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
