library(tidycensus)
library(tidyverse)
library(sf)
library(lubridate)
library(kableExtra)
library(patchwork)
library(ggplot2)
library(viridis)
library(gridExtra)
library(knitr)
library(dplyr)
library(osmdata)
library(tigris)
library(stringr)
library(mapview)
library(XML)
library(reshape2)
library(plyr)
library(rgdal)
rm(list=ls())

test = get_acs(geography = "tract", variables = c(), 
year=2019, state=36, county=061, geometry=T, output="wide", survey = "acs5")
dd19_5 <- load_variables(year = 2019,dataset = "acs5")
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

