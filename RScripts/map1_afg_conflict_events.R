
### map1 of #30DayMapChallenge

# data sources:
# 1. ACLED (Armed Conflict Location Event Data) - acleddata.com
# Data was downloaded on 30/10/2021 and contains all events between 01/01/2001 and 30/10/2021 for Afghanistan
# 2. GADM boundaries data version 3.6 - https://gadm.org/download_country.html
# Data was downloaded on 30/10/2021, in Shapefile format for Afghanistan

# software:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

rm(list=ls()) # to start, remove any existing objects 
setwd('') # set working directory to the directory containing raw data

# load packages
library(sf) # version 1.0-2
library(data.table) # version 1.14.0
library(viridis) # version 0.6.2

# read in data
afg <- read.csv('2001-01-01-2021-10-30-Afghanistan.csv') # conflict events data
admin1 <- st_read('gadm36_AFG_shp/gadm36_AFG_1.shp') # administrative boundaries

# explore ACLED data
names(afg)
table(afg$year) # data spans from 2017 to 2021
nrow(afg) #60,761 conflict event records during this time
table(afg$event_type) # breakdown of data by type of event
sum(afg$fatalities) # 194,452 = total fatalities recorded in the ACLED data from beginning of 2017 to mid-October 2021 

# subset the data for 2021 and convert to data.table
afgsub <- as.data.table(afg[afg$year==2021,c('latitude','longitude','geo_precision','fatalities','year')])

# sum the number of fatalities by point location
sum(afgsub$fatalities) # 41,689 fatalities in 2021 up to mid-October
nrow(afgsub) # 9.077 unique conflict events
fat_by_loc <- afgsub[,sum(fatalities),c('latitude','longitude')]
nrow(fat_by_loc) # 712 unique conflict locations (many have multiple events associated with them)
names(fat_by_loc) 
names(fat_by_loc)[3] <- 'total_fatalities'

# convert data.table to spatial data
fat_by_loc_sf <- sf::st_as_sf(fat_by_loc, coords = c("longitude", "latitude")) 
st_crs(fat_by_loc_sf) <- st_crs(admin1)

# decide on colour gradient that best communicates the variation in number of fatalities
par(mar=c(6,6,2,2))
hist(fat_by_loc_sf$total_fatalities, breaks = 100) # long right tail
fat_by_loc_sf$col_bin <- 1
fat_by_loc_sf$col_bin[fat_by_loc_sf$total_fatalities > 0] <- 2
fat_by_loc_sf$col_bin[fat_by_loc_sf$total_fatalities > 10] <- 3
fat_by_loc_sf$col_bin[fat_by_loc_sf$total_fatalities > 50] <- 4
fat_by_loc_sf$col_bin[fat_by_loc_sf$total_fatalities > 500] <- 5
table(fat_by_loc_sf$col_bin)
fat_by_loc_sf <- fat_by_loc_sf[order(fat_by_loc_sf$col_bin),]

# generate map
par(mar=c(1,1,1,1))
plot(admin1$geometry,col='grey',main='')
col_order <- viridis(5,direction=-1,option='mako')
col_list <- col_order[fat_by_loc_sf$col_bin]
plot(fat_by_loc_sf$geometry,add=T,pch=16,cex=((fat_by_loc_sf$col_bin/2)^0.5),col=col_list)

# add legend
legend(71.5,32.1, legend=c("0","1-10","11-50","51-500",">500"),bg='grey',pch=16,pt.cex=((1:5)/2)^0.5,col=col_order)
text(72.3,33,'Total fatalities at',cex=1.1,col='dimgrey')
text(72.3,32.7,'conflict locations between',cex=1.1,col='dimgrey')
text(72.3,32.4,'1st Jan to 22nd Oct 2021',cex=1.1,col='dimgrey')

# add title
text(63,38.5,'Afghanistan conflict event locations and fatalities',cex=1.6,family='serif',col='dimgrey')

# consider the precision of location points
# ACLED (excellently!) report the geo-precision of each event's location information
# see ACLED's codebook for definitions: https://acleddata.com/acleddatanew/wp-content/uploads/dlm_uploads/2019/01/ACLED_Codebook_2019FINAL.docx.pdf
# briefly, 1 = high level of precision, e.g. town or coordinates reported; 2 = the part of the region reported; 3 = the region reported
table(afg$geo_precision) # breakdown of event counts by geo-precision 
table(afg$geo_precision[afg$year==2021])

