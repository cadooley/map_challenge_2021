

### map 4 of #30DayMapChallenge

# data source:
# 1. EM-DAT public - https://public.emdat.be/data
# Downloaded data for Natural Disasters for all countries for 2020 & 2021 on 06/11/2021
# After download, deleted empty top rows and saved as .csv
# 2. Natural Earth 'map unit' boundaries - https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-details/
# Downloaded data on 06/11/2021

# software:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

rm(list=ls()) # to start, remove any existing objects 
setwd('') # set working directory to the directory containing raw data

# load packages
library(sf) # version 1.0-2
library(data.table) # version 1.14.0
library(RColorBrewer) # version 1.1-2

# read in data
admins <- st_read('ne_10m_admin_0_map_units/ne_10m_admin_0_map_units.shp') # boundaries 
nde <- read.csv('emdat_public_2021_11_06_query_uid-D4Lfgh.csv') # natural disasters event data

# select cols
names(admins)
admins <- admins[,c("SOVEREIGNT","ADMIN","ADM0_A3_IS","REGION_UN")]
names(nde)
nde <- nde[,c("Year","Disaster.Group","Disaster.Subgroup","Disaster.Type","Disaster.Subtype",
              "Event.Name","Country","ISO","Total.Affected","Total.Damages...000.US..")]

# mismatches in two datasets?
unique(sort(nde$ISO[!nde$ISO%in%admins$ADM0_A3_IS]))
nde[nde$ISO=='SPI',] # SPI = Canary Islands

# number of disasters reports for whole of 2020 & Jan-Nov 2021
table(nde$Year)
nde_2020 <- nde[nde$Year=='2020',]

# disaster types
table(nde_2020$Disaster.Subgroup)
nde_2020 <- nde_2020[!nde_2020$Disaster.Subgroup%in%c('Geophysical','Biological'),]
table(nde_2020$Disaster.Type)
nde_2020 <- data.table(nde_2020)

# replace NAs with 0s so that we can sum values
nde_2020$Total.Affected[is.na(nde_2020$Total.Affected)] <- 0
nde_2020$Total.Damages...000.US..[is.na(nde_2020$Total.Damages...000.US..)] <- 0

# total people affected
nde_2020_pa <- nde_2020[,sum(Total.Affected),by=c('ISO')]
nrow(nde_2020_pa) # 123
names(nde_2020_pa) <- c('ISO','tot_aff_people')

admins_tap <- merge(admins,nde_2020_pa,by.x='ADM0_A3_IS',by.y='ISO',all=TRUE)

col_order <- brewer.pal(6, 'YlGn')
admins_tap$col <- col_order[1]
admins_tap$col[admins_tap$tot_aff_people<=5000] <- col_order[2]
admins_tap$col[admins_tap$tot_aff_people>5000 & admins_tap$tot_aff_people<=50000] <- col_order[3]
admins_tap$col[admins_tap$tot_aff_people>50000 & admins_tap$tot_aff_people<=500000] <- col_order[4]
admins_tap$col[admins_tap$tot_aff_people>500000 & admins_tap$tot_aff_people<=5000000] <- col_order[5]
admins_tap$col[admins_tap$tot_aff_people>5000000] <- col_order[6]

plot(admins_tap[,'tot_aff_people'],col=admins_tap$col,bg='lightblue',main='')

legend(0.02,0.5,legend=c("0","1-5,000","5,001-50,000","50,001-500,000","500,001-5,000,000",">5,000,000"),
       bg='lightblue',box.col='lightblue',cex=1,border='black',fill=col_order,bty='o',
       title='Total people affected') 

text(0.5,0.9,'Total number of people affected by climate related natural disasters in 2020',cex=1.5)

# total damages
nde_2020_dam <- nde_2020[,sum(Total.Damages...000.US..),by=c('ISO')]
nrow(nde_2020_dam) # 123
names(nde_2020_dam) <- c('ISO','tot_dam_US')

admins_tdam <- merge(admins,nde_2020_dam,by.x='ADM0_A3_IS',by.y='ISO',all=TRUE)

col_order <- brewer.pal(6, 'PuBu')
admins_tdam$col <- col_order[1]
admins_tdam$col[admins_tdam$tot_dam_US<=10000] <- col_order[2]
admins_tdam$col[admins_tdam$tot_dam_US>10000 & admins_tdam$tot_dam_US<=100000] <- col_order[3]
admins_tdam$col[admins_tdam$tot_dam_US>100000 & admins_tdam$tot_dam_US<=1000000] <- col_order[4]
admins_tdam$col[admins_tdam$tot_dam_US>1000000 & admins_tdam$tot_dam_US<=10000000] <- col_order[5]
admins_tdam$col[admins_tdam$tot_dam_US>10000000] <- col_order[6]

plot(admins_tdam[,'tot_dam_US'],col=admins_tdam$col,bg='lightblue',main='')

legend(0.02,0.5,legend=c("0","1-10,000","10,001-100,000","100,001-1,000,000","1,000,001-10,000,000",">10,000,000"),
       bg='lightblue',box.col='lightblue',cex=1,border='black',fill=col_order,bty='o',
       title='Total damages (US dollars)') 

text(0.5,0.9,'Total damages in US dollars due to climate related natural disasters in 2020',cex=1.5)

