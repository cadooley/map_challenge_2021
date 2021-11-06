

### map 3 of #30DayMapChallenge

# data source:
# 1. The IPC Population Tracking Tool - http://www.ipcinfo.org/ipc-country-analysis/population-tracking-tool/en/
# Downloaded data for Democratic Republic of Congo on 06/11/2021
# 2. UNOCHA Democratic Republic of Congo level 2 administrative unit boundaries
# Downloaded from HDX: https://data.humdata.org/dataset/drc-administrative-boundaries-levels-0-2
# on 06/11/2021

# software:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

rm(list=ls()) # to start, remove any existing objects 
setwd('') # set working directory to the directory containing raw data

# load packages
library(sf) # version 1.0-2
library(RColorBrewer) # version 1.1-2

# read in data
admins <- st_read('cod_admbnda_rgc_itos_20190911_shp/cod_admbnda_adm2_rgc_20190911.shp') # boundaries 
ipc <- read.csv('ipc_drc.csv') # IPC classification

# subset IPC data to include Feb-2021 round only
ipc[1,] # because this spreadsheet has col headings and subheadings, need to look for col name associated with 'Date of Analysis'
unique(ipc$X.4) # look at what rounds are included in the spreadsheet
ipc_2021 <- ipc[ipc$X.4=="Feb-21",]

# subset IPC data to include cols: admin1, admin2, area phase
ipc_2021 <- ipc_2021[,c(2,3,9)]
head(ipc_2021)
ipc_2021 <- ipc_2021[!ipc_2021[,2]=='',] # exclude empty rows
names(ipc_2021) <- c('admin1','admin2','area_phase')

table(ipc_2021$admin1)
table(admins$ADM1_FR)
# the IPC data contains many units for Kinshasa compared to just 1 unit in the boundaries data 
# identify the maximum phase classification across all Kinshasa units
max(as.numeric(ipc_2021$area_phase[ipc_2021$admin1=='Kinshasa']))

# exclude all the Kinshasa units, then create one single entry for Kinshasa
ipc_2021 <- ipc_2021[-which(ipc_2021$admin1=='Kinshasa'),]
ipc_2021[(nrow(ipc_2021)+1),] <- c('Kinshasa','Kinshasa','3')

# look at admin2 unit names in IPC data that are not matched with a name in the boundaries data
sort(ipc_2021$admin2[(!ipc_2021$admin2%in%admins$ADM2_FR)&(!ipc_2021$admin1=='Kinshasa')])
admins$ADM2_FR # most can be matched up by eye
ipc_2021$admin2[ipc_2021$admin2 == 'Kabeya-kamwanga'] <- 'Kabeya-Kamwanga'
ipc_2021$admin2[ipc_2021$admin2 == 'Kasongo-lunda'] <- 'Kasongo-Lunda'
ipc_2021$admin2[ipc_2021$admin2 == 'Katako-kombe'] <- 'Katako-Kombe'
ipc_2021$admin2[ipc_2021$admin2 == 'Malemba-nkulu'] <- 'Malemba-Nkulu'
ipc_2021$admin2[ipc_2021$admin2 == 'Masi-manimba'] <- 'Masi-Manimba'
ipc_2021$admin2[ipc_2021$admin2 == 'Mbanza-ngungu'] <- 'Mbanza-Ngungu'
ipc_2021$admin2[ipc_2021$admin2 == 'Mbuji-mayi'] <- 'Mbuji-Mayi'
ipc_2021$admin2[ipc_2021$admin2 == 'Seke-banza'] <- 'Seke-Banza'
ipc_2021$admin2[ipc_2021$admin2 == 'Territoire de LODJA'] <- 'Lodja'
ipc_2021$admin2[ipc_2021$admin2 == 'Ville de Gbadolite'] <- 'Gbadolite'
ipc_2021$admin2[ipc_2021$admin2 == 'Beni-ville'] <- 'Beni'

# check whether any of these updated names already existed in the IPC data
table(ipc_2021$admin2)[table(ipc_2021$admin2)>1]
# Beni
ipc[ipc[,3]%in%c('Beni','Beni-ville'),]
# Beni & Beni-ville are both in Nord Kivu, perhaps one represents a town and the other the wider area

# exclude the entry with the lower IPC phase because we only have one admin unit for these two entries
ipc_2021[ipc_2021$admin2=='Beni',]
ipc_2021 <- ipc_2021[-which(ipc_2021$admin2=='Beni'&ipc_2021$area_phase==3),]

# merge datasets
admins <- merge(admins,ipc_2021,by.x='ADM2_FR',by.y='admin2',all=TRUE)
nrow(admins)

# assign colours to units based on IPC
colours <- brewer.pal(6, 'Reds')
admins$colour <- '#FFFFFF'
unique(admins$area_phase)
admins$colour[admins$area_phase=='2'] <- colours[3]
admins$colour[admins$area_phase=='3'] <- colours[4]
admins$colour[admins$area_phase=='4'] <- colours[5]

# plot
plot(admins[,'area_phase'],col=admins$colour,main='',bg='lightgrey')

# add legend
legend(0,0.9,legend=c("no data","minimal","stressed","crisis","emergency","famine"),text.font=6,cex=1,border='black',fill=c('#FFFFFF',colours[2:6]),bg='lightgrey',bty='n') #pch=16,
text(0.05,0.93,'Phase',cex=1.2,col='black',family='serif')

# add title
text(0.2,0.1,'Democratic Republic of Congo',cex=1.3,col='black',family='serif')
text(0.2,0.05,'Integrated Food Security Phase Classification (IPC)',cex=1,col='black',family='serif')
text(0.2,0,'February 2021',cex=1,col='black',family='serif')



