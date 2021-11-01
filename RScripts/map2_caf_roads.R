
### map 2 of #30DayMapChallenge

# data source:
# Humanitarian OpenStreetMap Team 
# downloaded from the Humanitarian Data Exchange (HDX): https://data.humdata.org/dataset/hotosm_caf_roads
# on 01/11/2021

# software:
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

rm(list=ls()) # to start, remove any existing objects 
setwd('') # set working directory to the directory containing raw data

# load packages
library(sf) # version 1.0-2

# read in data
roads <- st_read('hotosm_caf_roads_lines_shp/hotosm_caf_roads_lines.shp') # conflict events data

# explore roads data
roads
table(roads$highway)
table(roads$surface)

# plot roads data
plot(roads$geometry,bg='black',col='orange',lwd=0.5)
text(24,2.8,'Mapped Roads in Central African Republic',col='lightgrey',cex=1.3)
