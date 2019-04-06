library(tidyverse)
library(foreign)
library(openxlsx)


# User inputs -------------------------------------------------------------


bnd.year <- "hct_buff_V2050_juris"
data.year <- c("2010")
root.dir <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt"
input.dir <- file.path(root.dir, bnd.year, "r_input")
temp.dir <- file.path(root.dir, bnd.year, paste0("est", data.year),"r_temp")
raw.ofm.dir <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/"

# select geog code: 
# 1 = standard regional jurisdiction coverage
# 2 = annexations
# 3 = uga change
# 4 = rural-city annex
# 5 = other geographies that do not have regional coverage (e.g. centers, tod area, park buffers)
# 6 = aggregating by 2 geogs/attributes e.g. HCT modes by jurisdiction 
geog <- 6

# variables
col.county.name <- "CNTYNAME"
col.secondary.field <- "code"

# prefixes for output files
prefix <- switch(geog, NULL, "uga", "anx", "rc", "hct", "hctjur")

parcel.file.nm <- "parcels.rds"
# blocks.file.nm <- "hctbuffblks.dbf" # use NULL if geog = 1 | geog = 6
blocks.file.nm <- NULL
# blocks <- read.dbf(file.path(temp.dir, blocks.file.nm))