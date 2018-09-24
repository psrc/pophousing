library(tidyverse)
library(foreign)
library(openxlsx)


# User inputs -------------------------------------------------------------


bnd.year <- "parks_qm_buff_V2050_upd_test"
data.year <- c("2017")
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
geog <- 5

# variables
col.county.name <- "CNTYNAME"
col.secondary.field <- "symbol"

# prefixes for output files
prefix <- switch(geog, NULL, "uga", "anx", "rc", "prk")

parcel.file.nm <- "parcel_localparkqmbuff.rds"
blocks.file.nm <- "parkblks.dbf" # use NULL if geog = 1