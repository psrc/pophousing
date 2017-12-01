library(tidyverse)
library(foreign)


# User inputs -------------------------------------------------------------


bnd.year <- "2017"
data.year <- c("2000")
root.dir <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt"
input.dir <- file.path(root.dir, bnd.year, "r_input")
temp.dir <- file.path(root.dir, bnd.year, paste0("est", data.year),"r_temp")
raw.ofm.dir <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/"

# select geog code: standard = 1, annex = 2, uga change = 3, rural-city annex = 4, centers = 5
geog <- 1

# variables
vars <- switch(geog,
               c("COUNTY" = "CNTYNAME", "SecField" = "JURIS_1"),
               c("COUNTY" = "ANXCNTY", "SecField" = "ANXCITY"),
               c("COUNTY" = "UGACNTY", "SecField" = "add_remove"),
               c("COUNTY" = "UGACNTY", "SecField" = "CITYNAME", "SecField2" = "add_remove"))

# prefixes
prefix <- switch(geog, NULL, "uga", "anx", "rc", "ctr")


# Read data ---------------------------------------------------------------


ofm <- readRDS(file.path(raw.ofm.dir,"ofm_saep.rds"))
bldg <- readRDS(file.path(input.dir,"bldg.rds"))

parcel <- switch(geog, 
                 readRDS(file.path(input.dir,"parcel.rds")),
                 readRDS(file.path(input.dir,"parcel_annex.rds")),
                 readRDS(file.path(input.dir,"parcel_uga.rds")),
                 readRDS(file.path(input.dir,"parcel_rc.rds")))

blocks <- switch(geog,
                 NULL, 
                 read.dbf(file.path(temp.dir, "anxblks.dbf")), # 2014-17 Annexations
                 read.dbf(file.path(temp.dir, "uga1417blks.dbf")), # 2014-17 change
                 read.dbf(file.path(temp.dir, "rc1417blks.dbf"))) # 2014-17 Annexations rural-to-city


# Arguments ---------------------------------------------------------------


orig.vars <- as.vector(unname(vars))
new.vars <- as.vector(names(vars))

# b fields
b.vars <- paste0("b", new.vars)
  
# HHP no parcels
hhp.upd.vars <- c("COUNTY" = "ifelse((is.na(COUNTY) & !is.na(bSecField)), bCOUNTY, COUNTY)",
                  "SecField" = "ifelse((is.na(SecField) & !is.na(bSecField)), bSecField, SecField)",
                  "SecField2" = "ifelse((is.na(SecField2) & !is.na(bSecField2)), bSecField2, SecField2)",
                  "rxHHpop" = "ifelse(!is.na(bSecField), 1, rxHHpop)") 

if (geog == 4) {
  NULL 
} else {
  hhp.upd.vars <- hhp.upd.vars[c(1:2,4)]
}

# HHP no existing development
if (geog == 4) {
  hhp.upd.vars2 <- c("COUNTY" = "ifelse((is.na(COUNTY) & !is.na(bCOUNTY)), bCOUNTY, COUNTY)",
                     "SecField" = "ifelse((is.na(SecField) & !is.na(bSecField)), bSecField, SecField)",
                     "SecField2" = "ifelse((is.na(SecField2) & !is.na(bSecField2)), bSecField2, SecField2)",
                     "rxHHpop" = "ifelse((!is.na(bCOUNTY) & bCOUNTY == COUNTY), 1, rxHHpop)")
} else {
  hhp.upd.vars2 <- c("rxHHpop" = "ifelse((!is.na(bSecField) & bSecField == SecField), 1, rxHHpop)")
}


# exdevjb
exdevjb.upd.var <- c("edunits" = "ifelse(is.na(edunits), 0, edunits)",
                     "bCOUNTY" = 'ifelse(is.na(COUNTY), "Not Available", COUNTY)',
                     "bSecField" = 'ifelse(is.na(SecField), "Not Available", SecField)',
                     "bSecField2" = 'ifelse(is.na(SecField2), "Not Available", SecField2)')
if (geog == 4) {
  NULL
} else if (geog == 1) {
  exdevjb.upd.var <- c(exdevjb.upd.var[1], "bCOUNTY" = "COUNTY", "bSecField" = "SecField")
} else {
  exdevjb.upd.var <- exdevjb.upd.var[c(1:3)]
}

# fill group values
fill.grp.var <- c("bCOUNTY" = 'ifelse(is.na(COUNTY), "Not Available", COUNTY)',
                  "bSecField" = 'ifelse(is.na(SecField), "Not Available", SecField)',
                  "bSecField2" = 'ifelse(is.na(SecField2), "Not Available", SecField2)')

if (geog == 4) {
  NULL
} else if (geog == 1) {
  fill.grp.var <- c("bCOUNTY" = "COUNTY", "bSecField" = "SecField")
} else {
  fill.grp.var <- fill.grp.var[1:2]
}


# Create RDS (do only once)------------------------------------------------

# columns = c("STATEFP10",  "COUNTYFP10", "TRACTCE10", "BLOCKCE10", "GEOID10")

# raw.ofm.fn <- "ofm_saep.csv"
# raw.ofm <- read.csv(file.path(raw.ofm.dir, raw.ofm.fn), header = TRUE, colClasses = lapply(columns, function(x) x = "character"))
# raw.ofm[,28:ncol(raw.ofm)] <- lapply(raw.ofm[,28:ncol(raw.ofm)], as.numeric)
# saveRDS(raw.ofm,  file.path(raw.ofm.dir,"ofm_saep.rds"))
# rm(raw.ofm)

# raw.prcl <- file.path(root.dir, bnd.year, "GIS")
# raw.prcl.fn <- "Parcels_wCityBlk.dbf"
# parcel <- read.dbf(file.path(raw.prcl, raw.prcl.fn))
# saveRDS(parcel, file.path(input.dir, "parcel.rds"))
# rm(parcel)

# raw.bldg <- file.path(root.dir, bnd.year)
# raw.bldg.fn <- "buildings14.csv"
# building <- read.csv(file.path(raw.bldg, raw.bldg.fn))
# saveRDS(building, file.path(input.dir, "bldg.rds"))
# rm(building)

# raw.prcl <- file.path(root.dir, bnd.year, "GIS")
# raw.prcl.fn <- "Parcels_wAnnxBlk.dbf"
# parcel_annex <- read.dbf(file.path(raw.prcl, raw.prcl.fn))
# saveRDS(parcel_annex, file.path(input.dir, "parcel_annex.rds"))
# rm(parcel_annex)

# raw.prcl <- file.path(root.dir, bnd.year, "GIS")
# raw.prcl.fn <- "Parcels_wUGABlk.dbf"
# parcel_uga <- read.dbf(file.path(raw.prcl, raw.prcl.fn))
# saveRDS(parcel_uga, file.path(input.dir, "parcel_uga.rds"))
# rm(parcel_uga)

# raw.prcl <- file.path(root.dir, bnd.year, "GIS")
# raw.prcl.fn <- "Parcels_wRuralCityBlk.dbf"
# parcel_rc <- read.dbf(file.path(raw.prcl, raw.prcl.fn))
# saveRDS(parcel_rc, file.path(input.dir, "parcel_rc.rds"))
# rm(parcel_rc)
