mydir <- "C:/Users/CLam/Desktop/pophousing/blocksplits"
source(file.path(mydir, "settings.R"))


# create building and parcel rds ------------------------------------------


# raw.bldg <- file.path(root.dir, bnd.year)
# raw.bldg.fn <- "buildings14.csv"
# building <- read.csv(file.path(raw.bldg, raw.bldg.fn))
# saveRDS(building, file.path(input.dir, "bldg.rds"))
# rm(building)

raw.prcl <- file.path(root.dir, bnd.year, "GIS")
raw.prcl.fn <- "parcels_wjuris_buffer.dbf"
parcels2rds <- read.dbf(file.path(raw.prcl, raw.prcl.fn))
saveRDS(parcels2rds, file.path(input.dir, "parcels.rds"))
rm(parcels2rds)


# create ofm rds ----------------------------------------------------------


# columns = c("STATEFP10",  "COUNTYFP10", "TRACTCE10", "BLOCKCE10", "GEOID10")

# raw.ofm.fn <- "ofm_saep.csv"
# raw.ofm <- read.csv(file.path(raw.ofm.dir, raw.ofm.fn), header = TRUE, colClasses = lapply(columns, function(x) x = "character"))
# raw.ofm[,28:ncol(raw.ofm)] <- lapply(raw.ofm[,28:ncol(raw.ofm)], as.numeric)
# saveRDS(raw.ofm,  file.path(raw.ofm.dir,"ofm_saep.rds"))
# rm(raw.ofm)



