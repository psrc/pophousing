# variables
if (geog == 4) {
  vars <- c("COUNTY" = col.county.name, "SecField" = col.secondary.field, "SecField2" = "add_remove")
} else {
  vars <- c("COUNTY" = col.county.name, "SecField" = col.secondary.field)
}

# vars <- switch(geog,
#                c("COUNTY" = "CNTYNAME", "SecField" = "TOD_Area3"),#"JURIS_1" #TOD #JURIS
#                c("COUNTY" = "ANXCNTY", "SecField" = "ANXCITY"),
#                c("COUNTY" = "UGACNTY", "SecField" = "add_remove"),
#                c("COUNTY" = "UGACNTY", "SecField" = "CITYNAME", "SecField2" = "add_remove"),
#                c("COUNTY" = "CNTYNAME", "SecField" = "symbol")) #COUNTY #centers #TOD2 #Subarea #TOD_Area5
# 

# Read data ---------------------------------------------------------------


ofm <- readRDS(file.path(raw.ofm.dir,"ofm_saep.rds"))
bldg <- readRDS(file.path(input.dir,"bldg.rds"))

parcel <- readRDS(file.path(input.dir, parcel.file.nm))
blocks <- read.dbf(file.path(temp.dir, blocks.file.nm))


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
