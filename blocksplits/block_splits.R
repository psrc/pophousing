# Highlight sections to run. Do not hit Source.
# User can alter data.year and other directory paths under User Inputs section. 
# All files will be written out to temp.dir. Intermediate GIS .dbfs are read from temp.dir

library(tidyverse)
library(foreign)


# User inputs -------------------------------------------------------------


bnd.year <- "2017"
data.year <- c("2008")
root.dir <- "J:/Projects/Population/OFMPopHsgData/OFMSAEP/Custom_Ests/HHPop_est_Block_Spilt"
input.dir <- file.path(root.dir, bnd.year, "r_input")
temp.dir <- file.path(root.dir, bnd.year, paste0("est", data.year),"r_temp")
raw.ofm.dir <- "J:/OtherData/OFM/SAEP/SAEP Extract_2017_10October03/"


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


# Read data ---------------------------------------------------------------


ofm <- readRDS(file.path(raw.ofm.dir,"ofm_saep.rds"))
prcl <- readRDS(file.path(input.dir,"parcel.rds"))
bldg <- readRDS(file.path(input.dir,"bldg.rds"))

# filter ofm data
ofm.est <- ofm %>% select(starts_with("COUNTY"), GEOID10, contains(data.year))
ofm.est.a <- paste0(colnames(ofm.est)[grep("^HU", colnames(ofm.est))], '> 0 |', colnames(ofm.est)[grep("^GQ", colnames(ofm.est))], '> 0')
ofm.est.f <- ofm.est %>% filter_(ofm.est.a)

# juris-block relationship
jb <- prcl %>% select(CNTYNAME, JURIS = JURIS_1, GEOID10) %>% distinct() 

# split-blocks
sb <- jb %>% count(GEOID10) %>% filter(n > 1) %>% arrange(desc(n))

# sum # of residential units
exdev <- bldg %>% group_by(parcel_id) %>% summarise(edunits = sum(residential_units))

# existing dev & # of parcels by block
exdevb <- prcl %>% left_join(exdev, by = c("PSRC_ID" = "parcel_id")) %>%
  mutate(edunits = ifelse(is.na(edunits), 0, edunits)) %>%
  group_by(GEOID10) %>%
  summarise(parcels = n(), exdunits = sum(edunits))

# current year & existing dev by block
curr.exdevb <- ofm.est.f %>% left_join(exdevb, by = "GEOID10") %>% select(GEOID10, parcels, exdunits, starts_with("HU"), starts_with("GQ"))

# create df
df <- ofm.est.f %>% 
  left_join(jb, by = "GEOID10") %>% 
  select(GEOID10, COUNTYFP10, JURIS, starts_with("POP"), starts_with("HHP"), starts_with("GQ"), starts_with("HU"), starts_with("OHU")) %>%
  mutate(rxHHpop = 0, rxGQpop = 0, JURIS = as.character(JURIS)) 


## Household Pop -----------------------------------------------------------

# No Parcels --------------------------------------------------------------


# blocks with no parcels/existing dev., export for GIS 
df.no.prcl <- curr.exdevb %>% filter(is.na(parcels) | parcels == 0)
write.dbf(df.no.prcl, file.path(temp.dir, "bnoprcl.dbf"))


# Stop (Now do GIS work) --------------------------------------------------

# check for is.na(JURIS) before reading
# read bloxnop.dbf
bloxnop <- read.dbf(file.path(temp.dir, "bloxnop.dbf"))
bnop <- bloxnop %>%
  select(GEOID10 = bnoprcl_GE, bCNTYNAME = CNTYNAME, bJURIS = JURIS) %>%
  mutate_all(funs(as.character))

# update JURIS and rxHHpop 
df <- df %>% 
  left_join(bnop, by ="GEOID10") %>%
  mutate(JURIS = ifelse((is.na(JURIS) & !is.na(bJURIS)), bJURIS, JURIS)) %>%
  mutate(rxHHpop = ifelse(!is.na(bJURIS), 1, rxHHpop)) %>%
  select(-(bCNTYNAME:bJURIS))


# No Exisiting Development -------------------------------------------------


# blocks with no existing dev
df.no.exdev <- curr.exdevb %>% filter(!is.na(parcels) & (exdunits  == 0 | is.na(exdunits)))

# blocks with no exdev (that are split)
df.split <- df.no.exdev %>% inner_join(sb, by = "GEOID10")
write.dbf(df.split, file.path(temp.dir, "bnoed_split.dbf"))


# Stop (Now do GIS work) ---------------------------------------------------------


# read bloxnoed_split.dbf
bloxnoed.split <- read.dbf(file.path(temp.dir, "bloxnoed_split.dbf"))
bnoed.split <- bloxnoed.split %>%
  select(GEOID10 = bnoed_spli, bCNTYNAME = CNTYNAME, bJURIS = JURIS) %>%
  mutate_all(funs(as.character))

# update rxHHpop
df <- df %>% 
  left_join(bnoed.split, by ="GEOID10") %>%
  mutate(rxHHpop = ifelse((!is.na(bJURIS) & bJURIS == JURIS), 1, rxHHpop)) %>%
  select(-(bCNTYNAME:bJURIS))

# blocks with no exdev (not-split)
df.nosplit <- df.no.exdev %>% anti_join(df.split, by = "GEOID10") %>% 
  select(GEOID10) %>%
  mutate(nosplit = 1)

# update rxHHpop
df <- df %>% left_join(df.nosplit, by = "GEOID10") %>%
  mutate(rxHHpop = ifelse(!is.na(nosplit), 1, rxHHpop)) %>%
  select(-nosplit)


# Calculate Ratios --------------------------------------------------------

# sum of residential units by juris and block
exdevjb <- prcl %>% left_join(exdev, by = c("PSRC_ID" = "parcel_id")) %>%
  mutate(edunits = ifelse(is.na(edunits), 0, edunits)) %>%
  group_by(GEOID10, CNTYNAME, JURIS_1) %>%
  summarise(juris_units = sum(edunits)) %>%
  rename(JURIS = JURIS_1) 

# calculate hhpop ratios
bs.ratios <- exdevjb %>% inner_join(exdevb, by = "GEOID10") %>%
  mutate(bs_rxHHpop = round(juris_units/exdunits, 2)) %>% # contains Nan
  arrange(GEOID10) %>%
  mutate(COUNTYFP10 = recode(CNTYNAME, "King" = "033", "Kitsap" = "035", "Pierce" = "053", "Snohomish" = "061")) %>%
  filter(!is.nan(bs_rxHHpop))
  
df <- df %>% left_join(bs.ratios, by = c("GEOID10", "COUNTYFP10", "JURIS")) %>%
  mutate(rxHHpop = ifelse(!is.na(bs_rxHHpop), bs_rxHHpop, rxHHpop)) %>%
  select(-(CNTYNAME:bs_rxHHpop))

# calculate splitblkHHpop
df <- df %>%
  mutate_(splitblkHHpop = paste0(colnames(df)[grep("^HHP", colnames(df))], '* rxHHpop'))


# HH QC ----------------------------------------------------------------------

# sum by splitblkHHpop, recalc.blocks will list blocks where rx may not sum to 1
qc <- df %>% group_by(GEOID10) %>% summarise(sum_splitblkHHpop = round(sum(splitblkHHpop), 3))
recalc.blocks <- qc %>% left_join(ofm.est.f, by = "GEOID10") %>% filter_(paste0('sum_splitblkHHpop !=', colnames(ofm.est.f)[grep("^HHP", colnames(ofm.est.f))]))
# write.csv(df, file.path(temp.dir, "rxblkhhpop.csv"))

# quick-check, print cnty summary
ofm.est.f %>% group_by(COUNTYFP10) %>% summarise_(OFM_HHP = paste0('sum(', colnames(ofm.est.f)[grep("^HHP", colnames(ofm.est.f))],')'), OFM_GQ = paste0('sum(', colnames(ofm.est.f)[grep("^GQ", colnames(ofm.est.f))], ')'))
df %>% group_by(COUNTYFP10) %>% summarise(HHP = sum(splitblkHHpop))


## GQ pop ------------------------------------------------------------------

# blocks with GQ but no parcels
gq.noprcl <- curr.exdevb %>% 
  filter_(paste0('(is.na(parcels) | parcels == 0) &', colnames(curr.exdevb)[grep("^GQ", colnames(curr.exdevb))], '>0')) %>%
  mutate(gqnoprcl = 1) %>%
  select(GEOID10, gqnoprcl)

# update rxGQpop
df <- df %>% left_join(gq.noprcl, by = "GEOID10") %>%
  mutate(rxGQpop = ifelse(!is.na(gqnoprcl), 1, rxGQpop)) %>%
  select(-gqnoprcl)

# GQ blocks (split)
gq.split <- df %>% inner_join(sb, by = "GEOID10") %>%
  filter_(paste0(colnames(curr.exdevb)[grep("^GQ", colnames(curr.exdevb))], '>0'))
write.dbf(gq.split, file.path(temp.dir, "gqsplit.dbf"))


# Stop (now do GIS work) --------------------------------------------------

bloxgq.split <- read.dbf(file.path(temp.dir, "bloxgqs.dbf"))
bgq.split <- bloxgq.split %>% filter(gqyes == 1) %>% select(GEOID10, COUNTYFP10, JURIS, gqyes)

# update rxGQpop
df <- df %>% left_join(bgq.split, by = c("GEOID10", "COUNTYFP10", "JURIS")) %>%
  mutate(rxGQpop = ifelse(!is.na(gqyes), 1, rxGQpop)) %>%
  select(-gqyes)

gq.nosplit <- df %>%
  filter_(paste0(colnames(df)[grep("^GQ", colnames(df))], '>0')) %>%
  group_by(GEOID10) %>%
  summarise(gqnosplit = n()) %>%
  filter(gqnosplit == 1)

# update rxGQpop
df <- df %>% left_join(gq.nosplit, by = "GEOID10") %>%
  mutate(rxGQpop = ifelse(!is.na(gqnosplit), 1, rxGQpop)) %>%
  select(-gqnosplit)

# calculate splitblkGQpop
df <- df %>%
  mutate_(splitblkGQpop = paste0(colnames(df)[grep("^GQ", colnames(df))], '* rxGQpop'))


# GQ QC -------------------------------------------------------------------


# sum by splitblkGQpop, recalc.blocks2 will list blocks where rx may not sum to 1
qc2 <- df %>% group_by(GEOID10) %>% summarise(sum_splitblkGQpop = round(sum(splitblkGQpop), 3))
recalc.blocks2 <- qc2 %>% left_join(ofm.est.f, by = "GEOID10") %>% filter_(paste0('sum_splitblkGQpop !=', colnames(ofm.est.f)[grep("^GQ", colnames(ofm.est.f))]))#
# write.csv(df, file.path(temp.dir, "rxblkgqpop.csv"))


# Export ------------------------------------------------------------------


write.csv(df, file.path(temp.dir, "rxblksplit.csv"))
# quick-check, print cnty summary
ofm.est.f %>% group_by(COUNTYFP10) %>% summarise_(OFM_HHP = paste0('sum(', colnames(ofm.est.f)[grep("^HHP", colnames(ofm.est.f))],')'), OFM_GQ = paste0('sum(', colnames(ofm.est.f)[grep("^GQ", colnames(ofm.est.f))], ')'))
df %>% group_by(COUNTYFP10) %>% summarise(HHP = sum(splitblkHHpop), GQ = sum(splitblkGQpop))
