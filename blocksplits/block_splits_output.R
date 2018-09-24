# Highlight sections to run. Do not hit Source.
# User can alter data.year and other directory paths under User Inputs section. 
# All files will be written out to temp.dir. Intermediate GIS .dbfs are read from temp.dir


# User inputs -------------------------------------------------------------


mydir <- "C:/Users/CLam/Desktop/pophousing/blocksplits"
source(file.path(mydir, "settings.R"))
source(file.path(mydir, "block_splits_inputs.R"))


# functions ---------------------------------------------------------------


export.dbf <- function(table){
  f <- switch(deparse(substitute(table)), "df.no.prcl" = "bnoprcl", "df.split" = "bnoed_split", "gq.split" = "gqsplit")
  write.dbf(table, file.path(temp.dir, paste0(prefix, f, ".dbf")))
}


# Filter and assemble data -------------------------------------------------------------


ifelse(geog > 1, prcl <- parcel %>% filter(Filter == 1), prcl <- parcel)
prcl <- prcl %>%
  mutate_at(orig.vars, funs(as.character)) %>%
  rename_(.dots = vars)

# filter ofm data
if (geog > 1) {
  ofm.est <- ofm %>% 
    select(starts_with("COUNTY"), GEOID10, contains(data.year)) %>% 
    filter(GEOID10 %in% blocks$GEOID10) 
} else {
  ofm.est <- ofm %>% select(starts_with("COUNTY"), GEOID10, contains(data.year))
}

ofm.est.a <- paste0(colnames(ofm.est)[grep("^HU", colnames(ofm.est))], '> 0 |', colnames(ofm.est)[grep("^GQ", colnames(ofm.est))], '> 0')
ofm.est.f <- ofm.est %>% filter_(ofm.est.a)

# juris-block/all grouping variables relationship (is not full universe of relevant blocks)
jb <- prcl %>% select_(.dots = c("GEOID10", new.vars)) %>% distinct() %>% mutate(GEOID10 = as.character(GEOID10))

# split-blocks
sb <- jb %>% count(GEOID10) %>% filter(n > 1) %>% arrange(desc(n))

# sum # of residential units (kitchen sink)
exdev <- bldg %>% group_by(parcel_id) %>% summarise(edunits = sum(residential_units))

# existing dev & # of parcels by block
exdevb <- prcl %>% left_join(exdev, by = c("PSRC_ID" = "parcel_id")) %>%
  mutate(edunits = ifelse(is.na(edunits), 0, edunits)) %>%
  group_by(GEOID10) %>%
  summarise(parcels = n(), exdunits = sum(edunits))

# current year & existing dev by block (warning GEOID10 is factor in one of joining tables)
curr.exdevb <- ofm.est.f %>% left_join(exdevb, by = "GEOID10") %>% select(GEOID10, parcels, exdunits, starts_with("HU"), starts_with("GQ"))

# create df
df <- ofm.est.f %>% 
  left_join(jb, by = "GEOID10") %>% 
  select_(.dots = c(colnames(jb),'starts_with("POP")', 'starts_with("HHP")', 'starts_with("GQ")', 'starts_with("HU")', 'starts_with("OHU")')) %>%
  mutate(rxHHpop = 0, rxGQpop = 0) %>%
  mutate_at(new.vars, funs(as.character)) 


## Household Pop -----------------------------------------------------------

# No Parcels --------------------------------------------------------------


# blocks with no parcels/existing dev., export for GIS (Needs 'County' and 'SecField' populated)
df.no.prcl <- curr.exdevb %>% filter(is.na(parcels) | parcels == 0)
export.dbf(df.no.prcl)

# Stop (Now do GIS work) --------------------------------------------------

# check for is.na(SecField|COUNTY) before reading
# read bloxnop.dbf
bloxnop <- read.dbf(file.path(temp.dir, "bloxnop.dbf"))

# adjust field names from .dbf as necessary
if (geog == 4) {
  bnop <- bloxnop %>%
    select(GEOID10 = GEOID10, bCOUNTY = CNTYNAME, bSecField = CITYNAME, bSecField2 = add_remove) %>%
    mutate_all(funs(as.character))
} else {
  bnop <- bloxnop %>%
    select(GEOID10 = GEOID10, bCOUNTY = CNTYNAME, bSecField = symbol) %>% 
    mutate_all(funs(as.character))
}

# update COUNTY, SecField, and rxHHpop 
df <- df %>% 
  left_join(bnop, by ="GEOID10") %>%
  mutate_(.dots = hhp.upd.vars) %>%
  select_(.dots = paste0("-", b.vars))


# No Exisiting Development -------------------------------------------------


# blocks with no existing dev
df.no.exdev <- curr.exdevb %>% filter(!is.na(parcels) & (exdunits  == 0 | is.na(exdunits)))

# blocks with no exdev (that are split)
df.split <- df.no.exdev %>% inner_join(sb, by = "GEOID10")
export.dbf(df.split)

# Stop (Now do GIS work) ---------------------------------------------------------

# read bloxnoed_split.dbf
bloxnoed.split <- read.dbf(file.path(temp.dir, "bloxnoed_split.dbf"))

if (geog == 4) {
  bnoed.split <- bloxnoed.split %>%
    select(GEOID10 = GEOID10, bCOUNTY = UGACNTY, bSecField = CITYNAME, bSecField2 = add_remove) %>%
    mutate_all(funs(as.character))
} else if (geog == 5) {
  bnoed.split <- bloxnoed.split %>%
    select(GEOID10 = GEOID10, bCOUNTY = CNTYNAME, bSecField = symbol) %>% 
    mutate_all(funs(as.character))

} else {
  bnoed.split <- bloxnoed.split %>%
    select(GEOID10 = GEOID10, bCOUNTY = CNTYNAME, bSecField = TOD_Area3) %>% 
    mutate_all(funs(as.character))
}

# update rxHHpop
if (geog == 5) {
  df <- df %>%
    left_join(bnoed.split, by ="GEOID10") %>%
    mutate(COUNTY = ifelse(is.na(COUNTY) & bCOUNTY == 'Not Available', bCOUNTY, COUNTY),
           SecField = ifelse(is.na(SecField) & bSecField == 'Not Available', bSecField, SecField)) %>%
    select_(.dots = paste0("-", b.vars))
  
  df <- df %>%
    left_join(bnoed.split, by =c("GEOID10" = "GEOID10" , "COUNTY" = "bCOUNTY")) %>%
    mutate_(.dots = hhp.upd.vars2) %>%
    select(-starts_with("b"))
} else {
  df <- df %>%
    left_join(bnoed.split, by ="GEOID10") %>%
    mutate_(.dots = hhp.upd.vars2) %>%
    select_(.dots = paste0("-", b.vars))
}


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
  mutate_(.dots = exdevjb.upd.var) %>%
  group_by_(.dots = c("GEOID10", b.vars)) %>%
  summarise(juris_units = sum(edunits))

# calculate hhpop ratios
bs.ratios <- exdevjb %>% inner_join(exdevb, by = "GEOID10") %>%
  mutate(bs_rxHHpop = round(juris_units/exdunits, 3)) %>% # contains Nan
  arrange(GEOID10) %>%
  filter(!is.nan(bs_rxHHpop)) 

# fill missing group values
df <- df %>% 
  mutate_(.dots = fill.grp.var) %>%
  select(GEOID10, starts_with("b"), 2:ncol(df))

# update rxHHpop
df <- df %>% left_join(bs.ratios, by = c("GEOID10", b.vars)) %>%
  mutate(rxHHpop = ifelse(!is.na(bs_rxHHpop), bs_rxHHpop, rxHHpop)) %>%
  select(-(juris_units:bs_rxHHpop))

# calculate splitblkHHpop
df <- df %>%
  mutate_(splitblkHHpop = paste0(colnames(df)[grep("^HHP", colnames(df))], '* rxHHpop'))


# HH QC ----------------------------------------------------------------------


# sum by splitblkHHpop, recalc.blocks will list blocks where rx may not sum to 1
qc <- df %>% group_by(GEOID10) %>% summarise(sum_splitblkHHpop = round(sum(splitblkHHpop), 3))
recalc.blocks <- qc %>% left_join(ofm.est.f, by = "GEOID10") %>% 
  filter_(paste0('sum_splitblkHHpop !=', colnames(ofm.est.f)[grep("^HHP", colnames(ofm.est.f))]))
# write.csv(df, file.path(temp.dir, "rxblkhhpop.csv"))

# quick-check, print cnty summary (for standard block-splits only)
ofm.est.f %>% group_by(COUNTYFP10) %>% 
  summarise_(OFM_HHP = paste0('sum(', colnames(ofm.est.f)[grep("^HHP", colnames(ofm.est.f))],')'), OFM_GQ = paste0('sum(', colnames(ofm.est.f)[grep("^GQ", colnames(ofm.est.f))], ')'))
df %>% group_by(COUNTY) %>% summarise(HHP = sum(splitblkHHpop))


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
export.dbf(gq.split)

# gq.split2 <- gq.split %>% select(GEOID10, COUNTY) %>% distinct(GEOID10, COUNTY)
# write.dbf(gq.split2, file.path(temp.dir, paste0(prefix, "gqsplit2", ".dbf")))

# Stop (now do GIS work) --------------------------------------------------

bloxgq.split <- read.dbf(file.path(temp.dir, "bloxgqs.dbf"))
bgq.split <- bloxgq.split %>% 
  filter(gqyes == 1) %>% 
  select(GEOID10, bCOUNTY = bCOUNTY, bSecField = bSecField, gqyes)# 

# update rxGQpop
df <- df %>% left_join(bgq.split, by = c("GEOID10", b.vars)) %>%
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
recalc.blocks2 <- qc2 %>% 
  left_join(ofm.est.f, by = "GEOID10") %>% 
  filter_(paste0('sum_splitblkGQpop !=', colnames(ofm.est.f)[grep("^GQ", colnames(ofm.est.f))]))
# write.csv(df, file.path(temp.dir, "rxblkgqpop.csv"))


# Null rxHHP QC------------------------------------------------------------

check.rx <- df %>% group_by(GEOID10) %>% summarise(srxHHpop = sum(rxHHpop)) %>% arrange(desc(srxHHpop))
check.rx.na <- check.rx %>% filter(is.na(srxHHpop))
check.rx2 <- df %>% semi_join(check.rx.na , by = "GEOID10")

# df <- df %>% 
#   mutate(rxHHpop = ifelse(is.na(rxHHpop), 0, rxHHpop))


# Export ------------------------------------------------------------------

df <- df %>% select(-COUNTY, -SecField)#select(GEOID10, CNTYNAME = bCOUNTY, JURIS = bSecField, contains(data.year))
write.xlsx(df, file.path(temp.dir, "rxblksplit_xlsx.xlsx"))
# write.csv(df, file.path(temp.dir, "rxblksplit.csv"), row.names = FALSE)

# quick-check, print cnty summary
ofm.est.f %>% 
  group_by(COUNTYFP10) %>% 
  summarise_(OFM_HHP = paste0('sum(', colnames(ofm.est.f)[grep("^HHP", colnames(ofm.est.f))],')'), OFM_GQ = paste0('sum(', colnames(ofm.est.f)[grep("^GQ", colnames(ofm.est.f))], ')')) 
df %>% group_by(bCOUNTY) %>% summarise(HHP = sum(splitblkHHpop), GQ = sum(splitblkGQpop)) 


