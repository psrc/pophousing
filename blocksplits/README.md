# Block Splitting Procedure
This directory contains a set of scripts to split block level household population (HHpop) and group quarter population (GQpop) estimates for an area of interest (AOI) for a single year based on the best available parcel level housing unit estimates. This process is not a fully automated procedure; it will require GIS processing prior to and during the workflow.

The final output will be a dataframe consisting of relevant blocks with housing units and/or population disaggregated by its county and the AOI it falls or splits into. Each block is assigned a HHpop ratio (ranging from 0 to 1) and a GQpop ratio (either 0 or 1). The GQpop is not split but is instead fully assigned to one of the areas it splits into.

## Home Directory
Set up the project repository in J:\\Projects\\Population\\OFMPopHsgData\\OFMSAEP\\Custom_Ests\\HHPop_est_Block_Split and use the same directory structure as seen in previous projects. Each estimate year will have its own sub-directory.

## Prepare Input Files  

### GIS

If area of interest is a full regional coverage:  
    1. Spatial join the parcels feacture class with census blocks. 
    2. Spatial join the resulting parcel feature class in 1 with the AOI.

If area of interest does not have regional coverage (e.g. regional growth centers, park buffers, etc.):  
    1. Complete steps 1 and 2
    2. Select all census block features that intersect the AOI and export the attribute table as a .dbf to the 'r_temp' directory.
    3. Add new binary column (0 or 1) in resulting parcel feature class called 'Filter'. All parcels that lie within the blocks that intersect the area of interest will be assigned '1'.
    4. Export parcel feature class as a shapefile
    
### Create RDS

Edit create_rds.R to create an RDS file (a compressed R file) from the buildings and parcels datasets.

## Edit Settings

Edit settings in settings.R

## Run

Use block_splits_output.R to complete the block-splitting process. Do not hit 'Source' as this process involves several intermediate GIS steps. Run line by line or hightlight chunks to run.

Intermediate GIS steps will require spatial joining block centroids to determine where they land in the AOI.

## Post-script QC

After the final dataframe has been exported, check that the sum of the ratios by block for HHpop and GQpop equal 1. Rounding may cause some aggregations to sum to .99 or 1.01. 



    
    