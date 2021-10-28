# Packages ----

library(raster)
library(gurobi)
library(prioritizr)
library(rgeos)
library(rgdal)
library(spatialEco)
library(climateStability)
library(RandomFields)
library(tmap)
library(RColorBrewer)
library(viridis)
library(ggpubr)
library(rstatix)
library(units)
library(dplyr)
library(readr)
library(R.devices)
library(landscapemetrics)

# Load Habitat Suitability Models (HSMs) ----

# load habitat selection layers
nesting <- raster("nesting_HSM.tif")
brood <- raster("brood_HSM.tif")
winter <- raster("winter_HSM.tif")

# create CRS, extent, and a bounds objects for projecting
# and creating rasters
CRS_obj <- crs(brood)
ext_obj <- extent(brood)
bounds <- raster(ext_obj, crs = CRS_obj, res = 120)
# create a raster with area outside of
# the study boundary set as NA 
bckg <- brood
bckg[bckg!=0] <- 0 # change values to 0
bckg[!is.finite(bckg)] <- NA # set outside area as NA

# Big Sandy Reservoir (BSR) & Latitude Crop HSMs ----

# reduce signal on Big Sandy Reservoir for Brood & Winter
# used website: http://rcn.montana.edu/Resources/Converter.aspx
# to find coordinates to limit BSR
# by dropping pins in an interactive map converting to USGS
BSR <- rasterToPoints(brood) # ceate matrix to alter values at BSR
i <- 1
while (i <= dim(BSR)[1]) {
  if (BSR[i,1] >= 626000 & BSR[i,1] <= 632800 & BSR[i,2]
      >= 4678627 & BSR[i,2] <= 4684077) {
    BSR[i,3] <- BSR[i,3]/5
  }
  i <- i + 1
} # insert 4 coords, divide values by 5
brood <- rasterFromXYZ(BSR, res = c(120,120), crs = CRS_obj)
# repeat for winter
BSR <- rasterToPoints(winter)
i <- 1
while (i <= dim(BSR)[1]) {
  if (BSR[i,1] >= 626000 & BSR[i,1] <= 632800 & BSR[i,2]
      >= 4678627 & BSR[i,2] <= 4684077) {
    BSR[i,3] <- BSR[i,3]/5
  }
  i <- i + 1
} # insert 4 coords, divide values by 5
winter <- rasterFromXYZ(BSR, res = c(120,120), crs = CRS_obj)

# remove BSR object for memory, wont be used again
rm(BSR)


# Alter HSMs with DDCT ----

# load in DDCT layer (rasterized in ArcMap using SubType)
DDCT <- raster("StatewideExistingDisturbance.tif")
DDCT <- projectRaster(DDCT, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# isolate key disturbances (ag, burns, oil and gas, and roads)
DDCT_burns <- DDCT
DDCT_burns[DDCT_burns!=8] <- 0
# reduce by 20%
DDCT_burns[DDCT_burns!=0] <- -0.2 
DDCT_burns[DDCT_burns==0] <- NA
DDCT_ag <- DDCT
DDCT_ag[DDCT_ag!=2] <- 0
# reduce by 10%
DDCT_ag[DDCT_ag!=0] <- -0.1
DDCT_ag[DDCT_ag==0] <- NA
DDCT_roads <- DDCT
DDCT_roads[DDCT_roads!=1] <- 0
# reduce by 50%
DDCT_roads[DDCT_roads!=0] <- -0.5
DDCT_roads[DDCT_roads==0] <- NA
DDCT_og <- DDCT
DDCT_og[DDCT_og!=4] <- 0
# reduce by 50%
DDCT_og[DDCT_og!=0] <- -0.5
DDCT_og[DDCT_og==0] <- NA
# create HSM layers decreasing the probability of occurance
# on burned and agricultural areas
nesting_ddct <- sum(nesting, DDCT_burns, DDCT_ag, 
                    na.rm=TRUE)
nesting_ddct[nesting_ddct<0] <- 0
# remove background area
nesting_ddct <- sum(nesting_ddct, bckg, na.rm = FALSE)
brood_ddct <- sum(brood, DDCT_burns, DDCT_ag, 
                  na.rm=TRUE)
brood_ddct[brood_ddct<0] <- 0
brood_ddct <- sum(brood_ddct, bckg, na.rm = FALSE)
winter_ddct <- sum(winter, DDCT_burns, DDCT_ag, 
                   na.rm=TRUE)
winter_ddct[winter_ddct<0] <- 0
winter_ddct <- sum(winter_ddct, bckg, na.rm = FALSE)

# ensure each ddct layer has a unique name
names(nesting_ddct) <- "layer.1"
names(winter_ddct) <- "layer.2"
names(brood_ddct) <- "layer.3"


# Load PACs, Land Managers, KDE, abandoned leks, Other Species ----

# core areas
PACs <- raster('PACs.tif')
PACs <- projectRaster(PACs, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# merge PACs
PACs[PACs!=0] <- 1
PACs[is.na(PACs[])] <- 0

# load Manager types 
# PADUS data
# clipped in ArcMap
mang_type <- raster("PADUS2_Mngr_Type.tif")
mang_type <- projectRaster(mang_type, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# remove NAs so these areas are not available for selection
mang_type[is.na(mang_type)==TRUE] <- 0

# also kernel density lek file (see _KDE_Raster_Generation.R)
kde_lek <- raster('KDE_lek.tif')
kde_lek <- projectRaster(kde_lek, bounds, crs=CRS_obj, method = 'ngb', res = 120)

abandon_lek <- raster("kde_a_lek.tif")
abandon_lek <- projectRaster(abandon_lek, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# create aggregated version
abandon_lek_agg <- aggregate(abandon_lek,
                             fact = 13.41, expand = FALSE)

# winter obs KDE
winter_obs <- raster('KDE_winter.tif')
winter_obs <- projectRaster(winter_obs, bounds, crs=CRS_obj, method = 'ngb', res = 120)

# nesting observations KDE
nest_obs <- raster('KDE_nest.tif')
nest_obs <- projectRaster(nest_obs, bounds, crs=CRS_obj, method = 'ngb', res = 120)

# Elk Migration Routes
Elk <- raster('Elk_WY_SouthWindRiver_Routes11.tif')
Elk <- projectRaster(Elk, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# merge routes
Elk[Elk!=0] <- 1

# Mule Deer Corridors
MD <- raster('MD_WY_SubletteWGFDDesignated11.tif')
MD <- projectRaster(MD, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# merge corridors
MD[MD!=0] <- 1


# Load Expert Opinion Modified HSMs ----

# how raster were created in ArcMap
# ARCMAP STEPS:
# create a layer with highlighted winter areas
# first pdfs were converted to .png files 
# using website: https://pdf2png.com/
# then pngs were uploaded in arcmap
# georeferencing and changepoints were used to realign
# images with study extent
# from website: https://pro.arcgis.com/en/pro-app/latest/help/data/imagery/overview-of-georeferencing.htm
# "first-order polynomial transformation is 
# commonly used to georeference an image" which is what was used
# images were exported from arcmap as rasters manually entering the
# appropriate row and column lengths (1513 and 1503)
# then re-imported into arcmap
# next use the editor tool 
# select the study boundary (polygon) to use as a template
# draw polygons around ares highlighted by experts
# select each drawn polygon and export selected areas

# load drawn polygons
nesting_polygons <- readOGR(dsn = ".", "nesting_expert_polygons")
brood_polygons <- readOGR(dsn = ".", "brood_expert_2")
winter_polygons <- readOGR(dsn = ".", "winter_expert_2t")
riparian_polygons <- readOGR(dsn = ".", "riparian_expert_1")

# use polygons to crop HSMs
nesting_expert <- mask(nesting, nesting_polygons)
brood_expert <- mask(brood, brood_polygons)
winter_expert <- mask(winter, winter_polygons)
brood_riparian_expert <- mask(brood, riparian_polygons)

# Load Landscape Data ----

# resistance layer
# this layer was clipped from J. Row's rangewide
# using code by K. Winiarski
resist <- raster("Rocksprings_resist.tif")
# reproject and match extents and crs
resist <- projectRaster(resist, bounds, crs=CRS_obj, method = 'bilinear', res = 120)
# invert raster to get landscape connectivity (LC) layer
LC_in <- raster.invert(resist)
# rescale this layer so values range from 0 - 1 
# without rescaling the features are too big for prioritizr
LC_rescale <- rescale0to1(LC_in)

# load in NLCD (national land cover database) layers 
# ArcMap was used to clip the NLCD layer to the study extent
NLCD <- raster("NLCD_2016_Land_Cover_L48_2013_Clip.tif")
NLCD <- projectRaster(NLCD, bounds, crs=CRS_obj, method = 'bilinear', res = 120)

# wildfire risk
# USGS database
fire <- raster("Fire_Danger.tif")
fire <- projectRaster(fire, bounds, crs=CRS_obj, method = 'bilinear', res = 120)
fire_agg <- aggregate(fire, fact = 13.41 , expand = FALSE)

# Load Development Data ----

# load in oil & gas development potential layer developed by Copeland et al., 2009
# data from 2013
# downloaded as a .gbd folder from researchgate 
# ArcMap was used to clip to study extent
og <- raster("WYOGPotential_TNC2010_Clip22.tif")
# match crs, extent and resample to match resolutions
og <- projectRaster(og, bounds, crs=CRS_obj, method = 'bilinear', res = 120)

# load pad scar raster
# clipped and converted in arcmap
# source: https://doi.org/10.3133/ds934
scar <- raster('padscar_2012_utm12_Clip_Poly11.tif')
scar <- projectRaster(scar, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# buffer padscars by 500 m to account for avoidance by sage-grouse
# literature source: 
scar_buff <- buffer(scar, 500)

# load cultivation conversion risk layer
# developed by Smith et al., 2016
# https://doi.org/10.1016/j.biocon.2016.06.006
ccr <- raster('wyoming_Clip1.tif')
ccr <- projectRaster(ccr, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# change NA values to 0
ccr[is.na(ccr[])] <- 0 

# Copeland wind potential
windp2 <- raster("WY_WindDevelopmentPotential_TNC_WYNDD_July2011_1km1.img")
windp2 <- projectRaster(windp2, bounds, crs=CRS_obj, method = 'ngb', res = 120)

# Copeland residential potential 
resp <- raster("WYResidentialDevelopmentProbability_TNC20111.img")
resp <- projectRaster(resp, bounds, crs=CRS_obj, method = 'ngb', res = 120)

# load roads
# USGS database
roads <- raster('WY_Roads_2009_Clip_PolylineT1.tif')
roads <- projectRaster(roads, bounds, crs=CRS_obj, method = 'ngb', res = 120)
# remove trails, local / residential roads
roads[roads==6] <- NA
roads[roads==7] <- NA
roads[roads==8] <- NA
roads[roads==9] <- NA
roads[roads!=0] <- 1

# wind turbines
turbines <- readOGR(dsn = ".", "Wind_Turbines_Clipped")
t_raster <- raster(ncol=1513, nrow=1503,
                   ext = ext_obj)
turbines <- project(turbines@coords, proj = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
turbines <- rasterize(turbines, t_raster, 1)
crs(turbines) <- CRS_obj
turbines <- projectRaster(turbines, bounds, crs=CRS_obj, method = 'ngb', res = 120)
turbines <- buffer(turbines,300)
rm(t_raster)

# Stack Conservation Feature Layers ----
# aggregate stacks to lower resolution
# created 5 groups of conservation features: nesting, brood, winter, annual, and multi-species (multisp)

nesting1 <- aggregate(stack(nesting_ddct, nesting_expert, kde_lek,
                            nest_obs),
                      fact = 13.41,
                      expand = FALSE)

brood1 <- aggregate(stack(brood_ddct, brood_expert, brood_riparian_expert),
                    fact = 13.41,
                    expand = FALSE)

winter1 <- aggregate(stack(winter_ddct, winter_expert, winter_obs),
                     fact = 13.41,
                     expand = FALSE)

annual1 <- aggregate(stack(nesting_ddct, nesting_expert, 
                           winter_ddct, winter_expert,
                           brood_ddct, brood_expert, brood_riparian_expert,
                           LC_rescale,
                           kde_lek, winter_obs, nest_obs),
                     fact = 13.41,
                     expand = FALSE)

multisp <-  aggregate(stack(nesting_ddct, nesting_expert, 
                            winter_ddct, winter_expert,
                            brood_ddct, brood_expert, brood_riparian_expert,
                            LC_rescale, kde_lek, winter_obs, nest_obs, Elk, MD),
                      fact = 13.41,
                      expand = FALSE)

# ensure featue values are within a runnable range
# by clamping the lower bounds 
nesting1 <- raster::clamp(nesting1, lower = 0, useValues = TRUE)
brood1 <- raster::clamp(brood1, lower = 0, useValues = TRUE)
winter1 <- raster::clamp(winter1, lower = 0, useValues = TRUE)
annual1 <- raster::clamp(annual1, lower = 0, useValues = TRUE)
multisp <- raster::clamp(multisp, lower = 0, useValues = TRUE)


# Create Cost Layers ----

# cost1 layer
# all cells set to a value of 1
cost1 <- brood
cost1[cost1] <- 1
names(cost1) <- "Cost_1"

# cost 2 layer: dev
cost2 <- sum(og,ccr,windp2,resp,na.rm = TRUE)
# add bck to cost2
cost2 <- cost2 + bckg
# change the bckg values to NA
cost2[cost2>=100] <- NA

# cost 3 layer: threat
cost3 <- raster.invert(cost2)
cost3 <- rescale0to1(cost3)

# rescale cost features
cost1_agg <- aggregate(cost1,fact=13.41,expand = FALSE)
cost2_agg <- aggregate(cost2,fact=13.41,expand = FALSE)
cost3_agg <- aggregate(cost3,fact=13.41,expand = FALSE)

# Locked out raster ----

# create locked_out rasters to enforce constraints
# use NLCD layer to exclude:
# urban, agricultural areas, and forested areas 
locked_out_1 <- NLCD
locked_out_1[locked_out_1==81] <- 1 ## ag 1: pasture/hay
locked_out_1[locked_out_1==82] <- 1 ## ag 2: crops
locked_out_1[locked_out_1==21] <- 1 ## developed: open
locked_out_1[locked_out_1==22] <- 1 ## developed: low intensity
locked_out_1[locked_out_1==23] <- 1 ## developed: medium intensity
locked_out_1[locked_out_1==24] <- 1 ## developed: high intensity
locked_out_1[locked_out_1==41] <- 1 ## forest 1: deciduous
locked_out_1[locked_out_1==42] <- 1 ## forest 2: evergreen
locked_out_1[locked_out_1==43] <- 1 ## forest 3: mixed
locked_out_1[locked_out_1==11] <- 1 ## water
locked_out_1[locked_out_1!=1] <- 0
# add padscars_buff, roads, topo, turbines, and bckg
locked_out_1 <-  sum(locked_out_1,scar_buff, na.rm = TRUE)
locked_out_1 <-  sum(locked_out_1,roads, na.rm = TRUE)
locked_out_1 <-  sum(locked_out_1,turbines, na.rm = TRUE)
locked_out_1[locked_out_1!=0] <- 1
# change the bckg to NA
locked_out_1 <-  locked_out_1 + bckg

# locked_out raster 2
# limits areas to only those outside of PACs
# as well as those in locked out raster 1
locked_out_2 <- locked_out_1
locked_out_2[is.na(locked_out_2[])] <- 0
locked_out_2 <- locked_out_2 + PACs
locked_out_2[locked_out_2>0] <- 1
locked_out_2 <-  locked_out_2 + bckg

# public land raster
pub <- mang_type
pub[pub==1] <- 0 
pub[pub==2] <- 0
pub[pub!=0] <- 1 
is.na(pub) <- 0 
pub <-  pub + bckg

# private land raster
pri <- mang_type
pri[pri==3] <- 0 
pri[pri!=0] <- 1 
is.na(pri) <- 0 
pri <-  pri + bckg

# locked_out 3 limits areas to being public land only
locked_out_3 <- sum(pub, locked_out_1, na.rm = TRUE)
locked_out_3[locked_out_3!=0] <- 1
locked_out_3 <-  locked_out_3 + bckg

# locked out 4 limits areas to outside of PACs
# and BLM/USBR land only
locked_out_4 <- sum(pub, locked_out_2, na.rm = TRUE)
locked_out_4[locked_out_4!=0] <- 1
locked_out_4 <-  locked_out_4 + bckg

# rescale locked out layers
locked_out_1_agg  <- aggregate(locked_out_1,fact=13.41,
                               expand = FALSE)

locked_out_1_agg[locked_out_1_agg<0.35] <- 0
locked_out_1_agg[locked_out_1_agg!=0] <- 1

locked_out_2_agg  <- aggregate(locked_out_2,fact=13.41,
                               expand = FALSE)

locked_out_2_agg[locked_out_2_agg<0.35] <- 0
locked_out_2_agg[locked_out_2_agg!=0] <- 1

# Restoration ----
# Time to recovery / cost feature
recov <- raster('timetorecov_clip1.tif')
recov <- projectRaster(recov, bounds, method = 'bilinear', res = 120)
recov <- rescale0to1(recov) + bckg
names(recov) <- "recovery_time"
recov_agg <- aggregate(recov, fact = 13.41, expand = FALSE)

# create a locked out feature for areas where habitat cant be selected
locked_out_7 <- sum(nesting_ddct,winter_ddct,brood_ddct)
locked_out_7[locked_out_7<=0.5] <- NA
locked_out_7[locked_out_7!=0] <- 1
locked_out_7_agg <- aggregate(locked_out_7, fact = 13.41, expand = FALSE)
