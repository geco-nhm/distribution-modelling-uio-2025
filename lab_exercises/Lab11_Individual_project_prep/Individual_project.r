# BIOS 5211/9211
# Olav Skarpaas, NHM, University of Oslo, Nov 2021,
# with adjustments and replacements of species data Nov 2023


# Individual student project template
#####################################

# Read the instructions and write your code with annotations in this template.
# (Transferring the script to R markdown is ok, if you prefer.)
# In addition to this file, you will use code and data from previous labs, as well as
# some new environmental data, as described below.

# Please check that you have the latest version of the course material available on GitHub.

# You will work with ONE of these species:
# 1. Phellinus pini - a fungus
# 2. Letharia vulpina - a lichen
# 3. Orthotomicus longicollis - a beetle
# 4. Perisoreus infaustus - a bird


# Load libraries
#---------------
# NB! the libraries listed here are the libraries you need for data preparation
# - you may need to load additional libraries necessary for your choice of methods.
#remotes::install_github("rspatial/geodata")  # uncomment to install development version from GitHub (to avoid issues with CRAN version)
library(geodata)
library(terra)
library(raster)
library(sp)
library(rgbif)


# Source script with functions for data preparation
#--------------------------------------------------
source('lab_exercises/Lab10_RTPD/Dataprep_functions.r') # modify path and/or working directory if needed


# Environmental data
#-------------------

# THE WORLD
# We will use 'bioclim', i.e. Biologically relevant climate variables: http://www.worldclim.org/bioclim.
# The following code takes a subset of these variables that are also available in the data set for
# Norway (bioclim 1, 10 and 12; see below). Feel free to get additional data if you like, or modify
# the code to increase the resolution etc., but it is also perfectly fine to use the code as it is.
# Load data for current climate (predictors) and for projected future climate (scenario SSP5-8.5,
# year 2061-2080) in the same resolution (set by the argument res):
predictors.global <- worldclim_global(var='bio',res=10)[[c(1,10,12)]]  # Current climate
scenarios.global <- cmip6_world(var='bio',res=10,ssp=585,model="EC-Earth3-Veg",time="2061-2080")[[c(1,10,12)]]   # Climate scenarios
names(predictors.global) <- c("bioclim1","bioclim10","bioclim12") # renaming to simplify and match variable names
names(scenarios.global) <- c("bioclim1","bioclim10","bioclim12") # renaming to simplify and match variable names
# Layers in both of these datasets (current climate and scenarios):
#  bioclim1                         Annual mean temperature (Bioclim, degrees Celsius*10)
#  bioclim10                        Mean Temperature of Warmest Quarter (Bioclim, degrees Celsius*10)
#  bioclim12                        Annual Precipitation (Bioclim, mm)


# NORWAY
# The environmental data set for Norway is a scaled subset of
# the data compiled and described in Horvath et al. 2019. Distribution 
# modelling of vegetation types based on area frame survey
# data. Applied Vegetation Science, 22(4): 547-560 (see syllabus).
# Load environmental predictor rasters for Norway, 2 km resolution (modify path if needed):
r.list <- list.files("lab_data/Individual_project_rasters_2km/", pattern="tif$", full.names=TRUE) # modify path and/or working directory if needed
predictors.Norway <- rast(r.list)
# Layers with predictor variables in this raster dataset (expanded from previous labs):
# Topography & geology ------------------
#  dem100                            Digital elevation model (Kartverket)
#  Aspect                            Aspect (Derived from DEM, direction in radians)
#  TopographicWetnessIndex           Topographic Wetness Index (Derived from DEM, unitless index)
#  Totalinsolation                   Potential Incoming Solar Radiation (Derived from DEM)
#  geonorge123                       Bedrock nutrient status (three classes, derived from NGU bedrock types)
# Climatic variables --------------------
#  bioclim1                          Annual mean temperature (Bioclim, degrees Celsius*10)
#  bioclim10                         Mean Temperature of Warmest Quarter (Bioclim, degrees Celsius*10)
#  bioclim12                         Annual Precipitation (Bioclim, mm)
#  GrowingSeasonLength               Growing season length (MET - SeNorge 2, CDO algorithm - derived from temperature)
#  swe5                              Snow water equivalent in May (MET - SeNorge 1.1.1)
#  swe1                              Snow water equivalent in January (MET - SeNorge 1.1.1)
# Land cover ----------------------------
#  ar50artype                        Land cover classification, based on AR50 (NIBIO):
#        10	Developed areas: Residential area, town center, city, transport, industrial area and alike.
#        20	Agricultural areas: Cropland, cultivated soil and cultivated pastures
#        30	Forest
#        50	Barren land: Land areas with natural vegetation cover that is not forest.
#        60	Bog and Fen: Wetland areas with characteristics of marshes
#        70	Glacier: Ice and snow that does not melt during the summer.
#        81	Freshwater. Rivers and lakes
#        82	Ocean.
#        99	Not mapped.
# Forest structure ----------------------
# SR16 laser data, NIBIO
#  SRRBONITET                        Productivity class (height of tallest tree at age 40, in meters)
#  SRRKRONEDEK                       Crown cover (percent)
#  SRRTREALDER                       Stand age (years)
#  SRRTRESLAG                        Dominant tree species (1: spruce, 2: pine, 3: deciduous)
#  SRRVOLUMB                         Standing volume (cubic meters/ha)



# Training data
#--------------
# We prepare training data for distribution modelling by downloading species 
# occurrence data (presences) from GBIF, adding random pseudo-absence/background
# points (as in 'Lab3_DM_presence-absence/Dataprep.R', but with absences coded as NA),
# and extracting environmental variables for presence/absence points from the
# environmental rasters above.
# You can work with the code and data as they are, or, if you like, modify the script or write
# your own code to download additional occurrence data or build the training data differently.

# Download GBIF data for the World
key <- name_backbone(name= "Perisoreus infaustus")$speciesKey
sp.global <- occ_search(taxonKey=key, hasCoordinate=TRUE, limit=1000)

# Generate global training data
training.data.global <- generate.training.data(sp.global,predictors.global,generate.absences=TRUE,absence.value=NA,n.abs=1000)   # To see how the function 'generate.training.data' works, open the file 'Dataprep.functions.r'

# Download GBIF data for Norway
key <- name_backbone(name= "Perisoreus infaustus")$speciesKey
sp.Norway <- occ_search(taxonKey=key, hasCoordinate=TRUE, country="NO", limit=1000)

# Generate training data for Norway
training.data.Norway <- generate.training.data(sp.Norway,predictors.Norway,generate.absences=TRUE,absence.value=NA,n.abs=1000,factors=c("ar50artype","geonorge123"))   # To see how the function 'generate.training.data' works, open the file 'Dataprep.functions.r'



# Independent work
#-----------------

# Fill inn the script with your own code (reuse code from labs!) to address
# the project tasks (see also lecture last day):

# - Use an appropriate distribution modelling technique to estimate
#   responses to climate variables and predict the potential
#   distribution of the species across the globe, now and with future climate change.

# - Make a model (or several models) for the species based on environmental predictors
#   and occurrences in Norway and assess how observed and predicted occurrence
#   of the species varies with respect to climate, land cover and other environmental conditions
#   in Norway (to the extent that these are represented by environmental variables in the data set).

# - How good are the models for the global and national ranges, and how do they compare in terms of
#   modelled ecological responses and predictions?

# - Based on results from the modelling above, discuss
#     How is the distribution affected by climate?
#     How is the distribution affected by land use and forest structure?
#     How might the distribution of the species respond to future changes in
#     - global climate?
#     - dominant tree species?
#     - land use in Norway, forestry in particular?

# - Based on existing knowledge of the species and your results in this study, discuss
#   limitations of the models and (briefly) how further research on the species may be aided by
#   other modelling approaches and/or other kinds of studies.

# Annotate the script with comments to explain what you are doing.

# When done, please rename this script 'Individual_project_[species name]_[your name].r'
# and upload the script in canvas along with your report. (Alternatively,
# if you prefer to work in R markdown, please provide the markdown file.)

# Thanks!

