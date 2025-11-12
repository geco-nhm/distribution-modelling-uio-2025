# BIOs 5211/9211
# Olav Skarpaas, with contributions from Peter Horvath, Dag Endresen, and
# Erik Kusch, NHM, University of Oslo, Nov 2021, 2023, and Nov 2025

#########################################################
# Data preparation for distribution modelling
#########################################################

# This script demonstrates data preparation for distribution modelling,
# condensing and reiterating some of the material covered in Lab2 GBIF data,
# and preparing for later work with a specific environmental data set.

# The environmental data set for Norway is a subset of
# the data compiled and described in Horvath et al. 2019. Distribution 
# modelling of vegetation types based on area frame survey
# data. Applied Vegetation Science, 22(4): 547-560 (see syllabus).

# We will first import and work with the spatial environmental data 
# layers, and then combine this with species occurrences to prepare for
# distribution modelling.

# This script works with 100m-resolution data; in most labs to follow, we will
# work with coarser versions of the same data (2km or 10km resolution).


## Libraries: spatial data tools ----
# package installation and loading in one step:
install.load.package <- function(x) { # custom function that loads the desired package and automatically installs any non-installed packages
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c( # vector of package names
  "sp",              # vector tools
  "terra",           # raster tools
  "rgbif"            # tools for GBIF data
)
sapply(package_vec, install.load.package) # applying install/load function to each name in package vector


### Environmental data loading and preparation
##############################################
# if you have NOT downloaded the spatial data yet, then do so, by following the instructions in the course github repository.
# Set file path for data - modify path to files on your computer if necessary.
path <- "lab_data/RASTER" 

# Load environmental predictor maps for Norway
# Coordinate Reference System of the raster files is EPSG:32633 - WGS 84 / UTM zone 33N
r.list <- list.files(path, pattern="\\.tif$", full.names=TRUE) # Add all raster files in a given folder to a list
predictors <- rast(r.list)                                     # read and stack raster layers
names(predictors)
# Layers in this raster dataset:
# Topography & geology ------------------
#  dem100                            Digital elevation model (Kartverket)
#  Aspect                            Aspect (Derived from DEM, direction in radians)
#  Topographic_Wetness_Index         Topographic Wetness Index (Derived from DEM, unitless index)
#  Total_insolation                  Potential Incoming Solar Radiation (Derived from DEM)
#  geo_norge123                      Bedrock nutrient status (three classes, derived from NGU bedrock types)
# Climatic variables --------------------
#  bioclim_1                         Annual mean temperature (Bioclim, degrees Celsius*10)
#  bioclim_10                        Mean Temperature of Warmest Quarter (Bioclim, degrees Celsius*10)
#  bioclim_12                        Annual Precipitation (Bioclim, m)
#  Growing_season_length             Growing season length (MET - SeNorge 2, CDO algorithm - derived from temperature)
#  swe_5                             Snow water equivalent in May (MET - SeNorge 1.1.1)
#  swe_1                             Snow water equivalent in January (MET - SeNorge 1.1.1)
# Land cover ----------------------------
#  ar50_artype                       Land cover classification, based on AR50 (NIBIO):
#        10	Developed areas: Residential area, town center, city, transport, industrial area and alike.
#        20	Agricultural areas: Cropland, cultivated soil and cultivated pastures
#        30	Forest
#        50	Barren land: Land areas with natural vegetation cover that is not forest.
#        60	Bog and Fen: Wetland areas with characteristics of marshes
#        70	Glacier: Ice and snow that does not melt during the summer.
#        81	Freshwater. Rivers and lakes
#        82	Ocean.
#        99	Not mapped.

# Convert raster layers with factor variables to factors
predictors[["geo_norge123"]] <- as.factor(predictors[["geo_norge123"]])
predictors[["ar50_artype"]] <- as.factor(predictors[["ar50_artype"]])

# Make background raster for Norway
norway <- predictors[[1]]/predictors[[1]]   # raster with values=1 in mainland Norway, based on elevation raster


### GBIF Data loading and preparation
#####################################
# The approach below gives quick access to data sets of limited size, suitable
# for our purpose in this course.
# However, for scientific publications you should use asynchronous downloading,
# as demonstrated in Lab 2 GBIF data. This allows downloading larger data sets, and citation of a download with a single doi.
# See also e.g. material by Erik Kusch: https://www.erikkusch.com/courses/gbif/

### Data Download ----
# retrieval of 1000 observation of our target species within Norway
sp <- occ_search(scientificName="Picea sitchensis",       # which species we want
                 hasCoordinate=TRUE,                      # whether data should be geo-referenced
                 country="NO",                            # what country the data should come from, these are ALPHA-2 ISO country codes: https://www.nationsonline.org/oneworld/country_code_list.htm
                 limit=1000                               # how many observations to retrieve at most
) 

### Data Handling ----
# Convert lat-long coordinates to coordinate system (crs) of environmental raster data
occ_points <- data.frame(lon=sp$data$decimalLongitude,lat=sp$data$decimalLatitude) # extract coordinates
occ_points <- vect(occ_points,crs="+proj=longlat +datum=WGS84")   # make spatial vector, CRS is WGS84 because that is how GBIF records the data
occ_UTM33 <- project(occ_points,crs(norway))
sp$data$x <- occ_UTM33$x
sp$data$y <- occ_UTM33$y

# Point data like 'sp' can be used directly as input to MIAmaxent (Labs 4-5),
# along with environmental rasters.
# For glm (Lab 3), we need absence data in addition to presences, and we
# need to combine presences and absences with environmental data in a
# data set suitable for the glm function. This can be done as follows.

# Rasterize occurrences
occ_ras <- rasterize(occ_UTM33,norway,fun="length",background=0)   # raster with counts of occurrences in each cell of norway
plot(occ_ras)
occ_ras <- mask(occ_ras,norway)                                    # filtering occurrence raster with mainland raster
occ_ras[occ_ras>0] <- 1                                            # reducing counts>0 to 1 (presence)
plot(occ_ras)
table(values(occ_ras))                                             # counts of absences (0) and presences (1)

# Take the occurrence cells as presences
presences <- which(values(occ_ras)==1)

# Then generate absence data by sampling from the cells without occurrence observations.
# NB! This rests on risky assumptions, but we need the absences
# for logistic regression with glm (Lab 3). We make this absence data set for
# educational purposes, but ideally one would want a data set with true, verified presences
# and absences for logistic regression (and also for validation of 'presence-only' methods,
# such as maxent).
absences <- which(values(occ_ras)==0)
absences_sample <- sample(absences,size=length(presences)) # sample of same number of absence cells as presence cells

### Extracting Environmental Data to match GBIF Data ----
# Combine presences, absences and environmental data
selected <- c(presences,absences_sample)
xy <- xyFromCell(occ_ras,cell=selected)
training_data <- data.frame(xy,presence=values(occ_ras)[selected],
                            extract(predictors,xy))
head(training_data)
tail(training_data)
plot(training_data[,c("x","y")],col=c("blue","red")[training_data$presence+1])

# Convert discrete environmental predictors to factor variables
training_data$ar50_artype <- factor(training_data$ar50_artype)
training_data$geo_norge123 <- factor(training_data$geo_norge123)

# Save data (you'll overwrite any existing files with these names if you uncomment and run these lines)
#save(training_data,file="lab_data/Norway_sitka_training_data")
#save(predictors,file="lab_data/Norway_predictor_raster_stack")


# Some additional material for further independent study on spatial/environmental data:
# Lab2_GBIF_data, e.g. environment.Rmd and mapping.Rmd
# Climate data handling with KrigR: https://www.erikkusch.com/courses/krigr/
# GIS with R: https://pakillo.github.io/GISwithR/
