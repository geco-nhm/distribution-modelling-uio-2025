# BIOS 5211/9211
# Olav Skarpaas, NHM, University of Oslo, Nov 2021
# With minor updates by OS and Ingrid Vesterdal Tjessem Nov 2025

# Lab3. Distribution modelling with presence-absence data
#########################################################

# In this lab we will improve the modelling process illustrated briefly in Lab1,
# and detailed in the lecture by Rune Halvorsen earlier today. We will work with
# spatial data prepared as in the script Dataprep.R, but with lower resolution.
# See Dataprep.R for documentation of variables, e.g. land use classes.

# Load libraries
library(terra)
library(fields)    # for color ramps (e.g. tim.colors)
library(corrplot)  # useful correlation plot (reference: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html)

# Data are available in the 'lab_data' folder
# ===========================================
# The zipfile 'RASTER_2km_10km.zip' contains two folders with raster data:
# 2km: predictor rasters with 2km-resolution
# 10km: predictor rasters with 10km-resolution
# Unzip and put the two folder in 'lab_data/RASTER'


# Exercise 1: A model for Sitka spruce in Norway
# ==============================================

# Ecological model
# ----------------
# This first, important part of the modelling process is skipped here, but see
# introductory lecture day 1 (Olav Skarpaas) and Halvorsen 2012, Sommmerfeltia 34,
# and keep it in mind. Think of the purpose of modelling (ERM or SPM)
# and the relevance/importance of predictor variables.


# Data model: data structuring
# ----------------------------

path <- "lab_data/RASTER/2km/" # Change this to 10km if you have problems with large files

# Load predictor raster stack (maps of predictors for prediction and plotting)
r.list <- list.files(path, pattern="\\.tif$", full.names=TRUE) # Add all raster files in a given folder to a list
# Error? Did you remember to follow the instructions given in line number 17-22? 
predictors <- rast(r.list)                                    # Read and stack raster layers
names(predictors)                                             # Names of raster layers (maps of predictor variables)
names(predictors) <- gsub("_2km","",names(predictors))        # Rename: get rid of "_2km" (or "_10km"), to match variable names in training data
names(predictors)                                             # Check renaming
predictors$ar50_artype <- as.factor(predictors$ar50_artype)   # Define ar50_artype as factor, to match type in training data
predictors$geo_norge123 <- as.factor(predictors$geo_norge123) # Define geo_norge123 as factor, to match type in training data
plot(predictors)                                            # Plots of raster layers
# See the script Dataprep.R for more info on each layer

# Load, inspect and clean training data (for regression analysis)
load("lab_data/Norway_sitka_training_data_2km")  # Change this to 10km if you have problems with large files
names(training_data)
summary(training_data)                                         # Note missing values
training_data <- training_data[complete.cases(training_data),] # Remove missing values
any(is.na(training_data))                                      # Check: no missing values left

# Plot map of training data, with elevation as background
par(mfrow=c(1,1))
plot(predictors$dem100,main="Presence/absence points on elevation map",col=terrain.colors(64))
points(training_data$x,training_data$y,col=c("blue","red")[training_data$presence+1]) # Presences and random absences on elevation map

# Biplots of all variables in training data set
plot(training_data)

# Continuous variables: check correlations
# Correlation plot for training data set, removing response (presence) and factor variables
str(training_data)
corrplot.mixed(cor(training_data[,-c(3,14,15)],use="complete.obs"),lower="number",upper="ellipse",outline=TRUE,tl.pos="lt",tl.cex=0.90,tl.col = "black",cl.cex = 0.90)

# Which predictors will you include/exclude, considering your
# ecological understanding of the species, the relationship between predictors
# and the quality of the data (ecological model + data model)?

# Categorical variables: check combinations
table(training_data$ar50_artype, training_data$geo_norge123)

# Zero observations for (combinations of) levels may cause trouble for
# model fitting and prediction (e.g. in the 'predict' function for rasters).
# Consider simplifying levels and/or using only one of the factor variables.

# For land cover (ar50) - how does resolution (e.g. 10km vs. 100m) affect
# classification of pixels with presences?


# Statistical model
# -----------------
# Logistic regression with GLM: A presence-absence DM for 
# Sitka spruce in Norway.

# a) Build a preliminary "full" regression model containing
# all the predictors you selected above, using the 'glm' function
# (take a look at Lab1, or type '?glm' in the console, if you don't remember how to specify the model)
m <- glm(presence~bioclim_1+swe_5+Topographic_Wetness_Index+Total_insolation+ar50_artype+geo_norge123,family=binomial,data=training_data)


# b) Non-spatial model validation: Inspect model coefficients, tests of terms,
# residual deviance vs. null deviance. Is it a good model?
summary(m)


# c) Spatial model validation: Make predictions and plot prediction map
# Realistic predictions? Compare to species observations at gbif.org or artsdatabanken.no

p <- predict(predictors,m,type="response")  # Make predictions. Error?
# This gives an error because of levels of factors in predictors that are not present in training data,
# in the case in the variable ar50_artype. There are (at least) three solutions to this problem:
# 1) remove the variable from the model
# 2) match levels in predictors to training data before fitting the model - usually the best solution
# 3) match levels in predictors to fitted model before making predictions, e.g. like this:
m.levels <- m$xlevels$ar50_artype                                 # levels of artype in fitted model
p.levels <- as.character(levels(predictors$ar50_artype)[[1]]$ID)  # levels of artype in predictors
m.levels
p.levels
levels(predictors$ar50_artype)[[1]]$ar50_artype <- m.levels[match(p.levels,m.levels)] # selecting the artype levels in predictors that are present in the model
levels(predictors$ar50_artype)                                    # corrected levels in predictors matching levels in model (i.e, missing levels in model are set to NA)
# Then you should be able to run the predict command above without errors
# Note that the implication of this procedure is missing predictions where levels have been set to NA

# Plot predictions
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
plot(p,col=tim.colors())


# d) Model selection: Do stepwise backward model selection with AIC, using the 'step' function
# Which terms are removed from the full model by this procedure? Why?
ms <- step(m)
summary(ms)
summary(m)
AIC(m,ms)


# e) Plot and compare prediction maps for full model and selected model
par(mfrow=c(1,2))
plot(p,col=tim.colors(),main="Full model")
#points(training_data$x,training_data$y,col=c("black","grey")[training_data$presence+1]) # Presences and random absences on elevation map
ps <- predict(predictors,ms,type="response")
plot(ps,col=tim.colors(),main="Selected model")
#points(training_data$x,training_data$y,col=c("black","grey")[training_data$presence+1]) # Presences and random absences on elevation map


# f) Look at plots and summaries of the final model and compare to global model (Lab1)
# Which predictors are important for this species?
# What is its response to environmental variables according to the national and global models?
# How / why do they differ?
summary(ms)
m2 <- update(ms,~.+I(bioclim_1^2))  # Trying squared term for annual mean temperature
summary(m2)
AIC(ms,m2)                          # Marginal difference, keeping ms (linear temp response)


# Model evaluation with TRAINING data
#------------------------------------

# g) Evaluate the selected model with the 'evaluate' function
# and ROC plot for the full training data in package 'dismo'.
# Is this a good model?
library(dismo)
tr_pres <- training_data[training_data$presence==1,]
tr_abs <- training_data[training_data$presence==0,]
val <- evaluate(p=tr_pres,a=tr_abs,model=ms)
val
plot(val,"ROC")
density(val)

# Compare selected model to full model
val2 <- evaluate(p=tr_pres,a=tr_abs,model=m)
par(mfrow=c(1,2))
plot(val,"ROC")
plot(val2,"ROC")


# Model evaluation with data partitioning
#----------------------------------------

# h) Set aside random subset of training_data (20%) for evaluation
n <- nrow(training_data)
i <- sample.int(n,size=floor(n/5))
evaluation_data <- training_data[i,]

# i) Refit the selected model WITHOUT THE EVALUATION DATA set aside in exercise 1h.
ms2 <- update(ms,data=training_data[-i,])
summary(ms2)

# j) Evaluate the refitted selected model from 3b with 'evaluate' function
# and ROC plot in package 'dismo'. Is the fit as good as for the training data (exercise 1g)?
ev_pres <- evaluation_data[evaluation_data$presence==1,]
ev_abs <- evaluation_data[evaluation_data$presence==0,]
ev <- evaluate(p=ev_pres,a=ev_abs,model=ms2)
ev
plot(ev,"ROC")
density(ev)

# k) Repeat steps h-j. Do you get the same result? Why / why not?



# Exercise 2: Repeat Exercise 1 with other species (except 1f)
#=================================================

# Find and load training data for other species in the folder "lab_data"

# Consider the following questions:
#  - which predictor variables are important?
#  - signs of overfitting or lack of explanatory/predictive power?
#  - are the final models plausible? why, why not?
#  - do the final models make good predictions? why, why not?

# Syringa jokasiea
# Training data: "lab_data/Norway_lilac_training_data_2km"
load("lab_data/Norway_lilac_training_data_2km")
training_data <- training_data[complete.cases(training_data),] # Remove missing values
any(is.na(training_data))                                      # Check: no missing values left
corrplot.mixed(cor(training_data[,-c(3,14,15)],use="complete.obs"),lower="number",upper="ellipse",outline=TRUE,tl.pos="lt",tl.cex=0.90,tl.col = "black",cl.cex = 0.90)
table(training_data$ar50_artype, training_data$geo_norge123)
m <- glm(presence~bioclim_12+Growing_season_length+swe_5+Topographic_Wetness_Index+Total_insolation+ar50_artype+geo_norge123,family=binomial,data=training_data)
summary(m)
ms <- step(m)
summary(ms)
par(mfrow=c(1,2)) # prediction maps
# The model can only make predictions for artype levels present in training_data, we need to match levels in predictors and training data:
m.levels <- m$xlevels$ar50_artype                                 # levels of artype in fitted model
p.levels <- as.character(levels(predictors$ar50_artype)[[1]]$ID)  # levels of artype in predictors
m.levels
p.levels
levels(predictors$ar50_artype)[[1]]$ar50_artype <- m.levels[match(p.levels,m.levels)] # selecting the artype levels in predictors that are present in the model
levels(predictors$ar50_artype)                                    # corrected levels in predictors matching levels in model (i.e, missing levels in model are set to NA)
p <- predict(predictors,m,type="response")
plot(p,col=tim.colors(),main="Full model")
points(training_data$x,training_data$y,col=c("blue","red")[training_data$presence+1]) # Presences and random absences on elevation map
ps <- predict(predictors,ms,type="response")
plot(ps,col=tim.colors(),main="Selected model")
points(training_data$x,training_data$y,col=c("blue","red")[training_data$presence+1]) # Presences and random absences on elevation map
par(mfrow=c(2,2)) # evaluation plots
tr_pres <- training_data[training_data$presence==1,]
tr_abs <- training_data[training_data$presence==0,]
val <- evaluate(p=tr_pres,a=tr_abs,model=ms)
val
plot(val,"ROC")
density(val)
n <- nrow(training_data) # Total number of rows in the training data
i <- sample.int(n,size=floor(n/5)) # Randomly select 1/5 of the data for evaluation
evaluation_data <- training_data[i,] # Create evaluation data from the sampled indices
ms2 <- update(ms,data=training_data[-i,]) # Update the model with the remaining data
summary(ms2)
ev_pres <- evaluation_data[evaluation_data$presence==1,]
ev_abs <- evaluation_data[evaluation_data$presence==0,]
ev <- evaluate(p=ev_pres,a=ev_abs,model=ms2) # Evaluate the model based on presence and absence data
ev
plot(ev,"ROC")
density(ev) # Plot density of evaluation results


# Sus scrofa
# Training data: "lab_data/Norway_boar_training_data_2km"
load("lab_data/Norway_boar_training_data_2km")
training_data <- training_data[complete.cases(training_data),] # Remove missing values
any(is.na(training_data))                                      # Check: no missing values left
corrplot.mixed(cor(training_data[,-c(3,14,15)],use="complete.obs"),lower="number",upper="ellipse",outline=TRUE,tl.pos="lt",tl.cex=0.90,tl.col = "black",cl.cex = 0.90)
table(training_data$ar50_artype, training_data$geo_norge123)
m <- glm(presence~bioclim_1+bioclim_12+swe_5+Topographic_Wetness_Index+Total_insolation+ar50_artype,family=binomial,data=training_data)
summary(m)
ms <- step(m)
summary(ms)
par(mfrow=c(1,2)) # prediction maps
# The model can only make predictions for artype levels present in training_data, we need to match levels in predictors and training data:
m.levels <- m$xlevels$ar50_artype                                 # levels of artype in fitted model
p.levels <- as.character(levels(predictors$ar50_artype)[[1]]$ID)  # levels of artype in predictors
m.levels
p.levels
levels(predictors$ar50_artype)[[1]]$ar50_artype <- m.levels[match(p.levels,m.levels)] # selecting the artype levels present in the model
levels(predictors$ar50_artype)                                    # corrected levels in predictors matching levels in model (i.e, missing levels in model are set to NA)
p <- predict(predictors,m,type="response")
plot(p,col=tim.colors(),main="Full model")
points(training_data$x,training_data$y,col=c("blue","red")[training_data$presence+1]) # Presences and random absences on elevation map
ps <- predict(predictors,ms,type="response")
plot(ps,col=tim.colors(),main="Selected model")
points(training_data$x,training_data$y,col=c("blue","red")[training_data$presence+1]) # Presences and random absences on elevation map
par(mfrow=c(2,2)) # evaluation plots
tr_pres <- training_data[training_data$presence==1,]
tr_abs <- training_data[training_data$presence==0,]
val <- evaluate(p=tr_pres,a=tr_abs,model=ms)
val
plot(val,"ROC")
density(val)
n <- nrow(training_data)
i <- sample.int(n,size=floor(n/5))
evaluation_data <- training_data[i,]
ms2 <- update(ms,data=training_data[-i,])
summary(ms2)
ev_pres <- evaluation_data[evaluation_data$presence==1,]
ev_abs <- evaluation_data[evaluation_data$presence==0,]
ev <- evaluate(p=ev_pres,a=ev_abs,model=ms2)
ev
plot(ev,"ROC")
density(ev)




# Exercise 3: Independent study - do if time allows
#==============================

# a) If you have your own presence-absence data, apply the methods in this lab to your data.

# b) Download gbif data (see Lab2) for Norway or the World for a species you are interested in,
#    generate random absences (see Lab2), and develop a model with glm as in exercise 1.

# c) Take a look at tools for model validation in the sdm package.

# d) Prepare for tomorrow's lab by exploring the MIAmaxent package.

