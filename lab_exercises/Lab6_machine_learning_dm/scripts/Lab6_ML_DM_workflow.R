####################################################################
# BIOS 5211/9211
# Lasse T. Keetz, NHM, University of Oslo, Nov 2021
# Minor edits by Olav Skarpaas, Nov 2023, to standardize across labs
# and update code
# Minor edits by Adam E. Naas, Nov 2025
#
# Lab 6: Building a Machine Learning workflow
####################################################################

##########################
### 1. Library imports ###
##########################

package_names <- c("caret",        # ML tools
                   "ranger",       # Random Forest classifier
                   "randomForest", # Diff. RF classifier (needed for caret)
                   "terra",       # Spatial data, raster operations
                   "rstudioapi",   # Automatically set work dir.
                   "rgbif",        # GBIF R interface, species data
                   "MLmetrics",    # Machine learning metrics (AUC, etc.)
                   "RColorBrewer", # Color palettes
                   "ggplot2"       # Nicer plots
)

### Install and load packages
for(p_name in package_names) {
  if(p_name %in% rownames(installed.packages())){
    require(p_name, character.only = TRUE)
  }
  else {
    install.packages(p_name)
    require(p_name, character.only = TRUE)
  }
}

### Set working directory to current document's location
current_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_path)

### Set random seed, ensures that steps involving randomness
# will always give exactly the same outcome (--> reproducibility)
set.seed(112)

###################################################################
###################################################################

################################
####### 2. Prepare data ########
################################

############### Create stack of rasters for the features/predictors #################

# Path to raster (.tif) files
pred_path <- paste0(dirname(dirname(dirname(current_path))),
                    "/lab_data/nor_predictor_rasters/2km")

# Store all paths to files ending in ".tif" in a vector
raster_fnames <- list.files(path = pred_path, pattern = "\\.tif$",
                            ignore.case = TRUE, full.names = TRUE)

# Create a stack of all individual rasters
pred_stack <- rast(raster_fnames)
pred_stack

# Rename: get rid of "_2km" (or "_10km"), to match names in training data
names(pred_stack) <- gsub("_2km","",names(pred_stack))

# Plot DEM to test if everything works as expected
terra::plot(pred_stack[["dem100"]])


### Convert the value format to factor for the categorical features

# Create object with names for the categorical features
cat_var_names <- c("ar50_artype",  # Land cover type
                   "geo_norge123"  # Nutrient content in bedrock
)

# Loop over all features and convert to factor for the categorical features
for(cat_var in cat_var_names) {
  pred_stack[[cat_var]] <- as.factor(pred_stack[[cat_var]])
}

# Check if the code worked. It should be showing TRUE for categorical features and FALSE for continuous
for(ras_idx in 1:nlyr(pred_stack)) {
  print(paste0(names(pred_stack)[ras_idx],": ",
               is.factor(pred_stack[[ras_idx]])))
}

################## Load target (species) data #####################

### Pick your species here!
species_name = "Picea sitchensis"  # Group 1: Sitka spruce (invasive tree species)
species_name = "Silene acaulis" # Group 2: Moss campion (vascular plant with alpine distribution)

# Request the key used to ask for observations of your species from GBIF
key <- rgbif::name_backbone(name = species_name,
                            kingdom = "Plantae")$speciesKey

# Search for observations of the species
sp <- rgbif::occ_search(taxonKey = key,
                        hasCoordinate = TRUE, # Only request observations that include coordinates
                        country = "NO", # Search only within Norway
                        limit = 6000) # Limit the number of observations to 6000
# Print info
print(sp)


### Transform lat-long coordinates to UTM coordinate system of environmental raster data
occ_points <- data.frame(x = sp$data$decimalLongitude,
                         y = sp$data$decimalLatitude)

# Convert data.frame to SpatialPoints object
occ_points <- sp::SpatialPoints(occ_points,
                                proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

# Transform to UTM coordinates
occ_UTM33 <- sp::spTransform(occ_points,
                             sp::CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

# Replace columns with lat-long coordinates with columns with UTM coordinates 
sp$data$x <- occ_UTM33$x
sp$data$y <- occ_UTM33$y


### Make background raster for Norway to make nicer plots
norway_mask <- pred_stack[["dem100"]] / pred_stack[["dem100"]] # Division by itself creates a map where all values = 1
plot(norway_mask)

# raster with counts of occurrences in each cell of Norway
occ_ras <- terra::rasterize(x = vect(occ_UTM33),
                            y = norway_mask,
                            fun = 'count',
                            background = 0)

# Mask raster by setting values outside of Norway bounds to NA
occ_ras <- occ_ras * norway_mask

# Zoom in to visualize occurrences, random location
plot(occ_ras, xlim = c(-40000, -10000), ylim = c(6630000, 6650000)) # Values show the number of observations within each pixel


### Create training dataset (df)

# Presences
# Extract indices of raster matrix where species is observed
presence_indices <- which(values(occ_ras) >= 1)

# Absences
# Extract indices of raster matrix where species is not observed
absence_indices <- which(values(occ_ras) == 0)

# Create random sub-sample with the same amount of absence points as there are
# presence points (OBS! Think about potential problems with this approach!)
absences_sample_indices <- sample(absence_indices,
                                  size = length(presence_indices))

# Combine presences, absences and environmental data in a data.frame
training_indices <- c(presence_indices, absences_sample_indices)
training_xy <- terra::xyFromCell(occ_ras, training_indices)
training_data <- data.frame(training_xy, 
                            presence = values(occ_ras)[training_indices],
                            extract(pred_stack, training_xy))

# Change number of occurrences column to binary presence (1) / [pseudo]absence (0)
training_data$presence[training_data$presence >= 1] <- 1

# Check how many rows that have missing data
table(complete.cases(training_data))

# Remove rows that contain NA values in any of the columns. OBS! Also debatable! Other techniques (e.g., imputation) exist
training_data <- training_data[complete.cases(training_data), ]

# Set categorical features as factor variables (this time in the training dataset)
for(cat_var in cat_var_names) {
  training_data[[cat_var]] <- factor(training_data[[cat_var]])
}

# Check if the structure and values in the training dataset makes sense
str(training_data)
head(training_data) # Only first rows printed

# How many samples remained? Is the balance between presences and absences still okay?
print(paste0("Number of presences: ",
             length(which(training_data$presence==1))))
print(paste0("Number of absences: ",
             length(which(training_data$presence==0))))

# Plot map of Norway
terra::plot(norway_mask, 
            col = rgb(red = 0.9, green = 0.9, blue = 0.9, alpha=1))

# Create color palette based on presences and absences
colors <- c(rgb(red = 1, green = 0, blue = 0, alpha = 0.2), # Absences in red
            rgb(red = 0, green = 0, blue = 1, alpha = 0.2))[training_data$presence + 1] # Presences in blue

# Add presence and absence points to the map
points(training_data[,c("x","y")],
       col = colors, pch = 16, cex = 0.3)



################################
####### 3. Splitting data ######
################################


### 4-fold cross-validation
cv_indices <- caret::createFolds(
  y = training_data$presence,
  k = 4, # Number of folds
  returnTrain = TRUE # Return indices that corresponds to those in the training dataset
)


#################################################################
#################################################################

################################
####### 4. Model training ######
################################

# Here we will create our own functions for model training. Note that
# the following steps also can be performed using functions from the 
# caret package directly (-> see documentation for trainControl() and 
# train()). However, this comes at the cost of flexibility as you can 
# only tune a subset of the hyperparameters for each model that can be 
# trained using the function. For a list of all models that are available 
# in the function train(), see: https://topepo.github.io/caret/available-models.html

#################################################################
#################################################################

################################
####### Random forest ##########
################################

n_features <- nlyr(pred_stack) # Set the number of features
sqrt_n_featutres <- as.integer(sqrt(n_features)) # Choose the default value for the hyperparameter mtry (see below)

### Hyperparameter tuning ###
# Define tuning grid.
# Which hyperparameters should we test and which values of the hyperparameters should we try?
# There are also other hyperparameters that can affect performance, for example "the number of trees to grow"
tune_param_grid <- base::expand.grid(
  
  # Maximum depth of trees: Minimum value = 0 leading to no restriction on the tree depth. Maximum value = 1 leading to only 1 split per tree
  max.depth = seq(0, 10, by = 2), # Increasing max.depth -> fewer splits -> less algorithmic flexibility
  
  # Number of features randomly selected at each split. Increasing mtry -> more features considered for each solit -> decision trees more similar
  mtry = c(sqrt_n_featutres - 1, 
           sqrt_n_featutres,
           sqrt_n_featutres + 1)
  
  ### See "?ranger" for a full list of tunable parameters.
  # Think: Will the models performance on test data be optimized with a more complex or a simpler model? 
)

# Show the hyperparameter grid
tune_param_grid # Shows the combinations of hyperparameter values that will be tested

### Create a data frame to store the test results for the hyperparameter tuning
# Create vector with names for the folds
fold_names <- paste0("F", 1:4)

# Retrieve tuneable hyperparameter names
param_names <- colnames(tune_param_grid)

# Define metric names
metric_names <- c("AUC", "Accuracy", "Precision", "Recall")

# Determine number of total columns of the test result data frame
length_inner_df <- length(param_names) + length(metric_names)

# Create the data frame that will store the test results from hyperparameter tuning
hyperparam_df <- data.frame(
  matrix(ncol = length_inner_df, nrow = 0)
)
# Set the column names to the hyperparameter names and the metrics
colnames(hyperparam_df) <- c(param_names, metric_names)

# Create a list that will store the test results for each fold
rf_tuning_list <- sapply(fold_names, function(x) NULL)
# Each list entry will contain one data frame with test results
for(idx in 1:length(rf_tuning_list)) {
  rf_tuning_list[[idx]] <- hyperparam_df
}
rf_tuning_list


### Start training
# Hyperparameter searches and cross-validation requires that we train many models.
# Begin preparing a loop that will fit the models and test their performance
# Index keeping track of the current fold
fold_idx <- 1
# Index keeping track of the number of iterations
current_iter <- 1
# Determine total number of iterations (how often the loop "starts over")
# That number will be the number of folds times the number of combinations in our hyperparameter grid
total_iterations <- length(cv_indices) * nrow(tune_param_grid)
# Store the names of the features in a vector
feature_names <- names(pred_stack)

### Outer for-loop: run through each training sub-set (e.g., the cross-validation folds)
for(fold_indices in cv_indices) {
  
  ### Inner for-loop: run through list of hyperparam. combinations
  for(param_grid_idx in 1:nrow(tune_param_grid)) {
    
    # Use one fold to train the model. X = feature matrix (predictors), y = target variable (presence/absence)
    X_train <- training_data[fold_indices, feature_names]
    y_train <- training_data[fold_indices, "presence"]
    
    # Use the other folds to test the model
    X_test <- training_data[-fold_indices, feature_names]
    y_test <- training_data[-fold_indices, "presence"]
    
    ### Train random forest on current training set
    current_rf <- ranger::ranger(
      x = X_train,
      y = y_train,
      classification = TRUE, # Return predicted classes
      probability = TRUE, # Return predicted (relative) probabilities of presence betwen 0 and 1
      mtry = tune_param_grid[param_grid_idx, "mtry"],
      max.depth = tune_param_grid[param_grid_idx, "max.depth"],
      importance = "permutation" # Compute feature importance by permutation
    )
    
    ### Make predictions for each of thee test observations (the remaining folds)
    current_predictions <- predict(
      current_rf,
      X_test
    )
    
    ### Transform the probabilities to actual class predictions (0,1)
    # Create empty vector to store the class predictions
    class_predictions <- vector(
      mode = "numeric", 
      length = nrow(current_predictions$predictions)
    )
    
    # Assign class that is more likely (prob. > 0.5)
    cut_off_val <- 0.5
    for(idx in 1:nrow(current_predictions$predictions)) {
      if(current_predictions$predictions[idx, 1] < cut_off_val){
        class_predictions[idx] <- 0
      } else {
        class_predictions[idx] <- 1
      }
    }
    
    ### Generate confusion matrix, metrics like Accuracy, F1-score, ...
    cur_conf_mat <- caret::confusionMatrix(
      as.factor(class_predictions),
      as.factor(y_test)
    )
    
    ### Also calculate the AUC value
    cur_auc <- MLmetrics::AUC(
      y_true = as.factor(y_test),
      y_pred = current_predictions$predictions[, 1]
    )
    
    ### Add hyperparameter values to the test results data frame
    rf_tuning_list[[fold_idx]][param_grid_idx, "mtry"] <-
      tune_param_grid[param_grid_idx, "mtry"]
    
    rf_tuning_list[[fold_idx]][param_grid_idx, "max.depth"] <-
      tune_param_grid[param_grid_idx, "max.depth"]
    
    ### Add classification results to the test results data frame
    rf_tuning_list[[fold_idx]][param_grid_idx, "AUC"] <-
      cur_auc
    rf_tuning_list[[fold_idx]][param_grid_idx, "Accuracy"] <-
      cur_conf_mat$overall["Accuracy"]
    rf_tuning_list[[fold_idx]][param_grid_idx, "Precision"] <-
      cur_conf_mat$byClass["Precision"]
    rf_tuning_list[[fold_idx]][param_grid_idx, "Recall"] <-
      cur_conf_mat$byClass["Recall"]
    
    # Print the progress
    print(paste0("Current iter: ",current_iter," (out of ", total_iterations,")"))
    current_iter <- current_iter + 1
    
  }
  
  # Print the progress
  print(paste0("Done with fold: ", fold_idx))
  fold_idx <- fold_idx + 1
  
}

# Print the test results for all the folds
rf_tuning_list

### Calculate mean performance over all folds ###
mean_results_df <- rf_tuning_list[[1]]
for(idx in 2:length(rf_tuning_list)) {
  mean_results_df <- mean_results_df + rf_tuning_list[[idx]]
}
mean_results_df <- mean_results_df / length(rf_tuning_list)

### Transform to matrix
mean_results_mat <- matrix(
  nrow = nrow(unique(tune_param_grid["mtry"])),
  ncol = nrow(unique(tune_param_grid["max.depth"]))
)

# Unique hyperparam values from tuning grid
unique_mtry <- unique(tune_param_grid["mtry"])
unique_maxd <- unique(tune_param_grid["max.depth"])

# Name columns and rows of the matrix (with hyperparam values)
rownames(mean_results_mat) <- sapply(unique_mtry, as.character)
colnames(mean_results_mat) <- sapply(unique_maxd, as.character)

### Retrieve AUC values for each hyperparameter
### combination in averaged results data frame
for(i in 1:nrow(mean_results_mat)) {
  for(j in 1:ncol(mean_results_mat)) {
    df_row_idx <- intersect(
      which(mean_results_df["mtry"] == rownames(mean_results_mat)[i]),
      which(mean_results_df["max.depth"] == colnames(mean_results_mat)[j])
    )
    mean_results_mat[i, j] <- mean_results_df[df_row_idx, "AUC"]
  }
}

### Plot results ###
plot_heatmap <- function() {
  dev.new()
  # Create heatmap, darker red colors -> higher AUC values
  heatmap(mean_results_mat,
          main = "Random forest - hyperparameter tuning", 
          xlab = "max.depth",
          ylab = "mtry",
          cexRow = 1.5, 
          cexCol = 1.5,
          col = colorRampPalette(brewer.pal(8, "Oranges"))(10),
          Rowv = NA, 
          Colv = NA)
  legend(x = "topright", 
         title = "AUC",
         legend=c(
           paste0("min - ", round(min(mean_results_mat),3)),
           paste0("mean - ", round(mean(mean_results_mat),3)),
           paste0("max - ", round(max(mean_results_mat),3))),
         fill = colorRampPalette(brewer.pal(8, "Oranges"))(3)
  )
}

# Call function
plot_heatmap()

# Look at the boxplot.
# Are all hyperparameters equally important for model performance?
# Which hyperparameter values are reasonable to choose and which ones are not?

#####
# Pick model with best hyperparameters (on average) and plot performance across folds
#####

# Indices pointing to maximum AUC in mean results mat
best_idx <- which(mean_results_mat == max(mean_results_mat),
                  arr.ind = T)

# Retrieve corresponding mtry and max.depth values
best_mtry <- as.integer(rownames(mean_results_mat)[best_idx[1]])
best_maxd <- as.integer(colnames(mean_results_mat)[best_idx[2]])

### Create matrix to store metrics for each training set
fold_res_mat <- matrix(nrow = length(rf_tuning_list),
                       ncol = length(metric_names))
colnames(fold_res_mat) <- metric_names # Rename columns

### Fill matrix with metric values from best hyperparameters
for(idx in 1:length(rf_tuning_list)){
  cur_row <- intersect(
    which(rf_tuning_list[[idx]]$mtry == best_mtry),
    which(rf_tuning_list[[idx]]$max.depth == best_maxd)
  )
  for(metric in metric_names){
    fold_res_mat[idx, metric] <- 
      rf_tuning_list[[idx]][cur_row, metric]
  }
}

### Plot results as a boxplot
plot_boxplot <- function() {
  dev.new()
  boxplot(fold_res_mat, 
          main = "Best RF model - 4-fold-cv performance",
          col = "#E6E6FA")
}

plot_boxplot()

# Look at the boxplot: What does AUC, accuracy, precision and recall mean?
# Which metric is most important?

################################
####### 5. Feature importance ##
################################

### Retrain RF models (one for each fold) with "best" hyperparameters

# Create a list to store models
rf_list <- list()
idx <- 1
for(fold_indices in cv_indices) {
  
  #Separate into training and test folds
  # X -> feature matrix (predictors), y -> target variable (presence/absence)
  X_train <- training_data[fold_indices, feature_names]
  y_train <- training_data[fold_indices, "presence"]
  
  X_test <- training_data[-fold_indices, feature_names]
  y_test <- training_data[-fold_indices, "presence"]
  
  ### Train random forest on current training set
  current_rf <- ranger::ranger(
    x = X_train,
    y = y_train,
    classification = TRUE,
    probability = TRUE,
    mtry = best_mtry,
    max.depth = best_maxd,
    importance = "permutation"
  )
  
  ### Make predictions
  current_predictions <- predict(
    current_rf, 
    X_test
  )
  
  # Store the models in a list
  rf_list[[idx]] <- current_rf
  idx <- idx + 1 # Go on to the next index before the next iteration
}

### Plot feature importance ###
# Note that the permutation importance values contained in the
# ranger RF objects are calculated based on out-of-bag (OOB)
# error.

# Creating a matrix to store feature importance results
importance_mat <- matrix(nrow = length(feature_names),
                         ncol = length(rf_list) + 2)
colnames(importance_mat) <- c(fold_names, "mean_importance", "sd")
rownames(importance_mat) <- feature_names

# Insert feature importance values (computed for each fold)
for(idx in 1:length(rf_list)) {
  importance_mat[,idx] <- rf_list[[idx]]$variable.importance
}

### Calculate the mean and SD feature importance across folds
for(idx in 1:nrow(importance_mat)){
  importance_mat[idx, "mean_importance"] <-
    mean(importance_mat[idx, 1:length(rf_list)])
  importance_mat[idx, "sd"] <-
    sd(importance_mat[idx, 1:length(rf_list)])
}

# Convert to data frame for ggplot
importance_df <- as.data.frame(importance_mat)

# Plot ranked mean importances
ggplot(data = importance_df, 
       aes(x = reorder(feature_names, mean_importance), 
           y = mean_importance,
           fill = mean_importance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(data = importance_df,
                aes(ymin = mean_importance-sd,
                    ymax = mean_importance+sd),
                width = .2,
                position = position_dodge(.9)) +
  coord_flip() +
  ylab("Feature Importance (Permutation)") +
  xlab("") +
  guides(fill = "none") +
  scale_fill_gradient(low = "#d0efff", high = "#1167b1")

# Which features are most important? Does it make sense for the species you selected? 
# What does this plot not tell you about the relationship between the target and the features?


################################
####### 6. Feature selection ###
################################

# You can ignore this part, but you need to run it :)
rfFuncsEdit <- rfFuncs
rfFuncsEdit$fit <- function(x, y, first, last, ...) {
  loadNamespace("randomForest")
  if(!is.factor(y)) y = as.factor(y)
  randomForest::randomForest(
    x, y,
    ntree = 200,
    importance = TRUE,
    type = "classification",
    ...)
}

### Recursive feature elimination (RFE), backwards selection ###

# RFE control options
rfe_ctrl <- caret::rfeControl(
  functions = rfFuncsEdit, # Use random forest classifier
  method = "cv", # Cross-validation
  number = 4, # 4 folds
  saveDetails = TRUE,
  verbose = TRUE,
  returnResamp = "all"
)

# Perform recursive feature eliminations (RFE)
rfe <- caret::rfe(
  x = as.matrix(training_data[feature_names]),
  y = as.factor(as.matrix(training_data["presence"])),
  sizes = 1:length(feature_names),
  metric = "Accuracy",
  rfeControl = rfe_ctrl
)

### Plot results
plot(rfe, type = c("g", "o"))
print(rfe$results)
print(rfe$optVariables)

# What does the plot tell you? 
# How many features do you need to train an accurate model? Why?
# If you use a smaller subset of the features for training, does it matter which of them you use?

################################
####### 7. Make predictions ####
################################

### Train model on full data set with tuned hyperparameters ###
rf <- ranger::ranger(
  x = training_data[feature_names],
  y = as.matrix(training_data["presence"]),
  classification = TRUE,
  probability = TRUE,
  num.trees = 500,
  mtry = best_mtry,
  max.depth = best_maxd,
  importance = "permutation"
)

### Spatial prediction
print(names(pred_stack))
print(rf$forest$independent.variable.names)

# Create factor list, needed for raster::predict compatibility
factor.list <- list(
  levels(training_data$ar50_artype),
  levels(training_data$geo_norge123)
)
names(factor.list) <- c("ar50_artype", "geo_norge123")

prediction_ras <- terra::predict(
  pred_stack,
  model = rf,
  type = "response",
  progress = "text",
  na.rm = TRUE,
  factors = factor.list,
  fun = function(model, ...) predict(model, ...)$predictions[,1]
)

# Create continuous color scale
color_scale <- rev(terrain.colors(255))

### Plot results, compare to true occurrences
terra::plot(prediction_ras, col = color_scale) # Where does the model predict high probability of occurrence?
points(training_data$x[training_data["presence"] == 1],
       training_data$y[training_data["presence"] == 1],
       col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1),
       cex = 0.3, pch = 2) # Where is the species observed?

# Check if the model predicts the observations accurately. Are there any specific areas
# where the model is particularly accurate or inaccurate?

# Zoom in to different areas to visualize occurrences more locally
local_extent <- ext(200000, 300000, 6850000, 7000000)
#local_extent <- ext(0, 100000, 6750000, 6900000)

terra::plot(prediction_ras, ext = local_extent, col = color_scale)
points(training_data$x[training_data["presence"] == 1],
       training_data$y[training_data["presence"] == 1],
       col = rgb(red = 0, green = 0, blue = 0, alpha = 1),
       cex = 0.5, pch = 16)

# Check if the model predicts the observations accurately in the local extents.

### How do you evaluate our model? Is our test reliable? Do the predictions make sense?
### What was done to prevent overfitting? Which features are important predictors and why? Do you have 
### ideas for improving the workflow?


################################
####### 8. Optional exercises ##
################################

# Option 1: Machine Learning and Distribution Modelling
#
# Write your own code (with inspiration from the code above) to test
# a different machine learning classifier on the same data. Read the
# documentation about the new classifier's hyper-parameters and use
# grid search tuning to optimize the model. You can basically copy-
# paste the entire script and adapt the parts where the ranger RF
# objects are involved. You do not need to re-run the data extraction,
# these objects will stay loaded in your R environment.
#
# Suggestions for classifiers (package):
#
# Support Vector Machine - SVM (e1071)
# Artificial Neural Networks (neuralnet)
# Adaptive Boosting / eXtreme gradient boosting (fastAdaboost, xgboost)
#
# Use one of the models in this list: 
# https://topepo.github.io/caret/available-models.html
#
# ATTENTION! Unlike Random Forest, most ML classifiers cannot handle 
# categorical features by themselves without prior transformations
# (e.g., one-hot encoding). Easiest fix: remove the categorical predictors
# from the feature matrix before training your model, and from the raster
# stack before making spatial predictions.


### Option 2: Practice R coding
# Write a function in R that takes an integer number (n) as input and 
# then prints the first n numbers of the Fibonacci series (0, 1, 1, 2, 
# 3, 5, 8, 13, ...).
# See: https://www.mathsisfun.com/numbers/fibonacci-sequence.html)
#
# Expected output:
# print_fibo(n = 5)
# > The first 5 elements of the Fibonacci series are: 0, 1, 1, 2, 3


### The End. ###
