# BIOS 5211/9211
# Olav Skarpaas, NHM, University of Oslo, Nov 2025
# Based on code by Ovaskainen and Abrego 2020, Joint Species Distribution Modelling - 
# With Applications in R, Cambridge University Press.
# https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc


# Lab7. Hierarchical modelling of species communities (Hmsc)
############################################################

# In this lab we will 
# 1) Explore joint species distribution modelling with the package Hmsc
# 2) Demonstrate a way to organize and source code from multiple scripts
# 
# This script (Lab7_HMSC.R) contains mostly comments about the modelling
# process and instructions about how to run supporting code in other files and
# access the output from those files for model results and interpretation.

# The case study is taken from Ovaskainen and Abrego 2020, Joint Species 
# Distribution Modelling - With Applications in R, Cambridge University Press.
# Code for this exercise on birds (Section 11.1) is downloaded from
# https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc
# and slightly modified (minor updates/corrections labeled #OS:)


# 0. Preparation: install packages and set working directory
# ----------------------------------------------------------

# You need the Hmsc package to run the next scripts
install.packages("Hmsc")
library(Hmsc)

# Use 'setwd()' or your favorite method to set working directory to the folder
# 'lab_exercises/Lab7_HMSC/Ovaskainen_Abrego_2020/Section_11_1_birds/'
# This folder contains the scripts to be run in the next steps; one script for
# each of five steps.


# 1. Define and fit models
# ------------------------
# To run the entire script that defines and fits the models to data,
# you can uncomment and run the 'source' command below - but it takes a bit of 
# time on an ordinary computer, and to save time in the lab I already run it, 
# so that you don't have to do it now. The model results can be found in the 
# folder 'models'. If you run the script by sourcing, you get some plots
# (in your current plotting device), and by running the script line by line,
# you also get additional output in the console (see also options in ?source).

#source("S1_fit_models_11_1.R")

# At some point, you may want to open the script and take a look at how the data
# are structured. Useful for interpretation.


# 2. Evaluate convergence
# -----------------------
# The next step is to evaluate model convergence. Run the source
# command, and notice the argument 'echo=TRUE'. What does it do?

source("S2_evaluate_convergence_11_1.R",echo=TRUE)
# Histograms show model convergence, or potential scale reduction factors (psrf),
# for Beta and Omega parameters (niche and species association parameters).
# What do you think about model convergence based on the plotted histograms?


# 3. Explore model fit
# --------------------
# This script takes a little while (a few minutes on my computer).
# Results are in the 'models' folder ("CV_chains...").

source("S3_explore_model_fit_11_1.R",echo=TRUE)
# What do you make of the AUC and TjurR2 values in the printed output?
# AUC = explanatory power. AUC (CV) = predictive power.
# How is predictive power compared to explanatory power? See plot.


# 4. Explore parameter estimates
# ------------------------------
# This script produces several plots as outputs, displaying parameter estimates
# to support inference.

source("S4_explore_parameter_estimates_11_1.R",echo=TRUE)
# Look through the plots, try to interpret (some of) them, and check with 
# the associated comments in the script.


# 5. Make predictions
# -------------------
# The final script makes predictions from the models across environmental
# gradients and across space, and plots the output.

source("S5_make_predictions_11_1.R",echo=TRUE)
# Look through the plots, try to interpret (some of) them, and check with 
# the associated comments in the script.
# How could you make continuous map predictions?

