#Instructions for installation of nimble R package
#Ryan C. Burner rburner@usgs.gov
#UiO Distribution Modeling Course 2025
#built using R 4.5.1

########################
#  WINDOWS USERS
########################
#Install Rtools
##YOU MAY NEED ADMINISTRATOR RIGHTS TO DO THIS
#use this link:  
#  https://cran.r-project.org/bin/windows/Rtools/ 

#After install Rtools, you need to tell R where to find it:
write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)

#Confirm it worked:
Sys.which("make")
#should get something like this if using Windows:
## "C:\\rtools45\\usr\\bin\\make.exe"

###########################
# Mac and Linux users
###########################
#follow instructions here
# https://r-nimble.org/download.html

#I can't verify this will work, but chatGPT tells me to do as follows:

# On macOS:
# You must have a working C++ compiler.
## 1. Install Xcode command-line tools:
system("xcode-select --install")

# On Linux (Debian/Ubuntu):
## 1. Install build tools:
system("sudo apt-get install -y build-essential")


###########################
# Everyone
###########################

#to install mcmcplots, use this sequence:
#(if this fails it will only affect mcmc visualizations, can still do lab)
install.packages('sfsmisc')
install.packages('colorspace')
install.packages('denstrip')
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/mcmcplots/mcmcplots_0.4.3.tar.gz",
  repos = NULL,
  type = "source"
)

#now install any other packages from below that you don't already have
#try to load them with the code below, 
#then install any that are missing and reload them
install.packages(" [PUT PACKAGE NAME HERE, ONE AT A TIME] ")


#load packages - make sure you have all of these
library(nimble)
library(coda)
library(mcmcplots)
library(wiqid)
library(unmarked)
library(raster)

#nimble test model
code <- nimbleCode({
  mu ~ dnorm(0, sd = 1000)
  sigma ~ dunif(0, 1000)
  for(i in 1:10) {
    x[i] ~ dnorm(mu, sd = sigma)
  }
})

#nimble test data
data <- list(x = c(2, 5, 3, 4, 1, 0, 1, 3, 5, 3))

#nimble test initial values
inits <- function() list(mu = rnorm(1,0,1), sigma = runif(1,0,10))

#compile and fit model test
#this next step takes a few minutes...
mcmc.output <- nimbleMCMC(code, data = data, inits = inits,
                          monitors = c("mu", "sigma"), thin = 10,
                          niter = 20000, nburnin = 1000, nchains = 3,
                          summary = TRUE, WAIC = TRUE)
#print output
mcmc.output

#plot mcmc chains
#if this part fails you can still complete the lab 
#(just ignore lines with this function)
mcmcplot(mcmc.output$samples)

#you should get no errors and the last object (mcmc output) should print. 
#Then you are ready to go!