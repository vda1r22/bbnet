# Testing function


## Step 1
# Load library
library(bbnet)


## Step 2
# Load your data into R
data("bbn.model","priors1", "priors2", "priors3") # to load package example
# bbn.model <- read.csv() # to load your own dataset file
# priors1 <- read.csv()


## Step 3
# Create your prediction
# bbn.predict(end, boot_max, values, figure)
# end us the number of policy scenarios to perform - default of 1
# boot_max is the number of bootstraps to perform - default of 1
# values -  1 (default) prints the final values and confidence intervals of each scenario, 0 (or any other value) excludes these as an output
# figure - 0 no figures are produced, 1 (default) PDF is saved to working directory, 2 figure is presented in figure panel
# can be run simply as BBN.predict()

# bbn.predict()
bbn.predict(end = 3, boot_max = 1, values =0, figure = 2)

# Things to maybe include:
# figure option 3, prints figures independently, not grouped as all scenarios together
# some options for the figures, such as font sizes
