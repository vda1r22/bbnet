# Testing function

library(bbnet)

data("BBNfile","priors1")

BBNfile <- read.csv()

input.files(BBNfile, priors1, priors2, priors3) # set up of all the BBN models

# bbn.predict(boot_max, values, figure)
# boot_max is the number of bootstraps to perform - default of 1
# values -  1 (default) prints the final values and confidence intervals of each scenario, 0 (or any other value) excludes these as an output
# figure - 0 no figures are produced, 1 (default) PDF is saved to working directory, 2 figure is presented in figure panel
# can be run simply as BBN.predict()

bbn.predict() # ERROR AT THIS POINT - 173 I THINK
# only if loading priors again as dataframe
# if running as it is: ERROR TO SET COLNAMES on priors I THINK

bbn.predict(boot_max = 1000, values =0, figure = 2)

# Things to maybe include:
# figure option 3, prints figures independently, not grouped as all scenarios together
# some options for the figures, such as font sizes
