#This script visualises interesting patterns from web-scraped beer-ratingss

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(ggplot2)){install.packages('ggplot2')} #main plotting library
library(ggplot2)


###################################################################################################
# load data
load(url("https://github.com/rikunert/beer_rating/raw/master/BA_dat_2017-05-16.RData"))#beer advocate
load(url("https://github.com/rikunert/beer_rating/raw/master/RB_dat_2017-05-17.RData"))#rate beer
