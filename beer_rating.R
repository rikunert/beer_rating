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
BA_dat = BA_dat[BA_dat[,'raters'] > 100,]#remove beers with few raters

load(url("https://github.com/rikunert/beer_rating/raw/master/RB_dat_2017-05-17.RData"))#rate beer

###################################################################################################
#prepare general look of plots (very clean)

theme_set(theme_bw(18)+#remove gray background, set font-size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  plot.title = element_text(hjust = 0.5, face="bold"),
                  legend.key = element_blank(),
                  legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  legend.position = 'top',#specify the legend to be on top
                  legend.direction = 'vertical'))#specify the legend to be arranged vertically

###################################################################################################
# raw distribution
BA_dat$ABV

## Beer advocate the far better data base

#beer advocate has far more beers in the data base
dim(BA_dat)#far bigger data base
dim(RB_dat)#far smaller data base

#beers are typically rated by more people on beer advocate
median(BA_dat[,'raters'])#
median(RB_dat[,'raters'])#

#mean ratings are more diverse for beer advocate
sd(BA_dat[,'raters'])#
sd(RB_dat[,'raters'])#


unique(BA_dat[,'super_style'])#includes super styles
unique(RB_dat[,'sub_style'])

dens_plot = ggplot(data = RB_dat, aes(x = raters)) +
  geom_density() +
  labs(x = 'Raters', y = 'Density', title = 'beer advocate')
  #geom_density(aes(group=sub_style, colour=super_style))
dens_plot

dens_plot = ggplot(data = RB_dat, aes(x = rating)) +
  geom_density(aes(group=sub_style))+
  labs(x = 'Star rating', y = 'Density', title = 'rate beer') + xlim(c(1, 5))

dens_plot

scat_plot = ggplot(data = BA_dat, aes(x = ABV, y = rating, group = sub_style)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = F) +
  labs(x = 'ABV', y = 'rating', title = 'beer advocate')
#geom_density(aes(group=sub_style, colour=super_style))
scat_plot

  geom_bar(stat = 'identity', fill = '#000080') +
  labs(x = 'Year', y = 'Published articles', title = 'PLoS ONE has passed its publication peak') +#add labels and title
  theme(axis.text = element_text(size = 20)) + #axis label size
  theme(axis.title = element_text(size = 22)) +#axis title size
  theme(plot.title = element_text(size = 24)) +
  scale_x_discrete(breaks = levels(articles$year)[c(T, rep(F, 1))])#leave one year blank on x-axis after every year labeled

bar_plot#print actual plot


