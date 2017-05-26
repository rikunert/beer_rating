#This script visualises interesting patterns from web-scraped beer-ratingss

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(ggplot2)){install.packages('ggplot2')} #main plotting library
library(ggplot2)

if(!require(jpeg)){install.packages('jpeg')} #adding image to plot
library(jpeg)

if(!require(grImport)){install.packages('grImport')} #adding image to plot
library(grImport)

if(!require(gtable)){install.packages('gtable')} #adding footer to plot
library(gtable)

if(!require(gridExtra)){install.packages('gridExtra')} #adding footer to plot
library(gridExtra)

if(!require(scales)){install.packages('scales')} #comma notation in plot
library(scales)

###################################################################################################
# load data

load(url("https://github.com/rikunert/beer_rating/raw/master/BA_dat_2017-05-16.RData"))#beer advocate
BA_dat = BA_dat[BA_dat[,'raters'] > 100,]#remove beers with few raters

load(url("https://github.com/rikunert/beer_rating/raw/master/RB_dat_2017-05-18.RData"))#rate beer

load(url("https://github.com/rikunert/beer_rating/raw/master/UT_dat_2017-05-26.RData"))#beer advocate and untappd

# Beer advocate is the far better data base including more beers rated by more people, resulting in more diverse mean ratings

#download and load image of a star
z <- tempfile()
download.file('https://github.com/rikunert/Star_Trek_ratings/raw/master/gold_star.jpg',
              z, mode="wb")
pic <- readJPEG(z)
pic = rasterGrob(pic, interpolate=TRUE)
file.remove(z) # cleanup

###################################################################################################
#prepare general look of plots (very clean)

theme_set(theme_bw(14)+#remove gray background, set font-size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  #panel.background = element_blank(),
                  panel.border = element_blank(),
                  plot.title = element_text(hjust = 0.5, face="bold"),
                  #legend.key = element_blank(),
                  #legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  #legend.position = 'top',#specify the legend to be on top
                  legend.direction = 'vertical',#specify the legend to be arranged vertically
                  panel.background = element_rect(fill = "orange",
                                                  colour = NA)))

###################################################################################################
#custom functions

add_stars = function(p, star_pic, star_xpos, star_size, star_distance){#input: a plot
    
    p = p + theme(plot.margin = unit(c(1,1,1,4), "lines"))#increase left margin
    
    for (i in seq(1,5)){#for each star level
      for (j in seq(1, i)){#for each individual star
        p = p + annotation_custom(grob = star_pic, xmin=-(star_size - star_xpos)-j*star_distance, xmax=star_xpos - j*star_distance, ymin=i - 0.3, ymax=i + 0.3)  #add 1 star at coordinates
      }}      
    
    # Code to override clipping of images to within drawing area
    gp <- ggplot_gtable(ggplot_build(p))
    gp$layout$clip[gp$layout$name == "panel"] <- "off"
    grid.draw(gp)

}

###################################################################################################
#Which types of beers do people like to drink?

#order according to best beer
x = tapply(BA_dat$rating[!is.na(BA_dat$rating)], BA_dat$sub_style[!is.na(BA_dat$rating)], median)#get median for each substyle
BA_dat$sub_style = factor(BA_dat$sub_style, levels = names(x)[order(x)])

beer_pref = ggplot(data = BA_dat, aes(x = sub_style, y = rating)) +
  geom_point(aes(size = raters), alpha = 0.2, color = 'white', position=position_jitter(w=0.2, h=0)) +
  stat_summary(fun.y=median, geom="point", aes(colour= super_style), size=4) +
  ylim(1, 5) +
  ggtitle("What kind of beer is the best?") +
  labs(size="# Ratings", shape="Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +#x-axis labels at an angle
  theme(legend.position = c(0.88, 0.23), axis.title.x = element_blank()) +#legend position in bottom right
  theme(legend.key = element_rect(color = 'transparent', fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent', color = 'white'),
        legend.box = "horizontal",
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  annotate("text", x = 1, y = min(x[!is.na(x)]), 
           vjust = 0, hjust=0, label="The worst: Light Lager", fontface = 2) +
  annotate("text", x = 100, y = max(x[!is.na(x)]), 
           vjust = 0.5, hjust=1, label="The best: Gueuze", fontface = 2)

beer_pref = beer_pref + labs(caption = c('@rikunert                                                                                                                                                                                        Source: beeradvocate.com')) + 
  theme(plot.caption = element_text(size = 12, color = 'grey', face= 'italic'))

add_stars(beer_pref, star_pic = pic, star_xpos = 0.5, star_size = 1.5, star_distance = 1.5)#add vertical stars

###################################################################################################
#Are beer-ratings reliable?

beer_corr = ggplot(data = UT_dat, aes(x = BA_rating, y = UT_rating)) +
  geom_point(aes(size = UT_raters), alpha = 0.2, color = 'white') +
  ylim(1, 5) + xlim(1,5) + scale_size_continuous(labels=comma) +
  geom_abline(intercept = 0, slope = 1,
              colour = 'grey95', linetype = 5, size = 1) +#ideal line
  ggtitle("Do raters agree across beer-rating web sites?") +
  labs(size="# Untappd Ratings") +#legend title
  theme(legend.position = c(0.88, 0.23), axis.title.x = element_blank()) +#legend position in bottom right
  theme(legend.key = element_rect(color = 'transparent', fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent', color = 'white'),
        legend.box = "horizontal",
        axis.title=element_blank(),
        axis.text=element_blank()) +
  annotate("text", x = 1, y = 1.1, vjust = 0, hjust=0, angle = 23, label="Untappd = Beeradvocate", fontface = 1, colour = 'grey95') + #ideal fit line annotation
  annotate("text", x = 3, y = 1, vjust = 1, hjust=0.5, label="Beeradvocate rating", fontface = 1) + #x-axis
  annotate("text", x = 1, y = 3, vjust = -2.25, hjust=0.5, angle = 90, label="Untappd rating", fontface = 1) #y-axis

#fit measures
x = summary(lm(UT_rating ~ BA_rating, data = UT_dat))
beer_corr = beer_corr +
  annotate('text', x = 1, y = 5, vjust = 1, hjust = 0, fontface = 1, 
           label = sprintf('R squared = %0.2f\nRMSE = %1.2f', x$r.squared, sqrt(mean((x$residuals)^2))))

beer_corr = beer_corr + labs(caption = c('@rikunert                                                                                                                                                           Source: beeradvocate.com & untappd.com')) + 
  theme(plot.caption = element_text(size = 12, color = 'grey', face= 'italic', margin = margin(t = 10,10,15,10)))

#add horizontal stars (only visible after calling add_stars())
star_distance = 0.06
for (i in seq(1,5)){#for each star level
  for (j in seq(1, i)){#for each individual star
    beer_corr = beer_corr + annotation_custom(grob = pic,
                              xmin = (i - 0.12 - (i-1)*star_distance / 2) + j*star_distance,#
                              xmax = (i- (i-1)*star_distance / 2) + j*star_distance,#
                              ymin = 0.65, ymax = 0.65 + 0.13)  #add 1 star at coordinates
  }}

add_stars(beer_corr, star_pic = pic, star_xpos = 0.83, star_size = 0.06, star_distance = 0.065)#add vertical stars

###################################################################################################
#Which website offers the most ratings?
hist_dat = data.frame(raters = c(UT_dat[,'UT_raters'], BA_dat[,'raters'], RB_dat[,'raters']),
                      website = c(rep('Untappd', nrow(UT_dat)),
                                  rep('Beer advocate', nrow(BA_dat)), 
                                  rep('Rate beer', nrow(RB_dat)))
                      )
hist_dat$website = ordered(hist_dat$website, c('Untappd', 'Beer advocate', 'Rate beer'))
ggplot(data = hist_dat, aes(x = raters, colour = website)) +
  geom_freqpoly(size = 1.5) +
  scale_colour_grey(start = 0.7, end = 1) +
  xlim(-100,20000) + ggtitle("Which beer rating website is the biggest?") + xlab('# Raters per beer') + ylab('Count of beers') +
  theme(legend.position = c(0.88, 0.5)) +#legend position in bottom right
  theme(legend.key = element_rect(color = 'transparent', fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent', color = 'white'),
        legend.box = "horizontal") + 
  labs(caption = c('@rikunert                                                                                                                                Source: ratebeer.com, beeradvocate.com, & untappd.com')) + 
  theme(plot.caption = element_text(size = 12, color = 'grey', face= 'italic', margin = margin(t = 10,10,15,10)))

###################################################################################################
#How much alcohol should a beer have

alc_plot = ggplot(data = UT_dat, aes(x = UT_ABV, y = UT_rating)) +
  geom_point(aes(size = UT_raters), alpha = 0.2, color = 'white') +
  ylim(1, 5) + xlim(0,20) + scale_size_continuous(labels=comma) +
  theme(legend.key = element_rect(color = 'transparent', fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent', color = 'white'),
        legend.box = "horizontal",
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(size="# Ratings") +#legend title
  ggtitle("Beer: the more alcohol the better") +
  xlab('% Alcohol') +
  theme(legend.position = c(0.88, 0.23)) #+#legend position in bottom right

#add log-linear model to visualisation
m1 = lm(UT_rating ~ log(UT_dat[,'UT_ABV']), data = UT_dat, weights = UT_raters)#model bin counts as a function of reference logged age (log-model is best fitting model after trying out a few)  
pred = predict(m1, interval="conf", newdata=data.frame(UT_dat[,'UT_ABV']))    
dat_mod = data.frame(ABV = UT_dat[,'UT_ABV'], pred = pred[,1])

alc_plot = alc_plot + geom_line(data = dat_mod, aes(x = ABV, y = pred),
            colour = 'grey95', linetype = 5, size = 1)

x = summary(m1)
alc_plot = alc_plot +
  annotate('text', x = 0, y = 5, vjust = 1, hjust = 0, fontface = 1, 
           label = sprintf('R squared = %0.2f\nRMSE = %1.2f', x$r.squared, sqrt(mean((x$residuals)^2))))

alc_plot = alc_plot + labs(caption = c('@rikunert                                                                                                                                                                                                Source: untappd.com')) + 
  theme(plot.caption = element_text(size = 12, color = 'grey', face= 'italic', margin = margin(t = 10,10,15,10)))

add_stars(alc_plot, star_pic = pic, star_xpos = -1, star_size = 0.3, star_distance = 0.3)#add vertical stars

###################################################################################################
#How bitter should a beer be?

IBU_plot = ggplot(data = UT_dat, aes(x = UT_IBU, y = UT_rating)) +
  geom_point(aes(size = UT_raters), alpha = 0.2, color = 'white') +
  ylim(1, 5) + xlim(0,20) + scale_size_continuous(labels=comma) +
  geom_smooth(se = F, colour = 'grey95', linetype = 5, size = 1) +
  theme(legend.key = element_rect(color = 'transparent', fill = 'transparent'),
        legend.background = element_rect(fill = 'transparent', color = 'white'),
        legend.box = "horizontal",
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(size="# Ratings") +#legend title
  ggtitle("Beer: bitterness does not matter") +
  xlab('Bitterness (IBU value)') +
  theme(legend.position = c(0.1, 0.23)) #+#legend position in bottom right
  
IBU_plot = IBU_plot + labs(caption = c('@rikunert                                                                                                                                                                                                Source: untappd.com')) + 
  theme(plot.caption = element_text(size = 12, color = 'grey', face= 'italic', margin = margin(t = 10,10,15,10)))

add_stars(IBU_plot, star_pic = pic, star_xpos = -1, star_size = 0.3, star_distance = 0.3)#add vertical stars
