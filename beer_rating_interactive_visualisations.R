#This script visualises interesting patterns from web-scraped beer-ratingss

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(devtools)){install.packages('devtools')} #developer version download
library(devtools)
devtools::install_version("plotly", version = "4.5.6", 
                          repos = "http://cran.us.r-project.org")
library(plotly)

###################################################################################################
# load data

load(url("https://github.com/rikunert/beer_rating/raw/master/BA_dat_2017-05-16.RData"))#beer advocate
BA_dat = BA_dat[BA_dat[,'raters'] > 100,]#remove beers with few raters

load(url("https://github.com/rikunert/beer_rating/raw/master/RB_dat_2017-05-18.RData"))#rate beer

load(url("https://github.com/rikunert/beer_rating/raw/master/UT_dat_2017-05-29.RData"))#beer advocate and untappd

# Beer advocate is the far better data base including more beers rated by more people, resulting in more diverse mean ratings

#download and load image of a star
z <- tempfile()
download.file('https://github.com/rikunert/Star_Trek_ratings/raw/master/gold_star.jpg',
              z, mode="wb")
pic <- readJPEG(z)
pic = rasterGrob(pic, interpolate=TRUE)
file.remove(z) # cleanup

###################################################################################################
#The interaction of bitterness and alcohol content for beer ratings

int_dat = UT_dat[complete.cases(UT_dat[c('UT_beer_name', 'UT_brewery', 'UT_ABV', 'UT_IBU', 'UT_rating', 'UT_raters')]),]

p <- plot_ly(int_dat, x = ~UT_ABV, y = ~UT_IBU, z = ~UT_rating, size = ~UT_raters,
             marker = list(symbol = 'circle', sizemode = 'area', color = ~UT_rating, colorscale = c('#708090', '#683531'), showscale = F),
             sizes = c(50, 1000), opacity = 0.4,
             hoverinfo = 'text',
             text = ~paste('Type: ', UT_sub_style, '<br>Beer: ', UT_beer_name, '<br>Brewery: ', UT_brewery,
                           '<br>Untappd user rating: ', UT_rating,
                           '<br>Untappd raters: ', UT_raters,
                           '<br>Alcohol content: ', UT_ABV, '%',
                           '<br>Bitterness: ', UT_IBU, 'IBU')) %>%
  layout(title = 'How bittnerness and alcohol make beer good',
         scene = list(xaxis = list(title = '% Alcohol (ABV)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(0, 20),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'Bitterness (IBU)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(0, 310),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      zaxis = list(title = 'Untappd rating',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(1, 5),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2)),
         annotations = list(list(x = 0, y = 0,
                            text = '@rikunert',
                            xref = 'paper',
                            yref = 'paper',
                            xanchor = 'left',
                            showarrow = F),
                            list(x = 1, y = 0,
                                 text = 'Source: untappd.com',
                                 xref = 'paper',
                                 yref = 'paper',
                                 xanchor = 'right',
                                 showarrow = F))
  )
p

###################################################################################################
#Which country produces the best beers?
levels(UT_dat$UT_loc)
