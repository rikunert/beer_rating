#This script visualises interesting patterns from web-scraped beer-ratingss

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries
gif_building = T

if(gif_building == F){#older version for interactive plot version, otherwise file too big for upload
  
  if(!require(devtools)){install.packages('devtools')} #developer version download
  library(devtools)
  devtools::install_version("plotly", version = "4.5.6",
                            repos = "http://cran.us.r-project.org")
  
} else {

    install.packages('plotly')#most up to date plotly version for gif building, otherwise 500 HTTP error
  
}

library(plotly)
###################################################################################################
# load data
load(url("https://github.com/rikunert/beer_rating/raw/master/UT_dat_2017-05-29.RData"))#beer advocate and untappd
UT_dat$ALL_rating = with(UT_dat, (UT_rating*UT_raters + BA_rating*BA_raters)/(UT_raters + BA_raters))#overall rating
UT_dat$ALL_raters = with(UT_dat, UT_raters + BA_raters)#total number of raters

###################################################################################################
#The interaction of bitterness and alcohol content for beer ratings

xmax = 20#ABV
ymax = 310#IBU
interp_dens = 100#interpolation density for modeling

int_dat = UT_dat[complete.cases(UT_dat[c('UT_beer_name', 'UT_brewery', 'UT_ABV', 'UT_IBU', 'ALL_rating', 'ALL_raters')]),]

#Which individual beer is best?
#head(int_dat[order(-int_dat[,'UT_rating']),], 10)#order to see who is best in text
#quantile(int_dat$UT_IBU, seq(0, 1, 0.01))#put values in perspective

#generate LOESS model of beer ratings and predict interpolated ratings
m = loess(ALL_rating ~ UT_ABV * UT_IBU, data = int_dat, weights = ALL_raters)
x_marginal = seq(min(int_dat$UT_ABV), xmax, length = interp_dens)
y_marginal = seq(min(int_dat$UT_IBU), ymax, length = interp_dens)
data.fit <-  expand.grid(list(UT_ABV = x_marginal, UT_IBU = y_marginal))
pred_interp = predict(m, newdata = data.fit)#interpolated ratings    

#hover text for surface plot (LOESS model)
hover <- with(data.frame(x = rep(x_marginal, interp_dens),
                         y = rep(y_marginal, each = interp_dens),
                         p = as.vector(pred_interp)),
              paste('Modeled beer (LOESS)', '<br>',
                    "Predicted rating: ", round(p, digits = 2), "stars", '<br>',
                    "Alcohol content: ", round(x, digits = 2), '%', '<br>',
                    'Bitterness: ', round(y, digits = 2), 'IBU'))
hover_m = matrix(hover, nrow = interp_dens, ncol = interp_dens, byrow = T)

p <- plot_ly() %>%
  add_markers(data = int_dat, x = ~UT_ABV, y = ~UT_IBU, z = ~ALL_rating, size = ~ALL_raters,
             marker = list(symbol = 'circle', sizemode = 'area',
                           color = ~ALL_rating, colorscale = c('#708090', '#683531'), showscale = F,
                           zmin = 2, zmax = 5),
             sizes = c(50, 1000), opacity = 1,
             name = 'Beers',
             hoverinfo = 'text',
             text = ~paste('Actual beer',
                           '<br>Name: ', UT_beer_name,
                           '<br>Brewery: ', UT_brewery,
                           '<br>Type: ', UT_sub_style,
                           '<br>user rating: ', ALL_rating,
                           '<br>Untappd raters: ', ALL_raters,
                           '<br>Alcohol content: ', UT_ABV, '%',
                           '<br>Bitterness: ', UT_IBU, 'IBU')) %>%
  add_surface(x = x_marginal, y = y_marginal,
              z = t(pred_interp),#add_surface() expects the x-values on the columns and the y-values on the rows (very confusing, I know)
              opacity = 0.7,
              name = 'LOESS model',
              hoverinfo = 'text',
              text = hover_m,
              showscale = F,
              colorscale = c('#708090', '#683531'),
              cmin = 2, cmax = 5) %>%
  layout(title = 'How bitterness and alcohol make good beer',
         scene = list(xaxis = list(title = '% Alcohol',
                                   gridcolor = 'rgb(255, 255, 255)',#white
                                   range = c(0, xmax)),
                      yaxis = list(title = 'Bitterness',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(0, ymax)),
                      zaxis = list(title = 'Rating',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(1, 5))),
         annotations = list(list(x = 0, y = 0,#bottom left corner of frame
                                 text = '<a href="https://twitter.com/rikunert">@rikunert</a>',
                                 xref = 'paper', yref = 'paper',
                                 xanchor = 'left',#left aligned
                                 showarrow = F),
                            list(x = 1, y = 0,#bottom right corner of frame
                                 text = 'Source: <a href="http://untappd.com">untappd.com</a> <br>and <a href="http://beeradvocate.com">beeradvocate.com</a>',
                                 xref = 'paper', yref = 'paper',
                                 xanchor = 'right',#right aligned
                                 showarrow = F)))

#save on plotly website
if(gif_building == F) plotly_POST(p, filename = "ABV_IBU_ratings")

#save images for gif building
#be sure to have most up to date version of plotly
if (gif_building) {
  
  
  for(i in seq(0,pi * 2, length = 60)){
    
    outfile <- paste('C:\\Users\\Richard\\Desktop\\R\\beer_rating\\ABV_IBU_UT_gif\\ABV_IBU_ratings',round(i,digits=2), sep = "_")
    #outfile = paste('ABV_IBU_ratings',round(i,digits=2), sep = "_")
    cam.zoom = 2
    
    p = p %>% layout(showlegend = F,
                     scene=list(camera = list(eye = list(x = cos(i)*cam.zoom, y = sin(i)*cam.zoom, z=0.8),
                                          center = list(x = 0,
                                                        y = 0,
                                                        z = 0
                                          ))))
    
    cat("Now rendering iteration:", i,"\n")
    plotly_IMAGE(p,
                 width = 700,
                 height = 650,
                 format = "png",
                 scale = 1,
                 out_file = paste(outfile,"png", sep="."))
    
  }
}

#now that images are up, combine them into a gif:
#download ImageMagick
#open windows terminal
#navigate to folder housing all the png files making up gif
#> "C:\Program Files\ImageMagick-7.0.5-Q16\magick" *.png -delay 5 -loop 0 name.gif

###################################################################################################
#Which country produces the best beers?

#aggregate data by country
#get list of country names by using this example data set from the plotly website, not neat but it will do the job
ex <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
countries = ex$COUNTRY

map_dat = data.frame(country = ex$COUNTRY,
                     code = ex$CODE,
                     rating = rep(NaN, length(ex$COUNTRY)),
                     raters = rep(NaN, length(ex$COUNTRY)),
                     beers = rep(NaN, length(ex$COUNTRY)),
                     breweries = rep(NaN, length(ex$COUNTRY)),
                     best_beer = rep(NaN, length(ex$COUNTRY)),
                     best_brewery = rep(NaN, length(ex$COUNTRY)))

counter = 0
for(c in countries){#for each country in the world
  counter = counter + 1
  
  if(c == 'Korea, South') {
    c = 'South Korea' 
    map_dat[counter, 'code'] = 'KOR'}#curiously, the country codes of the Koreas were swapped
  if(c == 'Niger') c = ' Niger'#no Niger beers in data base, avoid confusion with Nigeria
  if(c == 'Korea, North') map_dat[counter, 'code'] = 'PRK'#curiously, the country codes of the Koreas were swapped
  if(c == 'Bahamas, The') c = 'Bahamas'
  if(c == 'United Kingdom') c = 'England|Scotland|Wales'
  
  print(c)
  map_dat[counter, 'rating_beermean'] = mean(UT_dat[grep(c, UT_dat$UT_loc),'ALL_rating'], na.rm = T)
  map_dat[counter, 'rating'] = weighted.mean(UT_dat[grep(c, UT_dat$UT_loc),'ALL_rating'], UT_dat[grep(c, UT_dat$UT_loc),'ALL_raters'], na.rm = T)
  map_dat[counter, 'raters'] = sum(UT_dat[grep(c, UT_dat$UT_loc),'ALL_raters'], na.rm = T)
  map_dat[counter, 'beers'] = length(unique(UT_dat[grep(c, UT_dat$UT_loc),'UT_beer_name']))
  map_dat[counter, 'breweries'] = length(unique(UT_dat[grep(c, UT_dat$UT_loc),'UT_brewery']))

  #best_beers_c = unique(UT_dat[grep(c, UT_dat$UT_loc) & UT_dat[grep(c, UT_dat$UT_loc),'UT_rating'] == max(UT_dat[grep(c, UT_dat$UT_loc),'UT_rating'], na.rm = T), 'UT_beer_name'])
  #map_dat[counter, 'best_beer'] = paste(best_beers_c[!is.na(best_beers_c)], collapse = ' | ')

  #breweries_c = unique(UT_dat[grep(c, UT_dat$UT_loc),'UT_brewery'])
  #brewery_ratings_c = sapply(breweries_c, function(x) weighted.mean(UT_dat[UT_dat[,'UT_brewery'] == x,'UT_rating'], UT_dat[UT_dat[,'UT_brewery'] == x,'UT_raters'], na.rm = T))
  #best_breweries_c = breweries_c[brewery_ratings_c == max(brewery_ratings_c, na.rm = T)]
  #map_dat[counter, 'best_brewery'] = paste(best_breweries_c[!is.na(best_breweries_c)], collapse = ' | ')
  
}

map_dat = map_dat[!is.na(map_dat[,'rating']),]
#map_dat[order(-map_dat[,'rating']),]#order to see who is best in text

#hover text
map_dat$hover <- with(map_dat, paste("Country: ", country, '<br>',
                                     "Mean rating: ", round(rating, digits = 2), "stars", '<br>',
                                     "Raters: ", raters, "<br>",
                                     "Beers: ", beers, "<br>",
                                     "Breweries: ", breweries)
                      )

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = F,
  showcoastlines = T,
  projection = list(type = 'orthographic')
  )

p <- plot_geo(map_dat) %>%
  add_trace(
    z = ~rating, color = ~rating, colors = c('#708090', '#d30f00'),
    text = ~hover, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Rating', ticksuffix = ' stars') %>%
  layout(
    title = 'Which country produces the best beers in the world?',
    geo = g,
    hoverinfo = 'text',
    annotations = list(list(x = 0, y = 0,
                            text = '<a href="https://twitter.com/rikunert">@rikunert</a>',
                            xref = 'paper',
                            yref = 'paper',
                            xanchor = 'left',
                            showarrow = F),
                       list(x = 1, y = 0,
                            text = 'Source: <a href="http://untappd.com">untappd.com</a> <br>and <a href="http://beeradvocate.com">beeradvocate.com</a>',
                            xref = 'paper',
                            yref = 'paper',
                            xanchor = 'right',
                            showarrow = F))
  )
p
plotly_POST(p, filename = "globe_beer_rating")#push plotly post to plotly website # Set up API credentials: https://plot.ly/r/getting-started
