#This script web-scrapes and then saves information about beer ratings from
# untapped based on beer anems from beeradvocate and ratebeer

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(RCurl)){install.packages('RCurl')} #html-tree parsing
library(RCurl)
options(RCurlOptions = list(useragent = "zzzz"))

if(!require(httr)){install.packages('httr')} #html-tree parsing
library(httr)

if(!require(XML)){install.packages('XML')} #html-tree parsing
library(XML)

if(!require(stringdist)){install.packages('stringdist')} #fuzzy string matching
library(stringdist)

###################################################################################################
# custom functions

#web_pre takes a webpage of address web_add and pre-processes it
#input: website address
#output: parsed html tree

web_pre = function(web_add, ssl_verification = T){
  
  web_nod <- getURL(web_add, .opts = list(ssl.verifypeer = ssl_verification))#load the webpage
  
  if (web_nod == "") {
    web_nod <- GET(web_add)#revert to httr package, don't ask me why it works
  } else {
    
    web_nod <- readLines(tc <- textConnection(web_nod))# Process escape characters
    close(tc)
  }
  
  tree <- htmlTreeParse(web_nod, error=function(...){}, useInternalNodes = TRUE)# Parse the html tree, ignoring errors on the page
  
  return(tree)#the parsed html tree is returned
}

###################################################################################################
# load data

load(url("https://github.com/rikunert/beer_rating/raw/master/BA_dat_2017-05-16.RData"))#beer advocate
#BA_dat = BA_dat[BA_dat[,'raters'] > 100,]#remove beers with few raters

#load(url("https://github.com/rikunert/beer_rating/raw/master/RB_dat_2017-05-18.RData"))#rate beer

# Beer advocate is the far better data base including more beers rated by more people, resulting in more diverse mean ratings

###################################################################################################
# web-scrape untappd

#search for 'a beer'

UT_dat = data.frame(BA_super_style = character(),
                    UT_sub_style = character(),
                    BA_sub_style = character(),
                    UT_beer_name = character(),
                    UT_brewery = character(),
                    BA_beer_name = character(),
                    BA_brewery = character(),
                    UT_ABV = double(),
                    BA_ABV = double(),
                    UT_IBU = double(),
                    UT_rating = double(),
                    UT_raters = integer(),
                    BA_rating = double(),
                    BA_raters = integer())

#get all beers by searching through already known beer names

for(i in seq(1, nrow(BA_dat))){
  
  print(paste('https://untappd.com/search?q=', gsub(' ', '+', BA_dat[i,'beer_name']),'&type=beer',sep = ""))
  UT_web = web_pre(paste('https://untappd.com/search?q=', gsub(' ', '+', BA_dat[i,'beer_name']),'&type=beer',sep = ""), ssl_verification = F)#
  
  if(length(getNodeSet(UT_web, "//div[@class ='results-none']")) == 0){#test whether there is a search result at all
    
    #enter into top search result (ordered by popularity)
    link_nodes = getNodeSet(UT_web, '//a')
    
    if(length(link_nodes) >= 18) {
      
      print(paste('https://untappd.com', as.character(xmlAttrs(link_nodes[[18]])), sep = ''))
      
      if(length(as.character(xmlAttrs(link_nodes[[18]]))) == 1) {
        
        UT_web = web_pre(paste('https://untappd.com', as.character(xmlAttrs(link_nodes[[18]])), sep = ''), ssl_verification = F)#
        
        #distance (percentage of max)
        tryCatch({
          
          beer_name_dist = stringdist(as.character(gsub('[[:punct:]]',' ', enc2native(as.character(BA_dat[i,'beer_name'])))),
                                      as(xmlChildren(link_nodes[[18]])$text, "character"), # name of the beer on untappd
                                      method = 'osa') / max(nchar(as.character(gsub('[[:punct:]]',' ', enc2native(as.character(BA_dat[i,'beer_name']))))),
                                                            nchar(as(xmlChildren(link_nodes[[18]])$text, "character")))
          
        }, error=function(cond) {
          message(cond)
          # Choose a return value in case of error
          beer_name_dist = NA
        })
        
        
        tryCatch({
          brewery_name_dist = stringdist(as.character(gsub('[[:punct:]]',' ', enc2native(as.character(BA_dat[i,'brewery'])))),
                                         as(xmlChildren(link_nodes[[19]])$text, "character"), # name of the beer on untappd
                                         method = 'osa') / max(nchar(as.character(gsub('[[:punct:]]',' ', enc2native(as.character(BA_dat[i,'brewery']))))),
                                                               nchar(as(xmlChildren(link_nodes[[19]])$text, "character")))
          
        }, error=function(cond) {
          message(cond)
          # Choose a return value in case of error
          brewery_name_dist = NA
        })
        
        tryCatch({
          ABV_diff = abs(as.numeric(strsplit(strsplit(as.character(xmlToDataFrame(getNodeSet(UT_web, "//p[@class ='abv']"))[[1]]), '% ')[[1]][1], '\n')[[1]][2]) - BA_dat[i, 'ABV']) / BA_dat[i, 'ABV']  
        }, error=function(cond){
          message(cond)
          ABV_diff = NA
        })
        
        
        if( sum(beer_name_dist*1.5, brewery_name_dist*1.5, ABV_diff*5, na.rm = T) > 1.50){#if differences too big
          
          cat(sprintf('X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X-X\n %d / %d : Differences too great. \nBA name: %s \nUntappd name: %s \n%s\n\n',
                      i, nrow(BA_dat),
                      BA_dat[i,'beer_name'],
                      as(xmlChildren(link_nodes[[18]])$text, "character"),
                      paste('https://untappd.com', as.character(xmlAttrs(link_nodes[[18]])), sep = '')))
          
        } else {#differences still ok
          
          cat(sprintf('O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O-O\n %d / %d : Differences ok. \nBA name: %s \nUntappd name: %s \n%s\n\n',
                      i, nrow(BA_dat),
                      BA_dat[i,'beer_name'],
                      as(xmlChildren(link_nodes[[18]])$text, "character"),
                      paste('https://untappd.com', as.character(xmlAttrs(link_nodes[[18]])), sep = '')))
          
          beer_name = as.character(xmlToDataFrame(getNodeSet(UT_web, '//h1'))$text)
          if(identical(beer_name, character(0))) beer_name = gsub(' ', '', as.character(xmlToDataFrame(getNodeSet(UT_web, '//h1'))$u))#the Baltika case
          
          UT_dat <- rbind(UT_dat, data.frame(
            BA_super_style = BA_dat[i,'super_style'],#not included by untappd
            UT_sub_style = as.character(xmlToDataFrame(getNodeSet(UT_web, "//p[@class ='style']"))[[1]]),
            BA_sub_style = BA_dat[i,'sub_style'],#
            UT_beer_name = beer_name,
            UT_brewery = as.character(xmlToDataFrame(getNodeSet(UT_web, "//p[@class ='brewery']"))[[2]]),
            BA_beer_name = BA_dat[i, 'beer_name'],
            BA_brewery = BA_dat[i, 'brewery'],
            UT_ABV = as.numeric(strsplit(strsplit(as.character(xmlToDataFrame(getNodeSet(UT_web, "//p[@class ='abv']"))[[1]]), '% ')[[1]][1], '\n')[[1]][2]),
            BA_ABV = BA_dat[i, 'ABV'],
            UT_IBU = as.numeric(strsplit(as.character(xmlToDataFrame(getNodeSet(UT_web, "//p[@class ='ibu']"))[[1]]), 'IBU')[[1]][1]),
            UT_rating = as.numeric(gsub("\\(|\\)", "", as.character(xmlToDataFrame(getNodeSet(UT_web, "//span[@class ='num']"))[[1]]))),#only supplied if enough raters
            UT_raters = as.numeric(gsub("([0-9]+).*$", "\\1", gsub(",","", as.character(xmlToDataFrame(getNodeSet(UT_web, "//p[@class ='raters']"))[[1]])))),
            BA_rating = BA_dat[i,'rating'],
            BA_raters = BA_dat[i,'raters']
          ))
        }}}}
}

###################################################################################################
# save data

save(UT_dat, file=sprintf("UT_dat_%s.RData", Sys.Date()))

