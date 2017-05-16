#This script web-scrapes and then saves information about beer ratings from
# beeradvocate

#(c) Richard Kunert 
#For questions please e-mail RiKunert at gmail dot com

###################################################################################################
# load libraries

if(!require(RCurl)){install.packages('RCurl')} #html-tree parsing
library(RCurl)
options(RCurlOptions = list(useragent = "zzzz"))

if(!require(XML)){install.packages('XML')} #html-tree parsing
library(XML)

###################################################################################################
# custom functions

#web_pre takes a webpage of address web_add and pre-processes it
#input: website address
#output: parsed html tree

web_pre = function(web_add, ssl_verification = T){
  
  web_nod <- getURL(web_add, .opts = list(ssl.verifypeer = ssl_verification))#load the webpage
  
  #sometimes error called 'transfer closed with outstanding read data remaining'
  #probably due to suboptimal internet connection.
  
  web_nod <- readLines(tc <- textConnection(web_nod))# Process escape characters
  close(tc)
  
  tree <- htmlTreeParse(web_nod, error=function(...){}, useInternalNodes = TRUE)# Parse the html tree, ignoring errors on the page
  
  return(tree)#the parsed html tree is returned
}

###################################################################################################
# web-scrape beer-advocate

BA_add = 'https://www.beeradvocate.com'
minimal_raters = 100#the minimum amount of raters to see a ratings as valuable
BA_dat = data.frame(super_style = character(),
                sub_style = character(),
                beer_name = character(),
                brewery = character(),
                ABV = double(),
                rating = double(),
                raters = integer())

#get all beers by looping through beer styles page
BA_web = web_pre('https://www.beeradvocate.com/beer/style/', ssl_verification = F)#starting page to all beers: the style page
hrefFun <- function(x){xpathSApply(x,'./a',xmlAttrs)}#extracting links
BA_table <- readHTMLTable(BA_web, elFun = hrefFun, stringsAsFactors = FALSE)#read table while extracting links
superstyles = c('ale', 'lager', 'hybrid')

for (s in seq(1, length(superstyles))){#for each super style
  
  print(paste('><>', superstyles[s], ""))
  
  for (t in seq(1, length(BA_table[s + 1]$`NULL`))){#for each substyle
    
    BA_web = web_pre(paste(BA_add, BA_table[s + 1]$`NULL`[t], '?sort=revsD&start=', 1, sep = ""), ssl_verification = F)#
    sub_style_t = as.character(xmlToDataFrame(getNodeSet(BA_web, '//h1'))$text)
    
    print(paste('><><><>', sub_style_t, ""))
    
    start = 1#starting page: the 50 beers with the most reviews
    minimal_ratings = T
    while (minimal_ratings){#while there is a minimal amount of raters contributing to each beer
      
      print(paste('><><><><><>', paste(BA_add, BA_table[s + 1]$`NULL`[t], '?sort=revsD&start=', toString(start), sep = ""), ""))
      
      BA_web = web_pre(paste(BA_add, BA_table[s + 1]$`NULL`[t], '?sort=revsD&start=', toString(start), sep = ""), ssl_verification = F)#starting page to all beers: the style page
      BA_style_table <- readHTMLTable(BA_web, stringsAsFactors = FALSE)
      
      i = length(BA_dat$super_style) + 1
      j = i + nrow(BA_style_table$`NULL`) - 4
      
      BA_dat <- rbind(BA_dat, data.frame(
        super_style = rep(superstyles[s], j - i + 1),
        sub_style = rep(sub_style_t, j - i + 1),
        beer_name = BA_style_table$`NULL`[3 :  (nrow(BA_style_table$`NULL`) - 1), 1],
        brewery = BA_style_table$`NULL`[3 :  (nrow(BA_style_table$`NULL`) - 1), 2],
        ABV = suppressWarnings(as.numeric(BA_style_table$`NULL`[3 :  (nrow(BA_style_table$`NULL`) - 1), 3])),#suppress warning when handling '?' representing no entry
        rating = suppressWarnings(as.double(BA_style_table$`NULL`[3 :  (nrow(BA_style_table$`NULL`) - 1), 4])),
        raters = suppressWarnings(as.numeric(gsub(",", "", BA_style_table$`NULL`[3 :  (nrow(BA_style_table$`NULL`) - 1), 5])))
      ))
      
      start = start + 50#go to next 50 beers and start all over
      
      minimal_ratings = min(BA_dat$raters[i : j]) >= minimal_raters#break loop in case not enough people rated these beers anymore
      
    }
  }
}

###################################################################################################
# save data

save(BA_dat, file=sprintf("BA_dat_%s.RData", Sys.Date()))