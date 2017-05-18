#This script web-scrapes and then saves information about beer ratings from
# ratebeer

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
# web-scrape ratebeer

RB_add = 'https://www.ratebeer.com'
minimal_raters = 100#the minimum amount of raters to see a ratings as valuable
RB_dat = data.frame(super_style = character(),
                sub_style = character(),
                beer_name = character(),
                brewery = character(),
                ABV = double(),
                rating = double(),
                raters = integer())

#get all beers by looping through brewers organised by alphabet
for (x in c('1', letters)){#for each dictionary entry

    RB_web = web_pre(paste(RB_add, '/browsebrewers-', x, '.htm', sep = ""), ssl_verification = F)#starting page to all beers: the brewer page
    link_nodes = getNodeSet(RB_web, '//a')
    
    for (y in seq(1, length(link_nodes))){#for each link
      if (!is.na(pmatch('/brewers', as.character(xmlAttrs(link_nodes[[y]]))))){#if link points to this substring indicating a brewery
        
        print(paste(RB_add, as.character(xmlAttrs(link_nodes[[y]])), sep = ""))
        
        RB_web = web_pre(paste(RB_add, as.character(xmlAttrs(link_nodes[[y]])), sep = ""), ssl_verification = F)#starting page to all beers: the brewer page
        
        brewery_name = as.character(xmlToDataFrame(getNodeSet(RB_web, '//h1'))$text)
        
        RB_table <- readHTMLTable(RB_web, stringsAsFactors = FALSE)
        
        if(max(as.numeric(RB_table$`brewer-beer-table`$`#`)) >= minimal_raters){#if at least one rating with sufficient raters
          
          RB_table_cut = RB_table$`brewer-beer-table`[as.numeric(RB_table$`brewer-beer-table`[,'#']) >= minimal_raters,]
          
          RB_dat <- rbind(RB_dat, data.frame(
            super_style = rep(NA, nrow(RB_table_cut)),#gets added later
            sub_style = unlist(lapply(RB_table_cut$Name, function(x) strsplit(x, '% ')[[1]][2])),#
            beer_name = unlist(lapply(RB_table_cut$Name, function(x) strsplit(x, '  ')[[1]][1])),
            brewery = rep(brewery_name, nrow(RB_table_cut)),
            ABV = unlist(lapply(RB_table_cut[,2], function(x) suppressWarnings(as.numeric(x)))),
            rating = unlist(lapply(RB_table_cut[,5], function(x) suppressWarnings(as.numeric(x)))),#apparently a weighted average
            raters = unlist(lapply(RB_table_cut[,7], function(x) suppressWarnings(as.numeric(x))))
          ))
        }
      }
    }
}

#add super style
RB_web = web_pre(paste(RB_add, '/beerstyles/', sep = ""), ssl_verification = F)#load beer styles page
for (super_style in c(16, 17, 18, 19, 21, 22, 23, 24)){
  x = unlist(xmlToList(getNodeSet(RB_web, '//div')[[super_style]]))#list of links and names of sub-styles of this super-style
  RB_dat[RB_dat[,'sub_style'] %in% x, 'super_style'] = as.character(x['h3'])
}

###################################################################################################
# save data
save(RB_dat, file=sprintf("RB_dat_%s.RData", Sys.Date()))