# Project
The aim of the project was to find the best beers based on online beer ratings. The following beer rating websites were web-scraped using R:
- [rate beer](http://ratebeer.com/)
- [beer advocate](http://beeradvocate.com/)
- [untappd](http://untappd.com/)

# Data acquisition
Data acquisition R-scripts were split by scraped website:
- [rate beer script](https://github.com/rikunert/beer_rating/blob/master/data_acquisition_ratebeer.R)
- [beer advocate script](https://github.com/rikunert/beer_rating/blob/master/data_acquisition_beeradvocate.R)
- [untappd script](https://github.com/rikunert/beer_rating/blob/master/data_acquisition_untappd.R)

Please note that while the scripts scraping ratebeer.com and beeradvocate.com attempt to sample every beer in the respective data bases, this was, unfortunately, not possible for untappd.com. Instead, the untappd script relies on beer data sampled from beer advocate.

# Data analysis and visualisation
Data visualisation used only the two biggest websites (beer advocate and untappd) and ignored ratebeer.com. The visualisation library I used was [plotly](http://plot.ly/~rikunert) where all resulting figures are also deposited. I called the plotly API using R with [this script](https://github.com/rikunert/beer_rating/blob/master/beer_rating_interactive_visualisations.R). An earlier attempt to visualise the data using ggplot2 was abandoned but the script can still be found [here](https://github.com/rikunert/beer_rating/blob/master/beer_rating_non-interactive_visualisations.R).

# Publication
The findings were published on my data science blog Rich Data [here](http://rikunert.com/guide_to_beer).

@rikunert
