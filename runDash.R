setwd('/home/rstudio/portfolio')
library(rmarkdown)

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
rmarkdown::render("Dashboard/dash_performance.Rmd",output_file="portfolio_performance.html")
# browseURL("Dashboard/portfolio_performance.html")
