setwd('/home/rstudio/portfolio_aws')
library(rmarkdown)

Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
rmarkdown::render("Dashboard/dash_performance.Rmd",output_file="portfolio_performance.html")
# browseURL("Dashboard/portfolio_performance.html")

a <- 1
save('a', file = paste0('testa ',Sys.time(),'.RData'))
