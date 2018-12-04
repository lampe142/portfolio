setwd('/home/rstudio/portfolio_aws')
source('send_email_profileRep.R')

gmailr::mime() %>%
  gmailr::to("maxlampe@posteo.de") %>%
  gmailr::from("lampe142@googlemail.com")%>%
  gmailr::subject(paste('Portfolio Performance Max', Sys.Date()))%>%
  gmailr::html_body("attached is my portfolio performance")%>%
  gmailr::attach_file('Portfolio_Performance_Risk.html') %>%
 gmailr::send_message()

gmailr::mime() %>%
  gmailr::to("maxlampe@posteo.de") %>%
  gmailr::from("lampe142@googlemail.com")%>%
  gmailr::subject(paste('Portfolio Risk Max', Sys.Date()))%>%
  gmailr::html_body("attached is my portfolio")%>%
  gmailr::attach_file('Portfolio_Risk.html') %>%
  gmailr::send_message()