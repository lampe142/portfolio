setwd(here::here())
source('E-Mail/send_email_profileRep.R')
# options(httr_oob_default=TRUE) 
# gar_auth(token = ".httr-oauth",new_user = FALSE) 
gmailr::mime() %>%
  gmailr::to("maxlampe@posteo.de") %>%
  gmailr::from("lampe142@googlemail.com")%>%
  gmailr::subject(paste('Portfolio Performance Max', Sys.Date()))%>%
  gmailr::html_body("attached is my portfolio performance")%>%
  gmailr::attach_file('Dashboard/portfolio_performance.html') %>%
 gmailr::send_message()

gmailr::mime() %>%
  gmailr::to("maxlampe@posteo.de") %>%
  gmailr::from("lampe142@googlemail.com")%>%
  gmailr::subject(paste('Portfolio Risk Max', Sys.Date()))%>%
  gmailr::html_body("attached is my portfolio")%>%
  gmailr::attach_file('Dashboard/portfolio_risk.html') %>%
  gmailr::send_message()