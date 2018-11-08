gmailr::mime() %>%
  gmailr::to("maxlampe@posteo.de") %>%
  gmailr::from("lampe142@googlemail.com")%>%
  gmailr::subject(paste('Portfolio Max', Sys.Date()))%>%
  gmailr::html_body("attached is my portfolio")%>%
  gmailr::attach_file('Portfolio_Performance_Risk.html') %>%
 gmailr::send_message()