#Tutorial for RStudio to Github: https://www.youtube.com/watch?v=-c2uNqEE6-c 
#Other Tutorial: https://www.youtube.com/watch?v=VOIQgViCyTo&ab_channel=%EC%A4%80%EB%AC%B8%5C

#Tutorial for AmerifluxR Package - https://github.com/khufkens/amerifluxr 
install.packages(c("rvest","data.table","curl","RCurl","DT","shiny","shinydashboard","leaflet","plotly","devtools"))

require(devtools)
devtools::install_github("khufkens/amerifluxr")
