# ATTRIBUTION: Proper attribution requires clear indication of the data source as "www.macrotrends.net".
# A "dofollow" backlink to the originating page is also required if the data is displayed on a web page.

rm(list=ls())

raw_data <- read.csv("FF_data.csv")


date_matrix <- read.table(text = as.character(raw_data$date), sep = "/", 
                          colClasses = "numeric", 
                          col.names = c("month", "day", "year"))


data <- cbind(date_matrix, raw_data$value)
