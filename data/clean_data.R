# ATTRIBUTION: Proper attribution requires clear indication of the data source as "www.macrotrends.net".
# A "dofollow" backlink to the originating page is also required if the data is displayed on a web page.


#mv ~/.rstudio-desktop ~/backup-rstudio-desktop

rm(list=ls())


library(quantmod)
library(dplyr)

raw_data <- read.csv("FF_data.csv")


date_matrix <- read.table(text = as.character(raw_data$date), sep = "/", 
                          colClasses = "numeric", 
                          col.names = c("month", "day", "year"))


data <- cbind(date_matrix, raw_data$value)

subset <- data[16621:23474,]
subset_end <- subset
subset_start <- subset


subset_end <- 
  subset_end %>% 
  group_by(year, month) %>%
  slice(which.max(day))

subset_start <-
  subset_start %>% 
  group_by(year, month) %>%
  slice(which.min(day))

getSymbols("^GSPC", src = "yahoo", from = '1950-01-01')
chartSeries(GSPC)
perc_change <- Delt(GSPC$GSPC.Open, GSPC$GSPC.Close)
summary(perc_change$Delt.0.arithmetic)
#export to RDS, use in sepearte R file