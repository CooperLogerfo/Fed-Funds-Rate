# ATTRIBUTION: Proper attribution requires clear indication of the data source as "www.macrotrends.net".
# A "dofollow" backlink to the originating page is also required if the data is displayed on a web page.


#mv ~/.rstudio-desktop ~/backup-rstudio-desktop

rm(list=ls())


library(quantmod) #historic S&P data
library(quandl)   #Unemployment, GDP, Inflation, T Bonds
library(dplyr)
library(ggplot2)


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

#here is the monthly percent change in the fed funds rate dating back to 2000.
#In some months the change is 0, others effectively 0, and others relatively substantial.
delta_monthly <- (subset_end$`raw_data$value` - subset_start$`raw_data$value`)/subset_start$`raw_data$value`

View(delta_monthly)

getSymbols("^GSPC", src = "yahoo", from = '1950-01-01')
chartSeries(GSPC)
perc_change <- Delt(GSPC$GSPC.Open, GSPC$GSPC.Close)
summary(perc_change$Delt.0.arithmetic)
#export to RDS, use in sepearte R file

ggplot(subset, aes(x = x_index,  y = `raw_data$value`)) + geom_point()
ggplot(subset, aes(x = x_index,  y = `raw_data$value`)) + geom_line()


#"character" vectors
GSPC$year <- format(index(GSPC),"%y")
GSPC$month <- format(index(GSPC),"%m")
GSPC$day <- format(index(GSPC),"%d")

#quarterly percent change in short term unemployment
unem <- Quandl("FRED/NROUST", api_key="2TpBKV-y2RxET2rG6Z2R", 
               transform="rdiff", collapse="quarterly")

