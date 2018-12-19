rm(list=ls())

library(quandl)   #Unemployment, GDP, Inflation, T Bonds
library(dplyr)
library(ggplot2)
library(lubridate)

# ------------------ Fed Funds Rate Data ------------------#
# ------------------ Default: 1950 - 2018 ------------------#

#annual percent change
a_delta <- Quandl("FRED/FEDFUNDS", api_key="2TpBKV-y2RxET2rG6Z2R", 
                  transform="rdiff", collapse="annual")

#quarterly percent change
q_delta <- Quandl("FRED/FEDFUNDS", api_key="2TpBKV-y2RxET2rG6Z2R", 
                  transform="rdiff", collapse="quarterly")

#monthly percent change
m_delta <- Quandl("FRED/FEDFUNDS", api_key="2TpBKV-y2RxET2rG6Z2R", 
                  transform="rdiff", collapse="monthly")

m_delta$Date <- as.character(m_delta$Date)

m_delta <-
  m_delta %>%
  arrange(Date)
  

# ------------------ General Economic Data ------------------#
#treasury yields, since 1990
t_yields <- Quandl("USTREASURY/YIELD", api_key="2TpBKV-y2RxET2rG6Z2R")

subset_end <- 
  subset_end %>% 
  group_by(year, month) %>%
  slice(which.max(day))

t_yields <- 
  t_yields %>%
  mutate(year = year(Date), month = month(Date))

monthly_avg <-
  t_yields %>%
  group_by(year, month) %>%
  summarize(m_avg = mean())
  

#function that returns range for all rate hikes
#function takes dataframe of 2 columns. 1st as date, 2nd as value
rate_incr <- function(rate_perc_change){
  val <- rate_perc_change[, "Value"]
  date <- rate_perc_change[, "Date"]
  
  #instantiate the return df. Will consist of dates where range begins and ends
  return_df <- cbind.data.frame(start = as.Date("9999-01-01"),
                                   end = as.Date("9999-01-01"))
  
  i = 1
  while(!is.na(val[i])){
    
    #value is positive, so increase fed funds rate
    if( val[i] > 0){
      #get the month and year for where ffr is first positive
      
      start_date = date[i]
      start_month = as.numeric(month(date[i]))
      start_year = as.numeric(year(date[i]))
      
      #advance loop to where val is  no longer positive, streak is over
      while( val[i] > 0 & (!is.na(val[i])) ){
        i = i + 1
      }
      #broke out of loop, no longer streak of consecutive increases
      
      end_date = date[i-1]
      end_month = as.numeric(month(date[i-1]))
      end_year = as.numeric(year(date[i-1]))
      
      if(end_year == start_year){
        if(end_month - start_month > 3){
          #rate hike streak lasted at least 1 financial quarter, store it
          #otherwise, throw it out. not long enough to be considered a streak
          temp_df <- cbind.data.frame(start = start_date,
                                      end = end_date)
          return_df <- rbind(return_df, temp_df)
        }
      }
      
      else if(end_year > start_year){
        #account for streak that overlaps a year
        if( (12 - start_month) + end_month > 3){
          temp_df <- cbind.data.frame(start = start_date,
                                         end = end_date)
          return_df <- rbind(return_df, temp_df)
        }
      }
      else{
        #no other valid case
      }
    }
  i = i + 1
  }

  return(return_df)
}

#function that returns range for all rate decreases
rate_decr <- function(rate_perc_change){
  
}


ffr_mag <- function(cumulative_ffr, time_frames){
  cum_sum_vec <- rep(0, length(time_fra))
  for( i in 1:length(time_frames)){
    for( j in 1:length(cumulative_ffr)){
      while( cum_date[j] < time_fr[i]){
        cumsum = cumsum + cum$value
      }
      cum_sum_vec[i] <- cumsum
    }

  }
}




rate_hike_cycles <- rate_incr(m_delta)

colnames(t_yields) <-  c("Date", "one_m", "two_m", "three_m", "six_m", 
                          "one_y", "two_y", "three_y", "five_y",
                          "seven_y", "ten_y", "twenty_y", "thirty_y",
                          "year", "month" )

ggplot(data = t_yields, aes(x = Date, y = ten_y)) +
  geom_line(color = "red", size = 2)


#"data" must have a "Date" column that is of Date class.
subset_data <- function(data, date_range){
  years_range <- t_yields[ year(t_yields[,"Date"]) > date_range[1] & year(t_yields[,"Date"]) < date_range[2] ,]
  return(data)
}


