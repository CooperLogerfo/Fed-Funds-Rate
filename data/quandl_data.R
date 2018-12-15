rm(list=ls())

library(quandl)   #Unemployment, GDP, Inflation, T Bonds
library(dplyr)
library(ggplot2)

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


# ------------------ General Economic Data ------------------#
#treasury yields, since 1990
t_yields <- Quandl("USTREASURY/YIELD", api_key="2TpBKV-y2RxET2rG6Z2R")



#function that returns range for all rate hikes
#function takes dataframe of 2 columns. 1st as date, 2nd as value
rate_incr <- function(rate_perc_change){
  val <- rate_perc_change$Value
  date <- rate_perc_change$Date
  
  #instantiate the return df. Will consist of dates where range begins and ends
  return_df <- as.data.frame(cbind(start_m = 1, start_y = 1, 
                                   end_m = 1, end_y = 2))
  i = 1
  while(!is.na(val[i])){
    
    #value is positive, so increase fed funds rate
    if( val[i] > 0){
      #get the month and year for where ffr is first positive
      start_month = as.numeric(unlist(strsplit(date[i], "-"))[2])
      start_year = as.numeric(unlist(strsplit(date[i], "-"))[1])
      
      #advance loop to where val is  no longer positive, streak is over
      while( val[i] > 0 & (!is.na(val[i])) ){
        i = i + 1
      }
      #broke out of loop, no longer streak of consecutive increases
      end_month = as.numeric(unlist(strsplit(date[i-1], "-"))[2])
      end_year = as.numeric(unlist(strsplit(date[i-1], "-"))[1])
      
      if(end_year == start_year){
        if(end_month - start_month > 3){
          #rate hike streak lasted at least 1 financial quarter, store it
          #otherwise, throw it out. not long enough to be considered a streak
          temp_df <- as.data.frame(cbind(start_m = start_month, start_y = start_year,
                                         end_m = end_month, end_y = end_year))
          return_df <- rbind(return_df, temp_df)
        }
      }
      
      else if(end_year > start_year){
        #account for streak that overlaps a year
        if( (12 - start_month) + end_month > 3){
          temp_df <- as.data.frame(cbind(start_m = start_month, start_y = start_year,
                                         end_m = end_month, end_y = end_year))
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
