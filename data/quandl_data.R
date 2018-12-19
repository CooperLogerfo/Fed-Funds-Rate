rm(list=ls())


# ---------------------------------------------------------------------#
# ----------------------------- PACKAGES ------------------------------#
# ---------------------------------------------------------------------#
library(quandl)   #Unemployment, GDP, Inflation, T Bonds
library(quantmod) #historic S&P data
library(dplyr)
library(ggplot2)
library(lubridate)


# ---------------------------------------------------------------------#
# ------------------------------ DATA ---------------------------------#
# ---------------------------------------------------------------------#


# ------------------ Fed Funds Rate Data ------------------#

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
  

# ------------------ General Economic/Financial Data ------------------#
#treasury yields, since 1990
t_yields <- Quandl("USTREASURY/YIELD", api_key="2TpBKV-y2RxET2rG6Z2R")

#clean column names so they don't include numbers
colnames(t_yields) <-  c("Date", "one_m", "two_m", "three_m", "six_m", 
                         "one_y", "two_y", "three_y", "five_y",
                         "seven_y", "ten_y", "twenty_y", "thirty_y",
                         "year", "month" )

#unemployment rate, since 1948
unem <- Quandl("FRED/UNRATE", api_key="2TpBKV-y2RxET2rG6Z2R")

#s&p percent change, daily, since 1950
getSymbols("^GSPC", src = "yahoo", from = '1950-01-01')
chartSeries(GSPC)
GSPC$delta <- Delt(GSPC$GSPC.Open, GSPC$GSPC.Close)
dd <- data.frame(Date=index(GSPC), coredata(GSPC))

plot(GSPC$delta)

# subset_end <- 
#   subset_end %>% 
#   group_by(year, month) %>%
#   slice(which.max(day))
# 
# t_yields <- 
#   t_yields %>%
#   mutate(year = year(Date), month = month(Date))
# 
# monthly_avg <-
#   t_yields %>%
#   group_by(year, month) %>%
#   summarize(m_avg = mean())




# ---------------------------------------------------------------------#
# ---------------------------- FUNCTIONS ------------------------------#
# ---------------------------------------------------------------------#
  

# ------------------ Identifies Ranges for Rate Hike Cycles ------------------#
# ------------- "Cycle" defined as period of at least 3 months ------------------#
rate_incr <- function(rate_perc_change){
  val <- rate_perc_change[, "Value"]
  date <- rate_perc_change[, "Date"]
  
  #instantiate the return df. Will consist of dates where range begins and ends
  return_df <- cbind.data.frame(start = as.Date("9999-01-01"),
                                end = as.Date("9999-01-01"))
  
  i = 1
  while(!is.na(val[i])){
    
    #value is positive, so increase fed funds rate
    if( val[i] >= 0){
      
      #get the month and year for where ffr is first positive
      start_date = date[i]
      start_month = as.numeric(month(date[i]))
      start_year = as.numeric(year(date[i]))
      
      #advance loop to where val is  no longer positive, streak is over
      while( val[i] >= 0 & (!is.na(val[i])) ){
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
    
  #increment while loop
  i = i + 1
  }
  
  #return df that consists of 2 columns, start and end dates for rate hike cycles.
  return(return_df[-1,])
}


# ---------------Subsets data based on date range ------------------#
#------"data" must have a "Date" column that is of Date class.------#
subset_data <- function(data, date_range){
  years_range <- data[ data[,"Date"] > date_range[1] & data[,"Date"] < date_range[2] ,]
  return(years_range)
}


# ----- Identifies if sp500 rose or fell that day -----#
identify_up_days <- function(data, date_range){
  subset <- subset_data(data, date_range)
  delta <- subset[ , "delta"]
  
  return(up <- ifelse(delta > 0, 1, 0))
}


# ----- Gets Number of Work Days in Date Range -----#
# --------technically just returns num rows--------#
get_total_days <- function(data, date_range){
  subset <- subset_data(data, date_range)
  num_days <- dim(years_range)[1]
  return(num_days)
}


# ----- Returns Percent of Days that sp500 was positive during Rate-Hike Cycles -----#
percent_up_days_RHC <- function(GSPC, cycles){
  percent_positive <- rep( 0, length(dim(cycles)[1]) )
  for( i in 1:dim(cycles)[1] ){
    count <- sum( identify_up_days(GSPC, c(cycles[i, 1], cycles[i, 2])) )
    percent_positive[i] <- count/get_total_days( GSPC, c(cycles[i, 1], cycles[i, 2]) )
  }
  return(cbind(cycles, percent_positive))
}


# ----- Returns Percent of Days that sp500 was positive during non-Rate-Hike Cycles -----#
percent_up_days_non_RHC <- function(GSPC, cycles){
  percent_positive <- rep( 0, length(dim(cycles)[1]) )
  for( i in 1:dim(cycles)[1] ){
    count <- sum( identify_up_days(GSPC, c(cycles[i, 2], cycles[i+1, 1])) )
    percent_positive[i] <- count/get_total_days( GSPC, c(cycles[i, 2], cycles[i+1, 1]) ) 
  }
  return(cbind(cycles, percent_positive))
}


# ----- returns sp500 return (percent change, no dividends) over given period -----#
#"d" argument must be dataframe of Daily sp500 open and close data, with Date column
percent_change_sp <- function(d, date_range){
  begin <- d[ d$Date == date_range[1], ]$GSPC.Open
  end <- d[ d$Date == date_range[2], ]$GSPC.Close
  if( identical(numeric(0), begin) ){
    begin <- d[ d$Date == (date_range[1] - 3) , ]$GSPC.Open
  }
  if( identical(numeric(0), end) ){
    end <- d[ d$Date == (date_range[2] + 3), ]$GSPC.Close
  }
  return( (end - begin)/begin )
}


# ----- function for plotting t-bond yield curves over given period -----#
#"yields" argument must be dataframe of tbond yields with the following columns of data:
#Date (as Date class), 1 month, 3 month, 6 month, 
#1 year, 2 year, 3 year, 5 year, 7 year, 10 year, 30 year bond yields, data reported DAILY
plot_yield_curve <- function(yields, date_range){
  subset <- subset_data(yields, c(date_range[1], date_range[2]) ) %>%
    select(-two_m, -year, -month) %>%
    gather(key = "period", value = "rate", -Date) %>%
    mutate(highlight = ifelse(period == 'two_y' | period == 'three_y' | 
                                period == 'five_y' | period == 'seven_y' | 
                                period == 'ten_y' , T, F)) %>%
    mutate(maturity = c( rep(1/12, length(period)/11 ), rep(3/12, length(period)/11), 
                         rep(6/12, length(period)/11), rep(1, length(period)/11),
                         rep(2, length(period)/11), rep(3, length(period)/11), 
                         rep(5, length(period)/11), rep(7, length(period)/11), 
                         rep(10, length(period)/11 ), rep(20, length(period)/11), 
                         rep(30, length(period)/11)))
  
  plot <- ggplot(subset, aes(maturity, rate, color = highlight )) +
    geom_point() + 
    labs(title = 'Day:{closest_state}', x = "Time to Maturity", y = "Yield") +
    transition_states(subset$Date, transition_length = 11, state_length = 1)
  
  animate(plot, fps = 10, nframes = length(subset$period)/11*2)
}




# ---------------------------------------------------------------------#
# ------------------------- Script and Plots --------------------------#
# ---------------------------------------------------------------------#

#First step is to get periods of time where there were rate-hikes
rate_hike_cycles <- rate_incr(m_delta)


#Check out T-Bonds first

p1 <- ggplot(data = t_yields, aes(x = Date)) +
  geom_line(aes(y = three_m))
p2 <- ggplot(data = t_yields, aes(x = Date)) +
  geom_line(aes(y = two_y))
p3 <- ggplot(data = t_yields, aes(x = Date)) +
  geom_line(aes(y = ten_y))
p4 <- ggplot(data = t_yields, aes(x = Date)) +
  geom_line(aes(y = thirty_y))

grid.arrange(p1, p2, p3, p4)


#Take a look at how 2 year and 5 year yields are converging during the most recent rate-hike cycle

d <- subset_data(t_yields, c(rate_hike_cycles[28,1], rate_hike_cycles[28,2]  ) ) %>%
  select(Date, two_y, five_y) %>%
  gather(key = "period", value = "rate", -Date)

ggplot(data = d, aes(x = Date, y = rate)) +
  geom_line(aes(color = period), size = 1) +
  labs(x = "Rate-Hike Cycle", y = "Yield") +
  theme_minimal()


#Use plot_yield_curve function to produce animated plots 
#displaying movement in the yield curve during rate-hike cycles
plot_yield_curve(t_yields, c(rate_hike_cycles[28,1], rate_hike_cycles[28,2]  ))

#Other good examples showing flattening/inversion: row 23, 21, 20



#during rate-hike cycles, what % of days did the s&p rise v fall
#compare that to the % of rise/fall days in non-rate hike cycles

non_Rate_Hike<- percent_up_days_non_RHC(dd, rate_hike_cycles)
Rate_Hike <- percent_up_days_RHC(dd, rate_hike_cycles)

#Index data for combining the data
non_Rate_Hike$index <- seq(2, 56, by = 2)
Rate_Hike$index <- seq(1, 55, by = 2)

combined_df <- rbind(Rate_Hike, non_Rate_Hike) %>%
  arrange(index)

dates <- Rate_Hike %>%
  gather(start, end, -percent_positive, -index) %>%
  arrange(end)

combined_df$Date <- dates$end

ggplot(combined_df, aes(as.factor(Date), percent_positive)) +
  geom_bar(stat = "identity", aes(fill = (as.numeric(index) %% 2 == 0))) +
  scale_fill_discrete(guide="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#during rate-hike cycles, what % of days did the s&p rise v fall
#compare that to the % of rise/fall days in non-rate hike cycles

percent_change_sp(dd, c(rate_hike_cycles[1,1], rate_hike_cycles[1,2] ))

p_c <- rep( 0 , dim(rate_hike_cycles)[1] )
for( i in 1:dim(rate_hike_cycles)[1]){
  p_c[i] <- percent_change_sp(dd, c(rate_hike_cycles[i,1],rate_hike_cycles[i,2] ))
}

rate_hike_cycles$spPC <- p_c


