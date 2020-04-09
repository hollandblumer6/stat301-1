# Final Project R Script ---------------------------------------------------------
# Name: Holland Blumer 
# Due Date: December 8, 2019 

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(stringi)


# Data Import -------------------------------------------------------------
nasdaq_2017_by_day <- read.csv('unprocessed data/^IXIC2017.csv')
storm_events_2017_by_month <- read.csv('unprocessed data/StormEvents_locations-ftp_v1.0_d2017_c20190817 copy.csv')
nasdaq_2017_every_day <- read.csv('unprocessed data/^IXIC_all_days_2017.csv') 
nasdaq_2017_by_month <- read.csv('unprocessed data/^IXIC_monthly_2017.csv')
sp500_by_month_2017 <- read.csv('unprocessed data/SP500_2017.csv') 


# Tidy Data ---------------------------------------------------------------

# First, I want to get a glimpse of the data to decide which variables I want to compare
glimpse(nasdaq_2017_by_day)
glimpse(storm_events_2017_by_month)

# In the NASDAQ DATA, I want to check if close and adj_close have the same values
nasdaq_2017_by_day %>%
  filter(Close != Adj.Close)
# They do have the same values 

# I also want to compare the open and close prices
nasdaq_2017_by_day %>%
  filter(Close != Open)
# There are 80 days where the prices are different, I will take this into consideration

# Next, I want to make a tibble with the variables I might compare 
nasdaq_2017_by_day_tibble <- tibble(
  open = nasdaq_2017_by_day$Open,
  high = nasdaq_2017_by_day$High,
  low = nasdaq_2017_by_day$Low,
  close = nasdaq_2017_by_day$Close,
  date_2017_nasdaq = nasdaq_2017_by_day$Date)

# It's difficult for me to visualize the trends in the tibble, so I will make a graph 
nasdaq_2017_by_day_tibble %>%
  ggplot(aes(date_2017_nasdaq, close, group = 1)) +
  geom_line() +
  geom_smooth(method = "lm") +
  xlab("Date in 2017") +
  ylab("Closing Price") +
  ggtitle("NASDAQ Closing Prices in 2017") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4)) 

# I want to plot the open and close prices on the same graph 
nasdaq_2017_plot <- nasdaq_2017_by_day_tibble %>%
  ggplot(aes(date_2017_nasdaq, group = 1)) +
  geom_line(aes(y=close, group = 1), color="red") +  
  geom_line(aes(y=open, group = 1), color="green") + 
  xlab("Date in 2017") +
  ylab("Price") +
  ggtitle("NASDAQ Prices in 2017") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 4)) 
nasdaq_2017_plot
 
# I also want to pull the variables from the storm data. The comparable variables are the date and intensity of the storm. 
storm_events_2017_by_month_tibble <- tibble(
  date_unformatted = storm_events_2017_by_month$YEARMONTH,
  range = storm_events_2017_by_month$RANGE)

# I also want to plot the storm data 
storm_events_2017_by_month_plot <- storm_events_2017_by_month_tibble %>%
  ggplot(aes(x = date_unformatted)) +
  geom_point(aes(y=range), color="green") +
  xlab("Unformatted Dates") +
  ylab("Range") +
  ggtitle("Storm Events in 2017 (Unformatted)")
storm_events_2017_by_month_plot

# From the plot, I realized that the data I found is grouped by month/year instead of month/day/year. To make it more comparable to the NASDAQ data, I will take the mean range for each month. 
filter_jan_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201701)
  mean_jan_range <- mean(filter_jan_storms_2017$range)

filter_feb_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201702)
  mean_feb_range <- mean(filter_feb_storms_2017$range)

filter_mar_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201703)
  mean_mar_range <- mean(filter_mar_storms_2017$range)
  
filter_apr_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201704)
  mean_apr_range <- mean(filter_apr_storms_2017$range)
  
filter_may_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201705)
  mean_may_range <- mean(filter_may_storms_2017$range)
  
filter_jun_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201706)
  mean_jun_range <- mean(filter_jun_storms_2017$range)

filter_jul_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201707)
  mean_jul_range <- mean(filter_jul_storms_2017$range)
  
filter_aug_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201708)
  mean_aug_range <- mean(filter_aug_storms_2017$range)
  
filter_sep_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201709)
  mean_sep_range <- mean(filter_sep_storms_2017$range)
  
filter_oct_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201710)
  mean_oct_range <- mean(filter_oct_storms_2017$range)

filter_nov_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201711)
  mean_nov_range <- mean(filter_nov_storms_2017$range)
  
filter_dec_storms_2017 <- filter(storm_events_2017_by_month_tibble, date_unformatted == 201712)
  mean_dec_range <- mean(filter_dec_storms_2017$range)


# The dates are formatted strangely in the file, so I will reformat them manually and then assign them to the mean ranges per month in a new tibble. 
month_year_weather_2017 <- c("01/2017","02/2017","03/2017","04/2017","05/2017","06/2017","07/2017","08/2017","09/2017","10/2017","11/2017","12/2017")
mean_range_values <- c(mean_jan_range, mean_feb_range, mean_mar_range, mean_apr_range, mean_may_range, mean_jun_range, mean_jul_range, mean_aug_range, mean_sep_range, mean_oct_range, mean_nov_range, mean_dec_range)
tidy_range_tibble <- tibble(month_year_weather_2017, mean_range_values)


# I want to plot the tidied weather data
tidy_range_plot <- tidy_range_tibble %>%
  ggplot(aes(month_year_weather_2017, mean_range_values, group = 1)) +
  geom_line() +
  geom_smooth(method = "lm") +
  xlab("Month in 2017") +
  ylab("Range") +
  ggtitle("Average Range of Storms Each Month in 2017") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) 
tidy_range_plot

# Let's compare the two plots for the weather data
grid.arrange(storm_events_2017_by_month_plot, tidy_range_plot, nrow=2)

# Comparing the Storm Data and Nasdaq Data
grid.arrange(nasdaq_2017_plot, tidy_range_plot, nrow=2)

# I realized that I should've plotted the data for the whole year
nasdaq_2017_every_day <- read.csv('unprocessed data/^IXIC_all_days_2017.csv') 

# Better than that, I can dowload the data for each month. It's important to know that the date is the first of every month. 
nasdaq_2017_by_month <- read.csv('unprocessed data/^IXIC_monthly_2017.csv')


nasdaq_2017_by_month_tibble <- tibble(
  date_nasdaq_month = nasdaq_2017_by_month$Date,
  close_nasdaq_month = nasdaq_2017_by_month$Close,
  open_nasdaq_month = nasdaq_2017_by_month$Open)

#Let's plot nasdaq_2017_by_month_tibble
nasdaq_2017_by_month_plot <- nasdaq_2017_by_month_tibble %>%
  ggplot(aes(date_nasdaq_month, group = 1)) +
  geom_line(aes(y=close_nasdaq_month, group = 1), color = "blue") +
  geom_line(aes(y=open_nasdaq_month, group = 1), color = "green") +
  xlab("Month in 2017") +
  ylab("Price") +
  ggtitle("Price of Nasdaq per Month in 2017") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) 
nasdaq_2017_by_month_plot 


# I am going to add the tibbles together
weather_nasdaq_2017_combined <- merge(nasdaq_2017_by_month_tibble, tidy_range_tibble, by = 0, all = TRUE, sort = FALSE)


# I want to add the weather data tibble to the monthly Nasdaq prices tibble and store it in the processed file.
write.csv(weather_nasdaq_2017_combined, "processed data/weather_nasdaq_2017_combined.csv")


# Next I want to plot the open and close prices with the range, to spot any trend I multiplied the mean values by 2500
weather_nasdaq_2017_combined_plot <- weather_nasdaq_2017_combined %>%
  ggplot(aes(date_nasdaq_month, group = 1)) +
  geom_line(aes(y=close_nasdaq_month, group = 1), color="blue") +
  geom_line(aes(y=open_nasdaq_month, group = 1), color="pink") +
  geom_line(aes(y=mean_range_values*2500, group = 1), color="green") +
  xlab("Month in 2017") +
  ylab("Trend Values") +
  ggtitle("Comparing Nasdaq Open and Close prices with Storm Range") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, size = 8)) 
weather_nasdaq_2017_combined_plot

# Perhaps it's more ethical to plot them side by side instead of on the same grid.
grid.arrange(tidy_range_plot, nasdaq_2017_by_month_plot, nrow = 2)


# It's still hard to see if the price increased or decreased or if the storm range increased or decreased each month, I can check this using a while loop
df_nasdaq_close_monthly = data.frame(nasdaq_2017_by_month_tibble$close_nasdaq_month)
df_nasdaq_open_monthly = data.frame(nasdaq_2017_by_month_tibble$open_nasdaq_month)
range_weather_monthly = data.frame(weather_nasdaq_2017_combined$mean_range_values)

i = 1
while (i<12){
for (value in df_nasdaq_close_monthly) {
  if(value[i] <= value[i+1]){ 
    print("Increase")
    i = i + 1
  }else{
    print("Decrease")
    i = i + 1
  }
}
}
# The data is mainly increasing, now I will analyze the open price data per month

j = 1
while (j<12){
  for (value in df_nasdaq_open_monthly) {
    if(value[j] <= value[j+1]){ 
      print("Increase")
      j = j + 1
    }else{
      print("Decrease")
      j = j + 1
    }
  }
}
# There is little fluctuation here too. This makes sense because the open and close price data are similar.

k = 1
while (k<12){
  for (value in range_weather_monthly) {
    if(value[k] <= value[k+1]){ 
      print("Increase")
      k = k + 1
    }else{
      print("Decrease")
      k = k + 1
    }
  }
}

#The range fluctuates a lot every month. It's hard to spot a trend. Perhaps the SP500 matches more. I need to import this data. 

sp500_by_month_2017 <- read.csv('unprocessed data/SP500_2017.csv') 

#I'm assuming this data looks similar to the Nasdaq data, but I want to make sure. 
glimpse(nasdaq_2017_by_day)

#I'm curious to see if close and adj.close are the same here
sp500_by_month_2017 %>%
  filter(Close != Adj.Close)
#They are!


#Next, I'm going to make a tibble for this 
sp500_by_month_2017_tibble <- tibble(
  date_sp500_month = sp500_by_month_2017$Date,
  open_sp500_month = sp500_by_month_2017$Open,
  close_sp500_month = sp500_by_month_2017$Close)

#I want to compare the means of the open and close prices, because I think they are too similar to include both 
(1 - (mean(sp500_by_month_2017_tibble$open_sp500_month)/mean(sp500_by_month_2017_tibble$close_sp500_month)))*100

# It seems like there is ~1% difference, so I'm gonna compare the close prices only 
  
sp500_2017_by_month_plot <- sp500_by_month_2017_tibble %>%
  ggplot(aes(date_sp500_month, close_sp500_month, group = 1)) +
  geom_line(color="blue") +
  xlab("Month in 2017") +
  ylab("Price") +
  ggtitle("Closing S&P 500 Prices in 2017") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) 
sp500_2017_by_month_plot

# Let's compare the plots side by side
grid.arrange(tidy_range_plot, nasdaq_2017_by_month_plot, sp500_2017_by_month_plot, nrow=3)

# I want to make one more loop to see the fluctuation in prices per month
df_sp500_close_monthly = data.frame(sp500_by_month_2017_tibble$close_sp500_month)
h = 1
while (h<12){
  for (value in df_sp500_close_monthly) {
    if(value[h] <= value[h+1]){ 
      print("Increase")
      h = h + 1
    }else{
      print("Decrease")
      h = h + 1
    }
  }
}

# There is not much fluctuation here. 

