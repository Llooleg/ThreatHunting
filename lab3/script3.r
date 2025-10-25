library("nycflights13")
library(dplyr)

data(package = "nycflights13")

data_list <- data(package = "nycflights13")$results[, "Item"]

total_rows <- sum(sapply(data_list, function(df_name) {
  nrow(get(df_name)) 
  }))

print(paste("Total rows across all data frames:", total_rows))


data_list <- data(package = "nycflights13")$results[, "Item"]

total_columns <- sum(sapply(data_list, function(df_name) {
  ncol(get(df_name)) 
  }))

print(paste("Total columns across all data frames:", total_columns))

airlines %>%
group_by(carrier) %>%
summarise(count = n()) %>%
nrow()


arrivals_count <- flights %>%
  filter(origin == "JFK", month==5) %>%  
tally()

arrivals_count

airports %>% filter(lat == max(lat)) %>%  select(name, lat)


airports %>% filter(alt == max(alt) ) %>% select(name, alt)

planes %>% select(model, year) %>% arrange(year, na.rm=TRUE)


glimpse(airlines)


weather_data <- weather %>%filter(origin == "JFK", month==9)

print((mean(weather_data$temp) - 32)*5/9)


flights %>% left_join(airlines, join_by(carrier)) %>% filter(month == 6)%>%group_by(name) %>% summarise(amount = n()) %>% arrange(desc(amount)) %>% slice(1) %>% select(name, amount) 

flights %>% left_join(airlines, join_by(carrier)) %>% group_by(name) %>% filter(arr_delay> 0 & year == 2013) %>% summarise(amount = n()) %>% arrange(desc(amount)) %>% slice(1) %>% select(name, amount) 