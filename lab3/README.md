# Третья практика
HelgiLO@yandex.ru

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Программное обеспечение Windows 11 Home
2.  Visual Studio
3.  Интерпретатор языка R 4.1

## План

Используя R и среду разработки RstudioIDE, выполнить задания

## Шаги:

1.  Сколько встроенных в пакет nycflights13 датафреймов

``` r
library("nycflights13")
```

    Warning: package 'nycflights13' was built under R version 4.4.3

``` r
data(package="nycflights13")
```

Ответ: 5 2. Сколько строк в каждом датафрейме??

``` r
data_list <- data(package = "nycflights13")$results[, "Item"]

total_rows <- sum(sapply(data_list, function(df_name) {
  nrow(get(df_name)) 
  }))

print(paste("Total rows across all data frames:", total_rows))
```

    [1] "Total rows across all data frames: 367687"

1.  Сколько столбцов в каждом датафрейме?

``` r
total_columns <- sum(sapply(data_list, function(df_name) {
  ncol(get(df_name)) 
  }))

print(paste("Total columns across all data frames:", total_columns))
```

    [1] "Total columns across all data frames: 53"

1.  Как просмотреть примерный вид датафрейма?

``` r
library(dplyr)
```

    Warning: package 'dplyr' was built under R version 4.4.3


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
planes %>% glimpse()
```

    Rows: 3,322
    Columns: 9
    $ tailnum      <chr> "N10156", "N102UW", "N103US", "N104UW", "N10575", "N105UW…
    $ year         <int> 2004, 1998, 1999, 1999, 2002, 1999, 1999, 1999, 1999, 199…
    $ type         <chr> "Fixed wing multi engine", "Fixed wing multi engine", "Fi…
    $ manufacturer <chr> "EMBRAER", "AIRBUS INDUSTRIE", "AIRBUS INDUSTRIE", "AIRBU…
    $ model        <chr> "EMB-145XR", "A320-214", "A320-214", "A320-214", "EMB-145…
    $ engines      <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    $ seats        <int> 55, 182, 182, 182, 55, 182, 182, 182, 182, 182, 55, 55, 5…
    $ speed        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ engine       <chr> "Turbo-fan", "Turbo-fan", "Turbo-fan", "Turbo-fan", "Turb…

1.  Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных
    (представлено в наборах данных)

``` r
airlines %>%
group_by(carrier) %>%
summarise(count = n()) %>%
nrow()
```

    [1] 16

1.  Сколько рейсов принял аэропорт John F Kennedy Intl в мае

``` r
glimpse(flights)
```

    Rows: 336,776
    Columns: 19
    $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2…
    $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, …
    $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, …
    $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1…
    $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,…
    $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,…
    $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1…
    $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "…
    $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4…
    $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394…
    $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA",…
    $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",…
    $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 1…
    $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, …
    $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6…
    $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0…
    $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 0…

``` r
arrivals_count <- flights %>%
  filter(origin == "JFK", month==5) %>%  
tally()

arrivals_count
```

    # A tibble: 1 × 1
          n
      <int>
    1  9397

1.  Какой самый северный аэропорт?

``` r
airports %>% filter(lat == max(lat)) %>%  select(name, lat)
```

    # A tibble: 1 × 2
      name                      lat
      <chr>                   <dbl>
    1 Dillant Hopkins Airport  72.3

1.  Какой аэропорт самый высокогорный (находится выше всех над уровнем
    моря)?

``` r
airports %>% filter(alt == max(alt) ) %>% select(name, alt)
```

    # A tibble: 1 × 2
      name        alt
      <chr>     <dbl>
    1 Telluride  9078

1.  Какие бортовые номера у самых старых самолетов?

``` r
planes %>% select(model, year) %>% arrange(year, na.rm=TRUE)
```

    # A tibble: 3,322 × 2
       model        year
       <chr>       <int>
     1 DC-7BF       1956
     2 150          1959
     3 OTTER DHC-3  1959
     4 172E         1963
     5 210-5(205)   1963
     6 737-524      1965
     7 65-A90       1967
     8 PA-28-180    1968
     9 E-90         1972
    10 310Q         1973
    # ℹ 3,312 more rows

1.  Какая средняя температура воздуха была в сентябре в аэропорту John F
    Kennedy Intl (в градусах Цельсия)

<!-- -->

1.  Какие бортовые номера у самых старых самолетов?

``` r
planes %>% select(model, year) %>% arrange(year, na.rm=TRUE)
```

    # A tibble: 3,322 × 2
       model        year
       <chr>       <int>
     1 DC-7BF       1956
     2 150          1959
     3 OTTER DHC-3  1959
     4 172E         1963
     5 210-5(205)   1963
     6 737-524      1965
     7 65-A90       1967
     8 PA-28-180    1968
     9 E-90         1972
    10 310Q         1973
    # ℹ 3,312 more rows

1.  Какая средняя температура воздуха была в сентябре в аэропорту John F
    Kennedy Intl (в градусах Цельсия).

``` r
weather_data <- weather %>%filter(origin == "JFK", month==9)
print((mean(weather_data$temp) - 32)*5/9)
```

    [1] 19.38764

1.  Самолеты какой авиакомпании совершили больше всего вылетов в июне?

``` r
flights %>% left_join(airlines, join_by(carrier)) %>% filter(month == 6)%>%group_by(name) %>% summarise(amount = n()) %>% arrange(desc(amount)) %>% slice(1) %>% select(name, amount) 
```

    # A tibble: 1 × 2
      name                  amount
      <chr>                  <int>
    1 United Air Lines Inc.   4975

1.  Самолеты какой авиакомпании задерживались чаще других в 2013 году?

``` r
flights %>% left_join(airlines, join_by(carrier)) %>% group_by(name) %>% filter(arr_delay> 0 & year == 2013) %>% summarise(amount = n()) %>% arrange(desc(amount)) %>% slice(1) %>% select(name, amount) 
```

    # A tibble: 1 × 2
      name                     amount
      <chr>                     <int>
    1 ExpressJet Airlines Inc.  24484

## Оценка результата

В результате лабораторной работы мы ознакомились с dplyr и nycflights13

## Вывод

Таким образом, мы научились основам dplyr
