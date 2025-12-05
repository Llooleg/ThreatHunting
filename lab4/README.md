# Четвертая практика
HelgiLO@yandex.ru

## Цель работы

1.  Закрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Программное обеспечение Windows 11 Home
2.  Visual Studio
3.  Интерпретатор языка R 4.1

## План

Используя R и среду разработки RstudioIDE, выполнить задания

## Шаги:

1.  Импортируйте данные DNS –
    https://storage.yandexcloud.net/dataset.ctfsec/dns.zip

Ответ: выполнено 2. Добавьте пропущенные данные о структуре данных
(назначении столбцов)

``` r
library(httr)
```

    Warning: package 'httr' was built under R version 4.4.3

``` r
library(jsonlite)
```

    Warning: package 'jsonlite' was built under R version 4.4.3

``` r
library(readr)
```

    Warning: package 'readr' was built under R version 4.4.3

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
log_file = file.path("dns.log")

column_names <- c(
  "timestamp", "uid", "source_ip", "source_port", "destination_ip", 
  "destination_port", "protocol", "transaction_id", "query", "qclass", 
  "qclass_name", "qtype", "qtype_name", "rcode", "rcode_name", 
  "AA", "TC", "RD", "RA", "Z", "answers", "TTLS", "rejected"
)
dns_data <- invisible(read_delim(
  log_file[1],
  delim = "\t",
  col_names = column_names,
  comment = "#",
  na = c("", "NA", "-"),
  trim_ws = TRUE,
  show_col_types = FALSE
)) %>% as_tibble()
```

1.  Преобразуйте данные в столбцах в нужный форма

``` r
dns_data_clean <- dns_data %>%
  mutate(
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    source_port = as.numeric(source_port),
    destination_port = as.numeric(destination_port),
    transaction_id = as.numeric(transaction_id),
    qclass = as.numeric(qclass),
    qtype = as.numeric(qtype),
    rcode = as.numeric(rcode),
  ) %>% as_tibble()

head(dns_data_clean,10)
```

    # A tibble: 10 × 23
       timestamp           uid                source_ip   source_port destination_ip
       <dttm>              <chr>              <chr>             <dbl> <chr>         
     1 2012-03-16 16:30:05 CWGtK431H9XuaTN4fi 192.168.20…       45658 192.168.27.203
     2 2012-03-16 16:30:15 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     3 2012-03-16 16:30:15 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     4 2012-03-16 16:30:16 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     5 2012-03-16 16:30:05 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     6 2012-03-16 16:30:06 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     7 2012-03-16 16:30:07 C36a282Jljz7BsbGH  192.168.20…         137 192.168.202.2…
     8 2012-03-16 16:30:06 ClEZCt3GLkJdtGGmAa 192.168.20…         137 192.168.202.2…
     9 2012-03-16 16:30:07 ClEZCt3GLkJdtGGmAa 192.168.20…         137 192.168.202.2…
    10 2012-03-16 16:30:07 ClEZCt3GLkJdtGGmAa 192.168.20…         137 192.168.202.2…
    # ℹ 18 more variables: destination_port <dbl>, protocol <chr>,
    #   transaction_id <dbl>, query <chr>, qclass <dbl>, qclass_name <chr>,
    #   qtype <dbl>, qtype_name <chr>, rcode <dbl>, rcode_name <chr>, AA <lgl>,
    #   TC <lgl>, RD <lgl>, RA <lgl>, Z <dbl>, answers <chr>, TTLS <chr>,
    #   rejected <lgl>

1.  Просмотрите общую структуру данных с помощью функции glimpse()

``` r
library(dplyr)

dns_data_clean %>% glimpse()
```

    Rows: 427,935
    Columns: 23
    $ timestamp        <dttm> 2012-03-16 16:30:05, 2012-03-16 16:30:15, 2012-03-16…
    $ uid              <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jl…
    $ source_ip        <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76"…
    $ source_port      <dbl> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ destination_ip   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255…
    $ destination_port <dbl> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ protocol         <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp…
    $ transaction_id   <dbl> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 6218…
    $ query            <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ qclass_name      <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET…
    $ qtype            <dbl> 33, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 1…
    $ qtype_name       <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB"…
    $ rcode            <dbl> 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ rcode_name       <chr> "NOERROR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ AA               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    $ TC               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    $ RD               <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
    $ RA               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    $ Z                <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1,…
    $ answers          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ TTLS             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ rejected         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…

1.  Сколько участников информационного обмена в сети Доброй Организации?

``` r
all_unique_ip <- unique(c(dns_data_clean$source_ip, dns_data_clean$destination_ip))
```

1.  Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?

``` r
internal_ips <- all_unique_ip[grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", all_unique_ip)]
external_ips <- all_unique_ip[!grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", all_unique_ip)]
length(internal_ips) / length(external_ips)
```

    [1] 13.77174

1.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.

``` r
dns_data_clean %>% group_by(source_ip) %>% count(sort = TRUE) %>% head(10)
```

    # A tibble: 10 × 2
    # Groups:   source_ip [10]
       source_ip           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

1.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений

``` r
dns_data_clean %>% count(query, sort = TRUE) %>% head(10)
```

    # A tibble: 10 × 2
       query                                                                       n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

1.  Опеределите базовые статистические характеристики (функция summary()
    ) интервала времени между последовательными обращениями к топ-10
    доменам.

``` r
top_domains <- dns_data_clean %>% 
  count(query, sort = TRUE) %>% 
  head(10) %>% 
  pull(query)

top_domains_data <- dns_data_clean %>% 
  filter(query %in% top_domains) %>% 
  arrange(timestamp)

time_intervals <- top_domains_data %>% 
  group_by(query) %>% 
  mutate(time_diff = as.numeric(timestamp - lag(timestamp))) %>% 
  ungroup() %>% 
  filter(!is.na(time_diff)) %>% 
  pull(time_diff)

summary(time_intervals)
```

        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.00     0.75     8.76     1.74 52723.50 

1.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?

``` r
periodic_analysis <- dns_data_clean %>%
  group_by(source_ip, query) %>%
  arrange(timestamp) %>%
  mutate(
    time_diff = as.numeric(timestamp - lag(timestamp)),
    request_count = n()
  ) %>%
  filter(request_count >= 5) %>%
  filter(!is.na(time_diff)) %>%
  summarise(
    request_count = first(request_count),
    mean_interval = mean(time_diff),
    sd_interval = sd(time_diff),
    cv_interval = sd_interval / mean_interval, 
    min_interval = min(time_diff),
    max_interval = max(time_diff),
    regularity_score = 1 / (1 + cv_interval)
  ) %>%
  ungroup() %>%
  filter(cv_interval < 0.5, 
         mean_interval >= 30,
         mean_interval <= 3600) %>%
  arrange(regularity_score)
```

    `summarise()` has grouped output by 'source_ip'. You can override using the
    `.groups` argument.

``` r
suspicious_ips <- periodic_analysis %>%
  group_by(source_ip) %>%
  summarise(
    domains_targeted = n_distinct(query),
    avg_regularity = mean(regularity_score),
    total_requests = sum(request_count)
  ) %>%
  filter(domains_targeted >= 1) %>%
  arrange(desc(avg_regularity))
```

1.  Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние сервисы,
    например http://ip-api.com (API-эндпоинт – http://ip-api.com/json).

``` r
print("Наиболее подозрительные IP-адреса:")
```

    [1] "Наиболее подозрительные IP-адреса:"

``` r
print(suspicious_ips, n = 20)
```

    # A tibble: 9 × 4
      source_ip       domains_targeted avg_regularity total_requests
      <chr>                      <int>          <dbl>          <int>
    1 192.168.202.65                 1          1.000              5
    2 10.10.117.209                  1          0.912              5
    3 192.168.202.84                 1          0.775             84
    4 192.168.202.80                 1          0.753             92
    5 192.168.203.45                 1          0.747             19
    6 192.168.202.145                4          0.742             20
    7 192.168.202.92                 2          0.735             23
    8 192.168.202.88                 2          0.725             16
    9 192.168.24.25                  2          0.714             10

``` r
head(suspicious_ips)
```

    # A tibble: 6 × 4
      source_ip       domains_targeted avg_regularity total_requests
      <chr>                      <int>          <dbl>          <int>
    1 192.168.202.65                 1          1.000              5
    2 10.10.117.209                  1          0.912              5
    3 192.168.202.84                 1          0.775             84
    4 192.168.202.80                 1          0.753             92
    5 192.168.203.45                 1          0.747             19
    6 192.168.202.145                4          0.742             20

``` r
top_10_domains <- dns_data_clean%>%count(query, sort = TRUE) %>%
  as_tibble() %>% head(10)
top_10_domains
```

    # A tibble: 10 × 2
       query                                                                       n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

``` r
top_domains <- dns_data_clean %>%
  filter(!is.na(query)) %>%
  count(query, sort = TRUE) %>%
  head(10)
get_geo_info <- function(ip) {
  if (is.na(ip) || ip == "") {
     return(tibble(
      ip_address = NA_character_,
      country = "IP не определён",
      city = "IP не определён",
      isp = "IP не определён"
    ))
  }
  if (grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", ip)) {
    return(tibble(
      ip_address = ip,
      country = "Частный IP",
      city = "Частный IP",
      isp = "Частный IP"
    ))
  }
  url <- paste0("http://ip-api.com/json/", ip)
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    if (data$status == "success") {
      return(tibble(
        ip_address = ip,
        country = data$country,
        city = data$city,
        isp = data$isp
      ))
    } else {
      return(tibble(
        ip_address = ip,
        country = paste("API ошибка:", data$status),
        city = paste("API ошибка:", data$status),
        isp = paste("API ошибка:", data$status)
      ))
    }
  } else {
    return(tibble(
      ip_address = ip,
      country = "Ошибка API",
      city = "Ошибка API",
      isp = "Ошибка API"
    ))
  }
}

dns_with_dest_ip <- dns_data_clean %>%
  filter(!is.na(destination_ip)) %>%
  select(query, destination_ip) %>%
  distinct()
relevant_dns <- dns_with_dest_ip %>%
  filter(query %in% top_10_domains$query)
geo_results_df <- tibble(
  ip_address = character(),
  country = character(),
  city = character(),
  isp = character()
)
#Запросы к API
unique_ips_to_check <- unique(relevant_dns$destination_ip)
for (ip in unique_ips_to_check) {
  geo_info_row <- get_geo_info(ip)
  geo_results_df <- bind_rows(geo_results_df, geo_info_row)
}
domain_geo_info_final <- relevant_dns %>%
  left_join(geo_results_df, by = c("destination_ip" = "ip_address")) %>%
  rename(ip_address = destination_ip) %>%
  select(domain = query, ip_address, country, city, isp)
domain_order_factor <- factor(domain_geo_info_final$domain, levels = top_10_domains$query)
domain_geo_info_final_sorted <- domain_geo_info_final %>%
  mutate(domain_order = domain_order_factor) %>%
  arrange(domain_order) %>%
  select(-domain_order)

# Вывод результата, группируя по домену
print(domain_geo_info_final_sorted)
```

    # A tibble: 1,213 × 5
       domain                    ip_address       country       city       isp      
       <chr>                     <chr>            <chr>         <chr>      <chr>    
     1 teredo.ipv6.microsoft.com fec0:0:0:ffff::2 Switzerland   Morat      Internet…
     2 teredo.ipv6.microsoft.com fec0:0:0:ffff::1 Switzerland   Morat      Internet…
     3 teredo.ipv6.microsoft.com fec0:0:0:ffff::3 Switzerland   Morat      Internet…
     4 teredo.ipv6.microsoft.com 192.168.207.4    Частный IP    Частный IP Частный …
     5 teredo.ipv6.microsoft.com 192.168.0.1      Частный IP    Частный IP Частный …
     6 tools.google.com          192.168.207.4    Частный IP    Частный IP Частный …
     7 tools.google.com          192.168.206.44   Частный IP    Частный IP Частный …
     8 tools.google.com          156.154.70.22    United States New York   Neustar …
     9 tools.google.com          8.26.56.26       United States Clifton    Flexenti…
    10 tools.google.com          68.87.75.198     United States Pittsburgh Comcast …
    # ℹ 1,203 more rows

## Оценка результата

В результате лабораторной работы мы ознакомились с продвинутыми
техниками обработки данных

## Вывод

Таким образом, мы научились продолжили изучать основы обработки данных в
R.
