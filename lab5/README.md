# Пятая практика
HelgiLO@yandex.ru

## Цель работы

Получить знания о методах исследования радиоэлектронной обстановки.
Составить представление о механизмах работы Wi-Fi сетей на канальном и
сетевом уровне модели OSI. Закрепить практические навыки использования
языка программирования R для обработки данных Закрепить знания основных
функций обработки данных экосистемы tidyverse языка R

## Исходные данные

1.  Программное обеспечение Windows 11 Home
2.  Visual Studio
3.  Интерпретатор языка R 4.1

## План

Используя R и среду разработки RstudioIDE, выполнить задания

## Шаги:

1.  Импорт данных

``` r
library(readr)
```

    Warning: package 'readr' was built under R version 4.4.3

``` r
library(stringr)
```

    Warning: package 'stringr' was built under R version 4.4.3

``` r
library("fpc")
```

    Warning: package 'fpc' was built under R version 4.4.3

``` r
library("mclust")
```

    Warning: package 'mclust' was built under R version 4.4.3

    Package 'mclust' version 6.1.2
    Type 'citation("mclust")' for citing this R package in publications.

``` r
library("httr") 
```

    Warning: package 'httr' was built under R version 4.4.3

``` r
library("V8") 
```

    Warning: package 'V8' was built under R version 4.4.3

    Using V8 engine 11.9.169.6

``` r
library(dplyr)
```

    Warning: package 'dplyr' was built under R version 4.4.3


    Attaching package: 'dplyr'

    The following object is masked from 'package:mclust':

        count

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    Warning: package 'tidyr' was built under R version 4.4.3

``` r
library(fpc)
library(janitor)
```

    Warning: package 'janitor' was built under R version 4.4.3


    Attaching package: 'janitor'

    The following objects are masked from 'package:stats':

        chisq.test, fisher.test

``` r
library("R.utils") 
```

    Warning: package 'R.utils' was built under R version 4.4.3

    Loading required package: R.oo

    Loading required package: R.methodsS3

    R.methodsS3 v1.8.2 (2022-06-13 22:00:14 UTC) successfully loaded. See ?R.methodsS3 for help.

    R.oo v1.26.0 (2024-01-24 05:12:50 UTC) successfully loaded. See ?R.oo for help.


    Attaching package: 'R.oo'

    The following object is masked from 'package:R.methodsS3':

        throw

    The following objects are masked from 'package:methods':

        getClasses, getMethods

    The following objects are masked from 'package:base':

        attach, detach, load, save

    R.utils v2.13.0 (2025-02-24 21:20:02 UTC) successfully loaded. See ?R.utils for help.


    Attaching package: 'R.utils'

    The following object is masked from 'package:tidyr':

        extract

    The following object is masked from 'package:utils':

        timestamp

    The following objects are masked from 'package:base':

        cat, commandArgs, getOption, isOpen, nullfile, parse, use, warnings

``` r
library("jsonlite") 
```

    Warning: package 'jsonlite' was built under R version 4.4.3


    Attaching package: 'jsonlite'

    The following object is masked from 'package:R.utils':

        validate

``` r
library("igraph") 
```

    Warning: package 'igraph' was built under R version 4.4.3


    Attaching package: 'igraph'

    The following object is masked from 'package:R.oo':

        hierarchy

    The following object is masked from 'package:tidyr':

        crossing

    The following objects are masked from 'package:dplyr':

        as_data_frame, groups, union

    The following objects are masked from 'package:stats':

        decompose, spectrum

    The following object is masked from 'package:base':

        union

``` r
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.4.3

    Warning: package 'ggplot2' was built under R version 4.4.3

    Warning: package 'tibble' was built under R version 4.4.3

    Warning: package 'forcats' was built under R version 4.4.3

    Warning: package 'lubridate' was built under R version 4.4.3

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.1     ✔ purrr     1.0.2
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ lubridate::%--%()       masks igraph::%--%()
    ✖ tibble::as_data_frame() masks igraph::as_data_frame(), dplyr::as_data_frame()
    ✖ purrr::compose()        masks igraph::compose()
    ✖ dplyr::count()          masks mclust::count()
    ✖ igraph::crossing()      masks tidyr::crossing()
    ✖ R.utils::extract()      masks tidyr::extract()
    ✖ dplyr::filter()         masks stats::filter()
    ✖ purrr::flatten()        masks jsonlite::flatten()
    ✖ dplyr::lag()            masks stats::lag()
    ✖ purrr::map()            masks mclust::map()
    ✖ purrr::simplify()       masks igraph::simplify()
    ✖ jsonlite::validate()    masks R.utils::validate()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
filename <- "P2_wifi_data.csv"
url <- "https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv"
if (!file.exists(filename)) {
  download.file(url, destfile = filename, mode = "wb")
}
raw_text <- read_lines(filename)
station_header <- "Station MAC, First time seen, Last time seen, Power, # packets, BSSID, Probed ESSIDs"
header_station_idx <- str_which(raw_text, paste0("^", station_header, "$"))
if (length(header_station_idx) == 0) {
  header_station_idx <- str_which(raw_text, "^Station MAC,")

  if (length(header_station_idx) == 0) {
    stop()
  }
}
all_empty_before_station <- str_which(raw_text[1:(header_station_idx - 1)], "^\\s*$")
if (length(all_empty_before_station) == 0) {
  stop()
}
separator_idx <- all_empty_before_station[length(all_empty_before_station)]
num_lines_ap <- separator_idx - 3
if (num_lines_ap > 0) {
  ap_col_types <- cols_only(
    "BSSID" = col_character(),
    "First time seen" = col_character(),
    "Last time seen" = col_character(),
    "channel" = col_number(),
    "Speed" = col_number(),
    "Privacy" = col_character(),
    "Cipher" = col_character(),
    "Authentication" = col_character(),
    "Power" = col_number(),
    "# beacons" = col_number(),
    "# IV" = col_number(),
    "LAN IP" = col_character(),
    "ID-length" = col_number(),
    "ESSID" = col_character(),
    "Key" = col_character()
  )
  wifi_ap_data <- read_csv(filename, n_max = num_lines_ap,
                           col_types = ap_col_types,
                           show_col_types = FALSE)
} else {
    stop()
}
skip_lines_station <- header_station_idx - 1
station_col_types <- cols_only(
  "Station MAC" = col_character(),
  "First time seen" = col_character(),
  "Last time seen" = col_character(),
  "Power" = col_number(),
  "# packets" = col_number(),
  "BSSID" = col_character(),
  "Probed ESSIDs" = col_character()
)
wifi_station_data <- read_csv(filename, skip = skip_lines_station,
                              col_types = station_col_types,
                              show_col_types = FALSE)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

1.  Приведение даннных к виду “аккуратных”

``` r
process_wifi_df <- function(df) {
  df %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), str_trim)) %>%
    mutate(across(contains("time_seen"), lubridate::as_datetime, tz = "UTC")) 
}

wifi_ap_data      <- process_wifi_df(wifi_ap_data)
```

    Warning: There was 1 warning in `mutate()`.
    ℹ In argument: `across(contains("time_seen"), lubridate::as_datetime, tz =
      "UTC")`.
    Caused by warning:
    ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    Supply arguments directly to `.fns` through an anonymous function instead.

      # Previously
      across(a:b, mean, na.rm = TRUE)

      # Now
      across(a:b, \(x) mean(x, na.rm = TRUE))

``` r
wifi_station_data <- process_wifi_df(wifi_station_data)
glimpse(wifi_station_data)
```

    Rows: 12,081
    Columns: 7
    $ station_mac     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ first_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ last_time_seen  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ power           <dbl> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45,…
    $ number_packets  <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7…
    $ bssid           <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:…
    $ probed_essi_ds  <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

``` r
glimpse(wifi_ap_data)
```

    Rows: 167
    Columns: 15
    $ bssid           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ first_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ last_time_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ channel         <dbl> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ speed           <dbl> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ privacy         <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2",…
    $ cipher          <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "C…
    $ authentication  <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "…
    $ power           <dbl> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ number_beacons  <dbl> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ number_iv       <dbl> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ lan_ip          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.…
    $ id_length       <dbl> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ essid           <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "M…
    $ key             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

1.  Просмотр

``` r
glimpse(wifi_ap_data)
```

    Rows: 167
    Columns: 15
    $ bssid           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ first_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ last_time_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ channel         <dbl> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ speed           <dbl> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ privacy         <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2",…
    $ cipher          <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "C…
    $ authentication  <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "…
    $ power           <dbl> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ number_beacons  <dbl> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ number_iv       <dbl> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ lan_ip          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.…
    $ id_length       <dbl> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ essid           <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "M…
    $ key             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

1.  Определить небезопасные точки доступа (без шифрования – OPN)

``` r
unsafe_wifi <- wifi_ap_data %>%
  filter(privacy == "OPN")
```

1.  Определить производителя для каждого обнаруженного устройства

``` r
get_manufacturer_by_mac <- function(mac_address, timeout = 10) {
  url <- paste0("https://www.macvendorlookup.com/api/v2/", mac_address)
  tryCatch({
    response <- httr::GET(
      url,
      httr::timeout(timeout),
      httr::add_headers(
        "Mozilla/5.0 (Windows NT 11.0; Win64; x64"
      )
    )
    if (httr::status_code(response) != 200) {
      return(NA)
    }
    
    content <- httr::content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)
    if (length(data) == 0 || is.null(data$company) || data$company == "") {
      return(NA)
    }
    return(data$company[1])
    
  }, error = function(e) {
    return(NA)
  })
}

wifi_ap_data$manufacturer <- sapply(wifi_ap_data$bssid, function(mac) {
  result <- get_manufacturer_by_mac(mac)
  if (is.null(result)) {
    return(NA)
  } else {
    return(result)
  }
})



final_ap <- wifi_ap_data %>%
  select(bssid, essid, manufacturer)

print(head(final_ap, 10))
```

    # A tibble: 10 × 3
       bssid             essid                    manufacturer           
       <chr>             <chr>                    <chr>                  
     1 BE:F1:71:D5:17:8B C322U13 3965             <NA>                   
     2 6E:C7:EC:16:DA:1A Cnet                     <NA>                   
     3 9A:75:A8:B9:04:1E KC                       <NA>                   
     4 4A:EC:1E:DB:BF:95 POCO X5 Pro 5G           <NA>                   
     5 D2:6D:52:61:51:5D <NA>                     <NA>                   
     6 E8:28:C1:DC:B2:52 MIREA_HOTSPOT            Eltex Enterprise Ltd.  
     7 BE:F1:71:D6:10:D7 C322U21 0566             <NA>                   
     8 0A:C5:E1:DB:17:7B AndroidAP177B            <NA>                   
     9 38:1A:52:0D:84:D7 EBFCD57F-EE81fI_DL_1AO2T Seiko Epson Corporation
    10 BE:F1:71:D5:0E:53 C322U06 9080             <NA>                   

1.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах

``` r
wpa3_aps <- wifi_ap_data %>%
  filter(grepl("WPA3", authentication) | grepl("WPA3", privacy))
print(wpa3_aps %>% select (bssid,privacy,essid, cipher ))
```

    # A tibble: 8 × 4
      bssid             privacy   essid                                       cipher
      <chr>             <chr>     <chr>                                       <chr> 
    1 26:20:53:0C:98:E8 WPA3 WPA2  <NA>                                       CCMP  
    2 A2:FE:FF:B8:9B:C9 WPA3 WPA2 "Christie’s"                                CCMP  
    3 96:FF:FC:91:EF:64 WPA3 WPA2  <NA>                                       CCMP  
    4 CE:48:E7:86:4E:33 WPA3 WPA2 "iPhone (Анастасия)"                        CCMP  
    5 8E:1F:94:96:DA:FD WPA3 WPA2 "iPhone (Анастасия)"                        CCMP  
    6 BE:FD:EF:18:92:44 WPA3 WPA2 "Димасик"                                   CCMP  
    7 3A:DA:00:F9:0C:02 WPA3 WPA2 "iPhone XS Max \U0001f98a\U0001f431\U0001f… CCMP  
    8 76:C5:A0:70:08:96 WPA3 WPA2  <NA>                                       CCMP  

1.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию

``` r
clean_duration <- wifi_ap_data %>%
  arrange(bssid, first_time_seen) %>%
  group_by(bssid) %>%
  mutate(
    gap = as.numeric(first_time_seen - lag(last_time_seen, default = first_time_seen[1]), units = "secs"),
    new_session = if_else(gap > 3000, 1, 0),
    session_id = cumsum(new_session)
  ) %>%
  group_by(bssid, session_id) %>%
  summarise(
    start = min(first_time_seen),
    end = max(last_time_seen),
    .groups = "drop"
  ) %>%
  mutate(duration = as.numeric(end - start, units = "secs")) %>%
  group_by(bssid) %>%
  summarise(total_time_on_channel_seconds = sum(duration)) %>%
  arrange(desc(total_time_on_channel_seconds))

clean_duration
```

    # A tibble: 167 × 2
       bssid             total_time_on_channel_seconds
       <chr>                                     <dbl>
     1 00:25:00:FF:94:73                          9795
     2 E8:28:C1:DD:04:52                          9776
     3 E8:28:C1:DC:B2:52                          9755
     4 08:3A:2F:56:35:FE                          9746
     5 6E:C7:EC:16:DA:1A                          9729
     6 E8:28:C1:DC:B2:50                          9726
     7 48:5B:39:F9:7A:48                          9725
     8 E8:28:C1:DC:B2:51                          9725
     9 E8:28:C1:DC:FF:F2                          9724
    10 8E:55:4A:85:5B:01                          9723
    # ℹ 157 more rows

1.  Обнаружить топ-10 самых быстрых точек доступа

``` r
top_fastest_points <- wifi_ap_data %>%
  filter(!is.na(speed)) %>%
  arrange(desc(speed)) %>%
  slice_head(n = 10)
print(select(top_fastest_points, speed, bssid, essid))
```

    # A tibble: 10 × 3
       speed bssid             essid             
       <dbl> <chr>             <chr>             
     1   866 26:20:53:0C:98:E8 <NA>              
     2   866 96:FF:FC:91:EF:64 <NA>              
     3   866 CE:48:E7:86:4E:33 iPhone (Анастасия)
     4   866 8E:1F:94:96:DA:FD iPhone (Анастасия)
     5   360 9A:75:A8:B9:04:1E KC                
     6   360 4A:EC:1E:DB:BF:95 POCO X5 Pro 5G    
     7   360 56:C5:2B:9F:84:90 OnePlus 6T        
     8   360 E8:28:C1:DC:B2:41 MIREA_GUESTS      
     9   360 E8:28:C1:DC:B2:40 MIREA_HOTSPOT     
    10   360 E8:28:C1:DC:B2:42 <NA>              

1.  Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию

``` r
print(wifi_ap_data %>%
  mutate(
    life_time = as.numeric(last_time_seen - first_time_seen, units = "secs"),
    beacons_per_minute = number_beacons / life_time
  ) %>%
  filter(life_time > 0) %>%
  arrange(desc(beacons_per_minute)) %>%
  select(essid, bssid, beacons_per_minute, number_beacons, life_time, 
         first_time_seen, last_time_seen) %>%
  head(10))
```

    # A tibble: 10 × 7
       essid   bssid beacons_per_minute number_beacons life_time first_time_seen    
       <chr>   <chr>              <dbl>          <dbl>     <dbl> <dttm>             
     1 "iPhon… F2:3…              0.857              6         7 2023-07-28 10:27:02
     2 "Михаи… B2:C…              0.8                4         5 2023-07-28 10:40:54
     3 "iPhon… 3A:D…              0.556              5         9 2023-07-28 10:27:01
     4 "MT_FR… 02:B…              0.5                1         2 2023-07-28 09:24:46
     5 "MT_FR… 00:3…              0.5                1         2 2023-07-28 10:34:03
     6  <NA>   76:C…              0.5                1         2 2023-07-28 11:16:36
     7 "Саня"  D2:2…              0.385              5        13 2023-07-28 09:45:29
     8 "C322U… BE:F…              0.174           1647      9461 2023-07-28 09:13:03
     9 "MT_FR… 00:0…              0.167              1         6 2023-07-28 10:29:13
    10 "EBFCD… 38:1…              0.163            704      4319 2023-07-28 09:13:03
    # ℹ 1 more variable: last_time_seen <dttm>

1.  Определить производителя для каждого обнаруженного устройства

``` r
parse_oui_file <- function(file_path) {
  cat(" Читаем OUI файл...\n")
  

  lines <- readLines(file_path, warn = FALSE)
  
  oui_pattern <- "^([0-9A-F]{2}-[0-9A-F]{2}-[0-9A-F]{2})\\s+\\(hex\\)\\s+(.+)$"
  
  oui_data <- tibble(
    line = lines
  ) %>%
    filter(grepl(oui_pattern, line)) %>%
    mutate(
      mac_prefix = gsub(oui_pattern, "\\1", line),
      manufacturer = trimws(gsub(oui_pattern, "\\2", line))
    ) %>%
    select(mac_prefix, manufacturer) %>%

    mutate(mac_prefix = tolower(gsub("-", ":", mac_prefix)))
  
  cat(" Загружено производителей:", nrow(oui_data), "\n")
  return(oui_data)
}


get_manufacturer_from_oui <- function(mac_address, oui_lookup) {
 
  mac_prefix <- tolower(substr(gsub("[^0-9A-Fa-f:]", "", mac_address), 1, 8))
  
  result <- oui_lookup %>%
    filter(mac_prefix == !!mac_prefix) %>%
    pull(manufacturer)
  
  if (length(result) > 0) {
    return(result[1])
  } else {
    return(NA)
  }
}

oui_file <- "oui.txt"  

if (file.exists(oui_file)) {
  cat("\nИспользуем локальный OUI файл\n")
  oui_lookup <- parse_oui_file(oui_file)
} else {
  
  
  cat(" Пытаемся скачать автоматически...\n")
  download.file(
    "https://standards-oui.ieee.org/oui/oui.txt",
    destfile = oui_file,
    quiet = FALSE
  )
  oui_lookup <- parse_oui_file(oui_file)
}
```


    Используем локальный OUI файл
     Читаем OUI файл...
     Загружено производителей: 38548 

``` r
unique_stations <- wifi_station_data %>%
  filter(!is.na(station_mac) & station_mac != "") %>%
  distinct(station_mac)

cat("Всего уникальных станций:", nrow(unique_stations), "\n")
```

    Всего уникальных станций: 12081 

``` r
station_manufacturers <- unique_stations %>%
  mutate(
    manufacturer = sapply(station_mac, function(mac) {
      get_manufacturer_from_oui(mac, oui_lookup)
    })
  )


cat("Успешно определено:", sum(!is.na(station_manufacturers$manufacturer)), 
    "из", nrow(station_manufacturers), "\n")
```

    Успешно определено: 214 из 12081 

``` r
wifi_station_data_with_manufacturer <- wifi_station_data %>%
  left_join(station_manufacturers, by = "station_mac")

final_station_tibble <- wifi_station_data_with_manufacturer %>%
  group_by(station_mac, manufacturer) %>%
  summarise(
    bssid = first(bssid),
    avg_power = mean(power, na.rm = TRUE),
    total_packets = sum(number_packets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_packets))

print(head(final_station_tibble, 20))
```

    # A tibble: 20 × 5
       station_mac       manufacturer                  bssid avg_power total_packets
       <chr>             <chr>                         <chr>     <dbl>         <dbl>
     1 00:95:69:E7:7D:21 LSD Science and Technology C… (not…       -33          8171
     2 00:95:69:E7:7C:ED LSD Science and Technology C… (not…       -55          4096
     3 00:95:69:E7:7F:35 LSD Science and Technology C… (not…       -69          2245
     4 98:F6:21:72:9E:D6 Xiaomi Communications Co Ltd  E8:2…       -59          2143
     5 F6:4D:98:98:18:C3 <NA>                          00:2…       -61          1062
     6 52:FE:C5:8B:DF:D3 <NA>                          8E:5…       -57          1037
     7 C0:E4:34:D8:E7:E5 AzureWave Technology Inc.     BE:F…       -61           958
     8 74:DF:BF:7B:00:19 Liteon Technology Corporation E8:2…       -65           911
     9 50:3E:AA:33:52:EC TP-LINK TECHNOLOGIES CO.,LTD. (not…       -53           862
    10 CA:66:3B:8F:56:DD <NA>                          BE:F…       -33           858
    11 14:13:33:59:9F:AB AzureWave Technology Inc.     (not…       -57           849
    12 0A:AB:49:39:BB:29 <NA>                          E8:2…       -61           801
    13 04:8C:9A:0B:40:EA Huawei Device Co., Ltd.       E8:2…       -73           756
    14 D2:29:A5:2C:7E:50 <NA>                          8E:5…       -45           701
    15 BC:F1:71:D5:0E:53 Intel Corporate               (not…       -35           675
    16 BC:F1:71:D5:17:8B Intel Corporate               (not…       -29           667
    17 BC:F1:71:D5:48:00 Intel Corporate               (not…       -37           635
    18 BC:F1:71:D5:B3:5D Intel Corporate               (not…       -61           617
    19 BC:F1:71:D6:10:D7 Intel Corporate               (not…       -31           606
    20 34:E1:2D:3C:C8:2D Intel Corporate               6E:C…       -59           580

``` r
manufacturer_stats <- final_station_tibble %>%
  filter(!is.na(manufacturer)) %>%
  group_by(manufacturer) %>%
  summarise(
    device_count = n(),
    total_packets = sum(total_packets),
    .groups = "drop"
  ) %>%
  arrange(desc(device_count))

print(head(manufacturer_stats, 10))
```

    # A tibble: 10 × 3
       manufacturer                         device_count total_packets
       <chr>                                       <int>         <dbl>
     1 Intel Corporate                               126         20741
     2 Xiaomi Communications Co Ltd                   16          2981
     3 Samsung Electronics Co.,Ltd                     9           205
     4 AzureWave Technology Inc.                       8          2146
     5 CHONGQING FUGUI ELECTRONICS CO.,LTD.            5           936
     6 HUAWEI TECHNOLOGIES CO.,LTD                     5           598
     7 Liteon Technology Corporation                   5          1248
     8 TP-LINK TECHNOLOGIES CO.,LTD.                   5          1861
     9 Apple, Inc.                                     4           481
    10 Seiko Epson Corporation                         4             5

``` r
output_file <- "station_manufacturers.csv"
write_csv(final_station_tibble, output_file)
```

1.  Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
  ap_strong <- wifi_ap_data %>% 
  filter(!grepl("^.[2367abefABEF]", `Station MAC`))
```

    # A tibble: 10 × 16
       bssid    first_time_seen     last_time_seen      channel speed privacy cipher
       <chr>    <dttm>              <dttm>                <dbl> <dbl> <chr>   <chr> 
     1 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:50       1   195 WPA2    CCMP  
     2 6E:C7:E… 2023-07-28 09:13:03 2023-07-28 11:55:12       1   130 WPA2    CCMP  
     3 9A:75:A… 2023-07-28 09:13:03 2023-07-28 11:53:31       1   360 WPA2    CCMP  
     4 4A:EC:1… 2023-07-28 09:13:03 2023-07-28 11:04:01       7   360 WPA2    CCMP  
     5 D2:6D:5… 2023-07-28 09:13:03 2023-07-28 10:30:19       6   130 WPA2    CCMP  
     6 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
     7 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:44      11   195 WPA2    CCMP  
     8 0A:C5:E… 2023-07-28 09:13:03 2023-07-28 11:36:31      11   130 WPA2    CCMP  
     9 38:1A:5… 2023-07-28 09:13:03 2023-07-28 10:25:02      11   130 WPA2    CCMP  
    10 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 10:29:21       1   195 WPA2    CCMP  
    # ℹ 9 more variables: authentication <chr>, power <dbl>, number_beacons <dbl>,
    #   number_iv <dbl>, lan_ip <chr>, id_length <dbl>, essid <chr>, key <chr>,
    #   manufacturer <chr>

1.  Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.

``` r
station_ap_matrix <- wifi_station_data %>%
  filter(!is.na(station_mac) & !is.na(bssid) & bssid != "(not associated)") %>%
  group_by(station_mac, bssid) %>%
  summarise(packets = sum(number_packets, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = bssid,
    values_from = packets,
    values_fill = 0
  )

station_ids <- station_ap_matrix$station_mac

station_matrix <- station_ap_matrix %>%
  select(-station_mac) %>%
  as.matrix()

rownames(station_matrix) <- station_ids

cat("Количество станций:", nrow(station_matrix), "\n")
```

    Количество станций: 186 

``` r
cat("Количество уникальных BSSID:", ncol(station_matrix), "\n")
```

    Количество уникальных BSSID: 74 

``` r
station_matrix_filtered <- station_matrix[rowSums(station_matrix) > 0, ]

cat("Количество станций:", nrow(station_matrix_filtered), "\n")
```

    Количество станций: 186 

``` r
if (nrow(station_matrix_filtered) >= 10) {
  max_k <- min(10, nrow(station_matrix_filtered) - 1)
  
  silhouette_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    set.seed(123)
    kmeans_result <- kmeans(station_matrix_filtered, centers = k, nstart = 25)
    
    dist_matrix <- dist(station_matrix_filtered)
    sil <- cluster::silhouette(kmeans_result$cluster, dist_matrix)
    silhouette_scores[k - 1] <- mean(sil[, 3])
  }
  
  optimal_k <- which.max(silhouette_scores) + 1
  
  
  cat("Силуэтные коэффициенты для k от 2 до", max_k, ":\n")
  for (i in 1:length(silhouette_scores)) {
    cat("k =", i + 1, ": ", round(silhouette_scores[i], 4), "\n")
  }
  cat("\nОптимальное количество кластеров:", optimal_k, "\n")
  
} else {
  optimal_k <- min(3, nrow(station_matrix_filtered) - 1)
  cat("\n=== Слишком мало данных ===\n")
  cat("Используем k =", optimal_k, "\n")
}
```

    Силуэтные коэффициенты для k от 2 до 10 :
    k = 2 :  0.9141 
    k = 3 :  0.8362 
    k = 4 :  0.8419 
    k = 5 :  0.845 
    k = 6 :  0.8008 
    k = 7 :  0.8111 
    k = 8 :  0.8159 
    k = 9 :  0.8052 
    k = 10 :  0.7851 

    Оптимальное количество кластеров: 2 

``` r
set.seed(420)
final_clusters <- kmeans(station_matrix_filtered, centers = optimal_k, nstart = 25)

cat(table(final_clusters$cluster))
```

    1 185

``` r
clustered_stations <- tibble(
  station_mac = rownames(station_matrix_filtered),
  cluster = final_clusters$cluster
)

wifi_station_clustered <- wifi_station_data %>%
  left_join(clustered_stations, by = "station_mac")

cat("\n=== Примеры станций по кластерам ===\n")
```


    === Примеры станций по кластерам ===

``` r
for (i in 1:optimal_k) {
  cat("\nКластер", i, ":\n")
  cluster_stations <- clustered_stations %>% filter(cluster == i)
  print(head(cluster_stations, 5))
}
```


    Кластер 1 :
    # A tibble: 1 × 2
      station_mac       cluster
      <chr>               <int>
    1 98:F6:21:72:9E:D6       1

    Кластер 2 :
    # A tibble: 5 × 2
      station_mac       cluster
      <chr>               <int>
    1 00:04:35:22:4F:75       2
    2 00:E9:3A:67:93:E9       2
    3 00:F4:8D:F7:C5:19       2
    4 02:01:5B:41:E9:B3       2
    5 02:69:A5:29:F1:3E       2

``` r
cluster_characteristics <- wifi_station_clustered %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    num_stations = n_distinct(station_mac),
    avg_power = mean(power, na.rm = TRUE),
    avg_packets = mean(number_packets, na.rm = TRUE),
    num_unique_bssids = n_distinct(bssid[bssid != "(not associated)"]),
    .groups = 'drop'
  )



cluster_characteristics
```

    # A tibble: 2 × 5
      cluster num_stations avg_power avg_packets num_unique_bssids
        <int>        <int>     <dbl>       <dbl>             <int>
    1       1            1     -59        2143                   1
    2       2          185     -61.5        95.2                74

1.  Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер

``` r
power_check <- wifi_station_clustered %>%
  filter(!is.na(cluster) & !is.na(power)) %>%
  group_by(cluster, station_mac) %>%
  summarise(
    n_records = n(),
    .groups = 'drop'
  )

cat("Записей на станцию: min =", min(power_check$n_records), 
    ", max =", max(power_check$n_records), 
    ", median =", median(power_check$n_records), "\n")
```

    Записей на станцию: min = 1 , max = 1 , median = 1 

``` r
signal_stability_between_stations <- wifi_station_clustered %>%
  filter(!is.na(cluster) & !is.na(power)) %>%
  group_by(cluster) %>%
  summarise(
    num_stations = n_distinct(station_mac),
    num_records = n(),
    mean_power = mean(power, na.rm = TRUE),
    sd_power = sd(power, na.rm = TRUE),
    min_power = min(power, na.rm = TRUE),
    max_power = max(power, na.rm = TRUE),
    range_power = max_power - min_power,
    q25_power = quantile(power, 0.25, na.rm = TRUE),
    q75_power = quantile(power, 0.75, na.rm = TRUE),
    iqr_power = IQR(power, na.rm = TRUE),
    cv_power = sd_power / abs(mean_power),
    .groups = 'drop'
  ) %>%
  arrange(sd_power)

cat("\n=== Стабильность сигнала между станциями в кластерах ===\n")
```


    === Стабильность сигнала между станциями в кластерах ===

``` r
print(signal_stability_between_stations)
```

    # A tibble: 2 × 12
      cluster num_stations num_records mean_power sd_power min_power max_power
        <int>        <int>       <int>      <dbl>    <dbl>     <dbl>     <dbl>
    1       2          185         185      -61.5     19.7       -88        -1
    2       1            1           1      -59       NA         -59       -59
    # ℹ 5 more variables: range_power <dbl>, q25_power <dbl>, q75_power <dbl>,
    #   iqr_power <dbl>, cv_power <dbl>

``` r
station_avg_power <- wifi_station_clustered %>%
  filter(!is.na(cluster) & !is.na(power)) %>%
  group_by(cluster, station_mac) %>%
  summarise(
    avg_power = mean(power, na.rm = TRUE),
    n_records = n(),
    .groups = 'drop'
  )

cluster_stability_by_avg <- station_avg_power %>%
  group_by(cluster) %>%
  summarise(
    num_stations = n(),
    mean_of_avg_power = mean(avg_power, na.rm = TRUE),
    sd_of_avg_power = sd(avg_power, na.rm = TRUE),
    min_avg_power = min(avg_power, na.rm = TRUE),
    max_avg_power = max(avg_power, na.rm = TRUE),
    range_avg_power = max_avg_power - min_avg_power,
    .groups = 'drop'
  ) %>%
  arrange(sd_of_avg_power)

print(cluster_stability_by_avg)
```

    # A tibble: 2 × 7
      cluster num_stations mean_of_avg_power sd_of_avg_power min_avg_power
        <int>        <int>             <dbl>           <dbl>         <dbl>
    1       2          185             -61.5            19.7           -88
    2       1            1             -59              NA             -59
    # ℹ 2 more variables: max_avg_power <dbl>, range_avg_power <dbl>

``` r
most_stable_cluster <- signal_stability_between_stations %>%
  arrange(sd_power) %>%
  slice(1)

cat("\n=== Наиболее стабильный кластер ===\n")
```


    === Наиболее стабильный кластер ===

``` r
cat("Кластер номер:", most_stable_cluster$cluster, "\n")
```

    Кластер номер: 2 

``` r
cat("Стандартное отклонение:", round(most_stable_cluster$sd_power, 2), "dBm\n")
```

    Стандартное отклонение: 19.72 dBm

``` r
cat("Средняя мощность сигнала:", round(most_stable_cluster$mean_power, 2), "dBm\n")
```

    Средняя мощность сигнала: -61.49 dBm

``` r
cat("Размах (range):", round(most_stable_cluster$range_power, 2), "dBm\n")
```

    Размах (range): 87 dBm

``` r
cat("Ковариация:", round(most_stable_cluster$cv_power, 4), "\n")
```

    Ковариация: 0.3207 

``` r
for (i in 1:optimal_k) {
  cluster_data <- wifi_station_clustered %>% 
    filter(cluster == i & !is.na(power))
  
  cat("\nКластер", i, ":\n")
  cat("  Всего записей:", nrow(cluster_data), "\n")
  cat("  Уникальных станций:", n_distinct(cluster_data$station_mac), "\n")
  cat("  Среднее power:", round(mean(cluster_data$power, na.rm = TRUE), 2), "dBm\n")
  cat("  Медиана power:", round(median(cluster_data$power, na.rm = TRUE), 2), "dBm\n")
  cat("  SD power:", round(sd(cluster_data$power, na.rm = TRUE), 2), "dBm\n")
  cat("  Квартили: Q1 =", round(quantile(cluster_data$power, 0.25, na.rm = TRUE), 2),
      ", Q3 =", round(quantile(cluster_data$power, 0.75, na.rm = TRUE), 2), "\n")
}
```


    Кластер 1 :
      Всего записей: 1 
      Уникальных станций: 1 
      Среднее power: -59 dBm
      Медиана power: -59 dBm
      SD power: NA dBm
      Квартили: Q1 = -59 , Q3 = -59 

    Кластер 2 :
      Всего записей: 185 
      Уникальных станций: 185 
      Среднее power: -61.49 dBm
      Медиана power: -67 dBm
      SD power: 19.72 dBm
      Квартили: Q1 = -72 , Q3 = -59 

``` r
temporal_stability <- wifi_station_clustered %>%
  filter(!is.na(cluster) & !is.na(power) & !is.na(first_time_seen)) %>%
  mutate(
    session_duration = as.numeric(last_time_seen - first_time_seen, units = "mins")
  ) %>%
  group_by(cluster) %>%
  summarise(
    avg_session_duration = mean(session_duration, na.rm = TRUE),
    total_observations = n(),
    .groups = 'drop'
  )

cat("\n=== Временная характеристика кластеров ===\n")
```


    === Временная характеристика кластеров ===

``` r
print(temporal_stability)
```

    # A tibble: 2 × 3
      cluster avg_session_duration total_observations
        <int>                <dbl>              <int>
    1       1                 75.0                  1
    2       2                 41.5                185

``` r
cat("1. Кластеризация разделила", nrow(station_matrix_filtered), "станций на", optimal_k, "кластера\n")
```

    1. Кластеризация разделила 186 станций на 2 кластера

``` r
cat("2. Наиболее стабильный сигнал в кластере №", most_stable_cluster$cluster, "\n")
```

    2. Наиболее стабильный сигнал в кластере № 2 

``` r
cat("   - Стандартное отклонение =", round(most_stable_cluster$sd_power, 2), "dBm\n")
```

       - Стандартное отклонение = 19.72 dBm

``` r
cat("   - Среднее =", round(most_stable_cluster$mean_power, 2), "dBm\n")
```

       - Среднее = -61.49 dBm

``` r
cat("3. Сравнение кластеров по стабильности:\n")
```

    3. Сравнение кластеров по стабильности:

``` r
for (i in 1:nrow(signal_stability_between_stations)) {
  row <- signal_stability_between_stations[i,]
  cat("   Кластер", row$cluster, ": SD =", round(row$sd_power, 2), 
      "dBm, Range =", round(row$range_power, 2), "dBm\n")
}
```

       Кластер 2 : SD = 19.72 dBm, Range = 87 dBm
       Кластер 1 : SD = NA dBm, Range = 0 dBm

## Оценка результата

Мы получили знания о методах исследования радиоэлектронной обстановки,
составили представление о механизмах работы Wi-Fi сетей на канальном и
сетевом уровне модели OSI, закрепили практические навыки использования
языка программирования R для обработки данных, закрепили знания основных
функций обработки данных экосистемы tidyverse языка R

## Вывод

В практической работе мы использовали навыки написания кода на языке
программирования R для обработки данных и закрепили знания основных
функций обработки данных экосистемы tidyverse языка R.
