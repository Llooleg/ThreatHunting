library(readr)
library(stringr)
library("fpc")
library("mclust")
library("httr") 
library("V8") 
library(dplyr)
library(tidyr)
library(fpc)
library(janitor)
library("R.utils") 
library("jsonlite") 
library("igraph") 
library("V8")
#library(factoextra)
library(tidyverse)

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



names(wifi_ap_data) <- janitor::make_clean_names(names(wifi_ap_data))
wifi_ap_data <- wifi_ap_data %>%
  mutate(
    first_time_seen = lubridate::ymd_hms(first_time_seen, tz = "UTC"),
    last_time_seen = lubridate::ymd_hms(last_time_seen, tz = "UTC")
  )
wifi_ap_data <- wifi_ap_data %>%
  mutate_if(is.character, ~trimws(.x))
names(wifi_station_data) <- janitor::make_clean_names(names(wifi_station_data))
wifi_station_data <- wifi_station_data %>%
  mutate(
    first_time_seen = lubridate::ymd_hms(first_time_seen, tz = "UTC"),
    last_time_seen = lubridate::ymd_hms(last_time_seen, tz = "UTC")
  )
wifi_station_data <- wifi_station_data %>%
  mutate_if(is.character, ~trimws(.x))
cat("\n--- Типы столбцов после преобразований ---\n")
glimpse(wifi_ap_data)

glimpse(wifi_station_data)


unsafe_wifi <- wifi_ap_data %>%
  filter(privacy == "OPN")

print(unsafe_wifi)


get_manufacturer_by_mac <- function(mac_address, timeout = 10) {
  url <- paste0("https://www.macvendorlookup.com/api/v2/", mac_address)
  tryCatch({
    response <- httr::GET(
      url,
      httr::timeout(timeout),
      httr::add_headers(
        "User-Agent" = "Mozilla/5.0 (compatible; R script)"
      )
    )
    if (httr::status_code(response) != 200) {
      return(NULL)
    }
    
    content <- httr::content(response, "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(content)
    if (length(data) == 0 || is.null(data$company) || data$company == "") {
      return(NULL)
    }
    return(data$company[1])
    
  }, error = function(e) {
    return(NULL)
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



final_ap_tibble <- wifi_ap_data %>%
  select(bssid, essid, manufacturer)

print(head(final_ap_tibble, 10))


wpa3_aps <- wifi_ap_data %>%
  filter(grepl("WPA3", authentication) | grepl("WPA3", privacy))
print(wpa3_aps %>% select(bssid, privacy, essid, cipher))


fastest_wifi <- wifi_ap_data %>%
  arrange(desc(speed)) %>%
  head(10) 
print(fastest_wifi)


join_sessions <- function(ap_data_single_bssid, threshold_seconds = 3000) {
  ap_data_single_bssid <- ap_data_single_bssid %>% 
    filter(!is.na(first_time_seen) & !is.na(last_time_seen))
  
  if (nrow(ap_data_single_bssid) == 0) {
    return(tibble(total_duration_seconds = numeric()))
  }
  
  ap_data_sorted <- ap_data_single_bssid %>% arrange(first_time_seen)
  first_times <- ap_data_sorted$first_time_seen
  last_times <- ap_data_sorted$last_time_seen
  
  session_starts <- first_times[1]
  session_ends <- last_times[1]
  
  for (i in 2:nrow(ap_data_sorted)) {
    current_start <- first_times[i]
    current_end <- last_times[i]
    last_end <- session_ends[length(session_ends)] 
    time_diff <- as.numeric(current_start - last_end, units = "secs")
    
    if (is.na(time_diff) || time_diff > threshold_seconds) {
      session_starts <- c(session_starts, current_start)
      session_ends <- c(session_ends, current_end)
    } else {
      session_ends[length(session_ends)] <- max(session_ends[length(session_ends)], current_end)
    }
  }
  
  durations <- as.numeric(session_ends - session_starts, units = "secs")
  
  result <- tibble(
    total_duration_seconds = durations
  )
  return(result)
}

wifi_ap_sorted_by_duration <- wifi_ap_data %>%
  filter(!is.na(first_time_seen) & !is.na(last_time_seen)) %>%
  group_by(bssid) %>%
  summarise(
    sessions_data = list(join_sessions(cur_data())),
    .groups = 'drop'
  ) %>%
  unnest(sessions_data) %>%
  group_by(bssid) %>%
  summarise(
    total_time_on_channel_seconds = sum(total_duration_seconds, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_time_on_channel_seconds))

print(wifi_ap_sorted_by_duration)


library(dplyr)
library(readr)

cat("\n=== ЗАДАНИЕ 11: Производители через OUI файл (как нормальные люди) ===\n")



parse_oui_file <- function(file_path) {
  cat("📖 Читаем OUI файл...\n")
  

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
    # Преобразуем формат XX-XX-XX в xx:xx:xx для совместимости
    mutate(mac_prefix = tolower(gsub("-", ":", mac_prefix)))
  
  cat(" Загружено производителей:", nrow(oui_data), "\n")
  return(oui_data)
}

# Функция поиска производителя по MAC
get_manufacturer_from_oui <- function(mac_address, oui_lookup) {
  # Берём первые 3 октета (XX:XX:XX)
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
  
  
  # ВАРИАНТ 2: Скачиваем автоматически
  cat(" Пытаемся скачать автоматически...\n")
  download.file(
    "https://standards-oui.ieee.org/oui/oui.txt",
    destfile = oui_file,
    quiet = FALSE
  )
  oui_lookup <- parse_oui_file(oui_file)
}


unique_stations <- wifi_station_data %>%
  filter(!is.na(station_mac) & station_mac != "") %>%
  distinct(station_mac)

cat("Всего уникальных станций:", nrow(unique_stations), "\n")

station_manufacturers <- unique_stations %>%
  mutate(
    manufacturer = sapply(station_mac, function(mac) {
      get_manufacturer_from_oui(mac, oui_lookup)
    })
  )

cat("\n=== Результаты ===\n")
cat("Успешно определено:", sum(!is.na(station_manufacturers$manufacturer)), 
    "из", nrow(station_manufacturers), "\n")
cat("Время выполнения: ~5 секунд (а не 3 часа)\n")


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

cat("\n=== Топ-20 клиентских устройств ===\n")
print(head(final_station_tibble, 20))

cat("\n=== ТОП-10 производителей по количеству устройств ===\n")
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

output_file <- "station_manufacturers.csv"
write_csv(final_station_tibble, output_file)
cat("\n Результаты сохранены в", output_file, "\n")





top_10_fastest_points <- wifi_ap_data %>%
  filter(!is.na(speed)) %>%
  arrange(desc(speed)) %>%
  slice_head(n = 10)
print(select(top_10_fastest_points, speed, bssid, essid, manufacturer))


print(wifi_ap_data %>%
  mutate(
    life_time = as.numeric(last_time_seen - first_time_seen, units = "mins"),
    beacons_per_minute = number_beacons / life_time
  ) %>%
  filter(life_time > 0) %>%
  arrange(desc(beacons_per_minute)) %>%
  select(essid, bssid, beacons_per_minute, number_beacons, life_time, 
         first_time_seen, last_time_seen) %>%
  head(10))


  ap_strong <- wifi_ap_data %>% filter(!substr("Station MAC", 2,2) %in% c("2", "3", "6", "7", "A", "B", "E", "F"))
print(head(ap_strong, 10))


# ============================================================
# Задание 13: Кластеризация запросов от устройств к точкам доступа
# ============================================================


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

cat("\n=== Размерность матрицы для кластеризации ===\n")
cat("Количество станций:", nrow(station_matrix), "\n")
cat("Количество уникальных BSSID:", ncol(station_matrix), "\n")

station_matrix_filtered <- station_matrix[rowSums(station_matrix) > 0, ]

cat("\nПосле фильтрации:\n")
cat("Количество станций:", nrow(station_matrix_filtered), "\n")

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
  
  cat("\n=== Оценка оптимального количества кластеров ===\n")
  cat("Силуэтные коэффициенты для k от 2 до", max_k, ":\n")
  for (i in 1:length(silhouette_scores)) {
    cat("k =", i + 1, ": ", round(silhouette_scores[i], 4), "\n")
  }
  cat("\nОптимальное количество кластеров:", optimal_k, "\n")
  
} else {
  optimal_k <- min(3, nrow(station_matrix_filtered) - 1)
  cat("\n=== Недостаточно данных для полного анализа ===\n")
  cat("Используем k =", optimal_k, "\n")
}

set.seed(123)
final_clusters <- kmeans(station_matrix_filtered, centers = optimal_k, nstart = 25)

cat("\n=== Результаты кластеризации ===\n")
cat("Размеры кластеров:\n")
print(table(final_clusters$cluster))

clustered_stations <- tibble(
  station_mac = rownames(station_matrix_filtered),
  cluster = final_clusters$cluster
)

wifi_station_clustered <- wifi_station_data %>%
  left_join(clustered_stations, by = "station_mac")

cat("\n=== Примеры станций по кластерам ===\n")
for (i in 1:optimal_k) {
  cat("\nКластер", i, ":\n")
  cluster_stations <- clustered_stations %>% filter(cluster == i)
  print(head(cluster_stations, 5))
}

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

cat("\n=== Характеристики кластеров ===\n")
print(cluster_characteristics)


# ============================================================
# Задание 14: Оценка стабильности уровня сигнала внутри кластеров
# ============================================================

cat("\n\n=== ЗАДАНИЕ 14: Оценка стабильности сигнала ===\n")

cat("\n=== Проверка данных ===\n")
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
cat("(Меньшее SD = более однородные уровни сигнала в кластере)\n\n")
print(signal_stability_between_stations)

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

cat("\n=== Стабильность средних уровней сигнала станций ===\n")
cat("(Анализ средних power каждой станции внутри кластера)\n\n")
print(cluster_stability_by_avg)

most_stable_cluster <- signal_stability_between_stations %>%
  arrange(sd_power) %>%
  slice(1)

cat("\n=== Наиболее стабильный кластер ===\n")
cat("Кластер номер:", most_stable_cluster$cluster, "\n")
cat("Стандартное отклонение:", round(most_stable_cluster$sd_power, 2), "dBm\n")
cat("Средняя мощность сигнала:", round(most_stable_cluster$mean_power, 2), "dBm\n")
cat("Размах (range):", round(most_stable_cluster$range_power, 2), "dBm\n")
cat("Коэффициент вариации:", round(most_stable_cluster$cv_power, 4), "\n")
cat("IQR:", round(most_stable_cluster$iqr_power, 2), "dBm\n")

cat("\n=== Детальное распределение мощности по кластерам ===\n")
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
  cat("  Записей с power > -70:", sum(cluster_data$power > -70, na.rm = TRUE), "\n")
  cat("  Записей с power < -80:", sum(cluster_data$power < -80, na.rm = TRUE), "\n")
}

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
print(temporal_stability)

cat("\n=== Выводы ===\n")
cat("1. Кластеризация разделила", nrow(station_matrix_filtered), "станций на", optimal_k, "кластера\n")
cat("2. Наиболее стабильный сигнал в кластере №", most_stable_cluster$cluster, "\n")
cat("   - SD =", round(most_stable_cluster$sd_power, 2), "dBm\n")
cat("   - Mean =", round(most_stable_cluster$mean_power, 2), "dBm\n")
cat("3. Сравнение кластеров по стабильности:\n")
for (i in 1:nrow(signal_stability_between_stations)) {
  row <- signal_stability_between_stations[i,]
  cat("   Кластер", row$cluster, ": SD =", round(row$sd_power, 2), 
      "dBm, Range =", round(row$range_power, 2), "dBm\n")
}