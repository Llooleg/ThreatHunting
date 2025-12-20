library(jsonlite)
library(tidyverse)
library(xml2)
library(rvest)

dataset <- "https://storage.yandexcloud.net/iamcth-data/dataset.tar.gz"
archive <- "dataset.tar.gz"

if (!file.exists(archive)) {
  cat("Скачивание архива...\n")
  download.file(url = dataset, destfile = archive, mode = "wb")
}

archive_contents <- untar(tarfile = archive, list = TRUE)
json_file <- archive_contents[grep("\\.json$", archive_contents)]
temp_json_dir <- tempdir()
untar(tarfile = archive, files = json_file, exdir = temp_json_dir)
json_file_path <- file.path(temp_json_dir, json_file)

json_con <- file(json_file_path, "r")
raw_data <- stream_in(json_con, pagesize = 10000, verbose = FALSE, flatten = TRUE)
close(json_con)

cat("Загружено:", nrow(raw_data), "записей,", ncol(raw_data), "колонок\n")

cat("Удаление константных колонок...\n")
variance_check <- sapply(raw_data, function(col) {
  if (is.list(col)) return(TRUE)  # Оставляем списки пока
  n_distinct(col, na.rm = TRUE) > 1
})

minimized_data <- raw_data[, variance_check]
cat("Осталось колонок:", ncol(minimized_data), "\n")

list_cols <- names(minimized_data)[sapply(minimized_data, is.list)]
cat("Колонок-списков для разворачивания:", length(list_cols), "\n")

if (length(list_cols) > 0) {
  for (col in list_cols) {
    cat("  Разворачиваем:", col, "\n")
    tryCatch({
      sample_val <- minimized_data[[col]][[which(!sapply(minimized_data[[col]], is.null))[1]]]
      
      if (is.list(sample_val) && !is.null(names(sample_val))) {
        minimized_data <- minimized_data %>% 
          unnest_wider(all_of(col), names_sep = "_", simplify = TRUE, strict = FALSE)
      } else {
        minimized_data[[col]] <- sapply(minimized_data[[col]], function(x) {
          if (is.null(x) || length(x) == 0) return(NA_character_)
          if (length(x) == 1) return(as.character(x))
          paste(x, collapse = ", ")
        })
      }
    }, error = function(e) {
      cat("    Ошибка, пропускаем\n")
    })
  }
}

cat("Финальных колонок:", ncol(minimized_data), "\n")

# Подсчёт хостов
host_col_candidates <- grep("hostname|host", names(minimized_data), 
                            ignore.case = TRUE, value = TRUE)
cat("\nНайденные колонки с хостами:", paste(host_col_candidates, collapse = ", "), "\n")

if (length(host_col_candidates) > 0) {
  host_col_name <- host_col_candidates[1]
  num_unique_hosts <- n_distinct(minimized_data[[host_col_name]], na.rm = TRUE)
  cat("Уникальных хостов:", num_unique_hosts, "\n")
}

# Загрузка справочника событий Microsoft
cat("\nЗагрузка справочника критичности событий...\n")
webpage_url <- "https://learn.microsoft.com/en-us/windows-server/identity/ad-ds/plan/appendix-l--events-to-monitor"
webpage <- read_html(webpage_url)
event_df_raw <- html_table(webpage)[[1]]

event_df <- event_df_raw %>%
  transmute(
    event_id = as.numeric(`Current Windows Event ID`),
    criticality = `Potential Criticality`
  ) %>%
  filter(!is.na(event_id))

cat("Загружено событий:", nrow(event_df), "\n")

# Поиск колонки с event_code
event_col_candidates <- grep("event.*code|event.*id", names(minimized_data), 
                             ignore.case = TRUE, value = TRUE)
cat("\nНайденные колонки с кодами событий:", paste(event_col_candidates, collapse = ", "), "\n")

if (length(event_col_candidates) > 0) {
  event_col_name <- event_col_candidates[1]
  cat("Используем колонку:", event_col_name, "\n")
  
  # Убеждаемся что это numeric
  if (!is.numeric(minimized_data[[event_col_name]])) {
    minimized_data[[event_col_name]] <- as.numeric(minimized_data[[event_col_name]])
  }
  
  # ОПТИМИЗАЦИЯ: сначала агрегируем, потом джойним
  cat("Подсчёт критичности событий...\n")
  
  event_summary <- minimized_data %>%
    count(.data[[event_col_name]], name = "count") %>%
    rename(event_code = 1) %>%
    left_join(event_df, by = c("event_code" = "event_id"))
  
  # Подсчёт по критичности
  criticality_counts <- event_summary %>%
    summarise(
      high = sum(count[grepl("High", criticality, ignore.case = TRUE)], na.rm = TRUE),
      medium = sum(count[grepl("Medium", criticality, ignore.case = TRUE)], na.rm = TRUE),
      total_events = sum(count),
      .groups = "drop"
    )
  
  cat("\n" , rep("=", 50), "\n", sep = "")
  cat("РЕЗУЛЬТАТЫ АНАЛИЗА\n")
  cat(rep("=", 50), "\n", sep = "")
  cat("События высокой значимости:  ", criticality_counts$high, "\n")
  cat("События средней значимости:  ", criticality_counts$medium, "\n")
  cat("Всего событий:               ", criticality_counts$total_events, "\n")
  
  if (criticality_counts$high == 0 && criticality_counts$medium == 0) {
    cat("\n⚠️  ВНИМАНИЕ:\n")
    cat("Ни одного события из справочника Microsoft не найдено.\n")
    cat("Это нормально — справочник покрывает только AD/Kerberos/IPsec.\n")
    cat("Ваш датасет содержит системные/PowerShell/WMI события.\n")
  }
  
  cat(rep("=", 50), "\n", sep = "")
  
  # Топ-10 самых частых событий
  cat("\nТоп-10 самых частых событий:\n")
  event_summary %>%
    arrange(desc(count)) %>%
    head(10) %>%
    mutate(criticality = ifelse(is.na(criticality), "Unknown", criticality)) %>%
    print()
  
} else {
  cat("\nОШИБКА: Не найдена колонка с кодами событий!\n")
  cat("Первые 20 колонок датасета:\n")
  print(head(names(minimized_data), 20))
}

cat("\nГотово!\n")

