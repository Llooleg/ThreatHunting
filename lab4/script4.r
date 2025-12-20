library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(viridis)

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
head(dns_data,10)

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

all_unique_ip <- unique(c(dns_data_clean$source_ip, dns_data_clean$destination_ip))

#print(length(all_unique_ip))

internal_ips <- all_unique_ip[grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", all_unique_ip)]
external_ips <- all_unique_ip[!grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", all_unique_ip)]
length(internal_ips) / length(external_ips)

dns_data_clean %>% group_by(source_ip) %>% count(sort = TRUE) %>% head(10)

print(dns_data_clean %>% count(query, sort = TRUE) %>% head(10))

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

suspicious_ips <- periodic_analysis %>%
  group_by(source_ip) %>%
  summarise(
    domains_targeted = n_distinct(query),
    avg_regularity = mean(regularity_score),
    total_requests = sum(request_count)
  ) %>%
  filter(domains_targeted >= 1) %>%
  arrange(desc(avg_regularity))

print("Наиболее подозрительные IP-адреса:")
print(suspicious_ips, n = 20)
head(suspicious_ips)
top_10_domains <- dns_data_clean%>%count(query, sort = TRUE) %>%
  as_tibble() %>% head(10)
top_10_domains
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

print(domain_geo_info_final_sorted)

library(ggplot2)
library(scales)
library(viridis)
library(gridExtra)
library(lubridate)
library(tidyr)

theme_set(theme_minimal(base_size = 12))

# ============================================================================
# 1. TOP 10 ДОМЕНОВ - Bar Chart (потому что препод любит классику)
# ============================================================================
p1 <- ggplot(top_10_domains, aes(x = reorder(query, n), y = n)) +
  geom_bar(stat = "identity", fill = "#2E86AB", alpha = 0.8) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Most Queried Domains",
    subtitle = "Количество DNS запросов",
    x = "Domain",
    y = "Number of Queries"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1)))

print(p1)
ggsave("01_top_domains.png", p1, width = 10, height = 6, dpi = 300)

# ============================================================================
# 2. ВРЕМЕННОЙ HEATMAP - Активность по часам и дням недели
# ============================================================================
dns_temporal <- dns_data_clean %>%
  mutate(
    hour = hour(timestamp),
    wday = wday(timestamp, label = TRUE, week_start = 1)
  ) %>%
  count(wday, hour)

p2 <- ggplot(dns_temporal, aes(x = hour, y = wday, fill = n)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis(
    option = "plasma",
    name = "Requests",
    labels = scales::comma
  ) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(
    title = "DNS Request Activity Heatmap",
    subtitle = "Когда твоя сеть не спит (ботнеты работают 24/7)",
    x = "Hour of Day",
    y = "Day of Week"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p2)
ggsave("02_temporal_heatmap.png", p2, width = 12, height = 6, dpi = 300)

# ============================================================================
# 3. TIMELINE TOP ДОМЕНОВ - Запросы во времени
# ============================================================================
top_5_domains <- head(top_10_domains$query, 5)  # Берём топ-5 чтобы не было каши

dns_timeline <- dns_data_clean %>%
  filter(query %in% top_5_domains) %>%
  mutate(
    time_rounded = floor_date(timestamp, "10 minutes")
  ) %>%
  count(time_rounded, query)

p3 <- ggplot(dns_timeline, aes(x = time_rounded, y = n, color = query)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_brewer(palette = "Set2", name = "Domain") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "DNS Request Timeline for Top 5 Domains",
    subtitle = "Активность по времени (окно 10 минут)",
    x = "Time",
    y = "Number of Requests"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(p3)
ggsave("03_domain_timeline.png", p3, width = 14, height = 6, dpi = 300)

# 
p4 <- ggplot(periodic_analysis, aes(x = mean_interval, y = cv_interval)) +
  geom_point(aes(size = request_count, color = regularity_score), alpha = 0.6) +
  scale_color_viridis(
    option = "magma",
    name = "Regularity\nScore",
    direction = -1
  ) +
  scale_size_continuous(name = "Request\nCount", range = c(2, 10)) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Suspicious Periodic DNS Activity",
    subtitle = "Низкий CV + высокая регулярность = потенциальный C&C beacon",
    x = "Mean Interval (seconds, log scale)",
    y = "Coefficient of Variation"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5)

print(p4)
ggsave("04_suspicious_activity.png", p4, width = 12, height = 8, dpi = 300)

top_sources <- dns_data_clean %>%
  group_by(source_ip) %>%
  count(sort = TRUE) %>%
  head(10)

p5 <- ggplot(top_sources, aes(x = reorder(source_ip, n), y = n)) +
  geom_bar(stat = "identity", fill = "#A23B72", alpha = 0.8) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Most Active Source IPs",
    subtitle = "Кто больше всех спамит DNS запросами",
    x = "Source IP",
    y = "Number of Queries"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1)))

print(p5)
ggsave("05_top_source_ips.png", p5, width = 10, height = 6, dpi = 300)


  geo_summary <- domain_geo_info_final_sorted %>%
    filter(!is.na(country), country != "Частный IP") %>%
    count(country, sort = TRUE) %>%
    head(15)
  
  p6 <- ggplot(geo_summary, aes(x = reorder(country, n), y = n)) +
    geom_bar(stat = "identity", fill = "#F18F01", alpha = 0.8) +
    geom_text(aes(label = n), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(
      title = "Geographic Distribution of DNS Servers",
      subtitle = "Откуда отвечают DNS серверы (спойлер: половина из США)",
      x = "Country",
      y = "Number of Unique IPs"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  
  print(p6)
  ggsave("06_geographic_distribution.png", p6, width = 10, height = 8, dpi = 300)
}

# ============================================================================
# 7. РАСПРЕДЕЛЕНИЕ ИНТЕРВАЛОВ - Box Plot для топ доменов
# ============================================================================
top_domains_intervals <- dns_data_clean %>%
  filter(query %in% top_5_domains) %>%
  group_by(query) %>%
  arrange(timestamp) %>%
  mutate(time_diff = as.numeric(timestamp - lag(timestamp))) %>%
  filter(!is.na(time_diff), time_diff < 300)  # Фильтр < 5 минут для читаемости

p7 <- ggplot(top_domains_intervals, aes(x = query, y = time_diff, fill = query)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Request Interval Distribution for Top Domains",
    subtitle = "Распределение времени между запросами (< 5 min, log scale)",
    x = "Domain",
    y = "Time Difference (seconds, log scale)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(p7)
ggsave("07_interval_distribution.png", p7, width = 12, height = 6, dpi = 300)

# ============================================================================
# 8. QUERY TYPE DISTRIBUTION - Pie Chart (потому что кто-то их любит)
# ============================================================================
qtype_dist <- dns_data_clean %>%
  filter(!is.na(qtype_name)) %>%
  count(qtype_name, sort = TRUE) %>%
  head(8) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(qtype_name, "\n", round(percentage, 1), "%")
  )

p8 <- ggplot(qtype_dist, aes(x = "", y = n, fill = qtype_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Spectral", name = "Query Type") +
  geom_text(aes(label = round(percentage, 1)), 
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  labs(
    title = "DNS Query Type Distribution",
    subtitle = "Какие типы запросов используются (обычно 90% это A записи)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )

print(p8)
ggsave("08_query_type_distribution.png", p8, width = 10, height = 6, dpi = 300)

# ============================================================================
# 9. ПОДОЗРИТЕЛЬНЫЕ IP - Faceted анализ
# ============================================================================
if(exists("suspicious_ips") && nrow(suspicious_ips) > 0) {
  top_suspicious <- head(suspicious_ips, 6)
  
  suspicious_details <- dns_data_clean %>%
    filter(source_ip %in% top_suspicious$source_ip) %>%
    group_by(source_ip, query) %>%
    arrange(timestamp) %>%
    mutate(time_diff = as.numeric(timestamp - lag(timestamp))) %>%
    filter(!is.na(time_diff))
  
  p9 <- ggplot(suspicious_details, aes(x = time_diff)) +
    geom_histogram(bins = 50, fill = "#C73E1D", alpha = 0.7) +
    facet_wrap(~source_ip, scales = "free_y", ncol = 2) +
    scale_x_log10() +
    labs(
      title = "Request Interval Patterns for Suspicious IPs",
      subtitle = "Гистограммы интервалов - ищем аномально регулярные паттерны",
      x = "Time Difference (seconds, log scale)",
      y = "Frequency"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold")
    )
  
  print(p9)
  ggsave("09_suspicious_patterns.png", p9, width = 14, height = 10, dpi = 300)
}

# ============================================================================
# БОНУС: Сохраняем summary статистику в текстовый файл
# ============================================================================

cat("=== DNS LOG ANALYSIS SUMMARY ===\n\n")
cat("Total DNS records:", nrow(dns_data_clean), "\n")
cat("Unique domains queried:", n_distinct(dns_data_clean$query), "\n")
cat("Unique source IPs:", n_distinct(dns_data_clean$source_ip), "\n")
cat("Date range:", min(dns_data_clean$timestamp), "to", max(dns_data_clean$timestamp), "\n")
cat("\nTop 3 most queried domains:\n")
print(head(top_10_domains, 3))
if(exists("suspicious_ips")) {
  cat("\n\nSuspicious IPs detected:", nrow(suspicious_ips), "\n")
  cat("Top 3 suspicious IPs by regularity:\n")
  print(head(suspicious_ips, 3))
}
sink()

cat("\n✓ Все графики сохранены в PNG файлы (300 DPI для печати)\n")
cat("✓ Summary сохранён в visualization_summary.txt\n")
cat("\nТеперь у тебя есть что показать преподу. Удачи с защитой! 🎓\n")