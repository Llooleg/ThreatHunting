library(dplyr)
print(starwars %>% nrow())

print(starwars %>% ncol())
print(starwars %>% glimpse())
print(starwars %>% distinct(species), n=38)
print(starwars %>% filter(height == max(height, na.rm = TRUE)))
print(starwars %>% filter(height<170), n=22)
starwars %>%
  filter(!is.na(height), !is.na(mass)) %>%
  mutate(BMI = mass / (height/100)^2) %>%
  select(name, height, mass, BMI) %>%
  print(n=59)

  filter(!is.na(height), !is.na(mass)) %>%
  mutate(
    BMI = mass / (height/100)^2,
    BMI_category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal",
      BMI < 30 ~ "Overweight",
      TRUE ~ "Obese"
    )
  ) %>%
  count(BMI_category, sort = TRUE)