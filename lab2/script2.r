library(dplyr)

#print(starwars %>% nrow())

#print(starwars %>% ncol())

#print(starwars %>% glimpse())

#nrow(starwars %>% distinct(species))

starwars %>% filter(height == max(height, na.rm = TRUE)) %>%  select(name)


starwars %>% filter(height<170)

starwars %>%
  filter(!is.na(height), !is.na(mass)) %>%
  mutate(BMI = mass / (height/100)^2) %>%
  select(name, height, mass, BMI) %>%
  print(n=59)

starwars %>%
  filter(!is.na(height), !is.na(mass)) %>%
  mutate(elongation = mass / (height)) %>%
  select(name, elongation) %>%
  arrange(desc(elongation)) %>%
  print(n=59)

starwars %>%
  filter(!is.na(birth_year)) %>%
  mutate(age = 100 + birth_year) %>% 
  summarise(mean_age = mean(age))


starwars %>%
count(eye_color, sort = TRUE) %>%
slice(1) %>%
pull(eye_color)

starwars %>%
mutate(str_length = nchar(as.character(name))) %>%
summarise(avg_length = mean(str_length, na.rm = TRUE))