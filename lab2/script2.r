library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
print(starwars %>% nrow())

print(starwars %>% ncol())

print(starwars %>% glimpse())

nrow(starwars %>% distinct(species))

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


starwars_bmi <- starwars %>%
  filter(!is.na(height), !is.na(mass)) %>%
  mutate(BMI = mass / (height/100)^2)


starwars_bmi %>%
arrange(desc(BMI)) %>%
slice(1:10) %>%
ggplot(aes(x = reorder(name, BMI), y = BMI, fill = BMI)) +
geom_col() +
coord_flip()

# Example of how the first plot might be constructed
starwars_bmi_top10 <- starwars_bmi %>%
  arrange(desc(BMI)) %>%
  head(10)

starwars_bmi %>%
  arrange(desc(BMI)) %>%
  slice(1:10) %>%  
  ggplot(aes(x = reorder(name, BMI), y = BMI, fill = BMI)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#FFA500", high = "#F00000") +

  labs(title = "Топ-10 персонажей с самым высоким ИМТ",
       subtitle = "Джабба явно не беспокоится о своей фигуре",
       x = NULL,
       y = "ИМТ") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 15))  
ggsave("plot1.png")
ggplot(starwars_bmi, aes(x = height, y = mass, color = BMI, size = BMI)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient2(low = "blue", mid ="green", high ="red", 
                        midpoint = 25) +
  labs(title = "Рост vs Масса персонажей Star Wars",
       subtitle = "Размер и цвет точки показывают ИМТ",
       x = "Рост (см)",
       y = "Масса (кг)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave("plot2.png")

starwars %>%
  select(height, mass, birth_year) %>%
  cor(use = "complete.obs") %>%
  round(2)
starwars %>%
  select(height, mass, birth_year) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal()

ggsave("plot3.png")