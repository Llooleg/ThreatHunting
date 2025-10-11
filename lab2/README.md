# Вторая практика
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

1.  Сколько строк в датафрейме?

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
starwars %>% nrow()
```

    [1] 87

1.  Сколько столбцов в датафрейме?

``` r
library(dplyr)

starwars %>% ncol()
```

    [1] 14

1.  Как просмотреть примерный вид датафрейма?

``` r
library(dplyr)

starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

1.  Сколько уникальных рас персонажей (species) представлено в данных?

``` r
library(dplyr)

nrow(starwars %>% distinct(species))
```

    [1] 38

1.  Найти самого высокого персонажа.

``` r
library(dplyr)

starwars %>% filter(height == max(height, na.rm = TRUE)) %>%  select(name)
```

    # A tibble: 1 × 1
      name       
      <chr>      
    1 Yarael Poof

1.  Найти всех персонажей ниже 170

``` r
library(dplyr)

starwars %>% filter(height<170)
```

    # A tibble: 22 × 14
       name     height  mass hair_color skin_color eye_color birth_year sex   gender
       <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
     1 C-3PO       167    75 <NA>       gold       yellow           112 none  mascu…
     2 R2-D2        96    32 <NA>       white, bl… red               33 none  mascu…
     3 Leia Or…    150    49 brown      light      brown             19 fema… femin…
     4 Beru Wh…    165    75 brown      light      blue              47 fema… femin…
     5 R5-D4        97    32 <NA>       white, red red               NA none  mascu…
     6 Yoda         66    17 white      green      brown            896 male  mascu…
     7 Mon Mot…    150    NA auburn     fair       blue              48 fema… femin…
     8 Wicket …     88    20 brown      brown      brown              8 male  mascu…
     9 Nien Nu…    160    68 none       grey       black             NA male  mascu…
    10 Watto       137    NA black      blue, grey yellow            NA male  mascu…
    # ℹ 12 more rows
    # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
    #   vehicles <list>, starships <list>

1.  Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ
    подсчитать по формуле 𝐼 = 𝑚/(ℎ^2)

``` r
library(dplyr)

starwars %>%
  filter(!is.na(height), !is.na(mass)) %>%
  mutate(BMI = mass / (height/100)^2) %>%
  select(name, height, mass, BMI) %>%
  print(n=59)
```

    # A tibble: 59 × 4
       name                  height   mass   BMI
       <chr>                  <int>  <dbl> <dbl>
     1 Luke Skywalker           172   77    26.0
     2 C-3PO                    167   75    26.9
     3 R2-D2                     96   32    34.7
     4 Darth Vader              202  136    33.3
     5 Leia Organa              150   49    21.8
     6 Owen Lars                178  120    37.9
     7 Beru Whitesun Lars       165   75    27.5
     8 R5-D4                     97   32    34.0
     9 Biggs Darklighter        183   84    25.1
    10 Obi-Wan Kenobi           182   77    23.2
    11 Anakin Skywalker         188   84    23.8
    12 Chewbacca                228  112    21.5
    13 Han Solo                 180   80    24.7
    14 Greedo                   173   74    24.7
    15 Jabba Desilijic Tiure    175 1358   443. 
    16 Wedge Antilles           170   77    26.6
    17 Jek Tono Porkins         180  110    34.0
    18 Yoda                      66   17    39.0
    19 Palpatine                170   75    26.0
    20 Boba Fett                183   78.2  23.4
    21 IG-88                    200  140    35  
    22 Bossk                    190  113    31.3
    23 Lando Calrissian         177   79    25.2
    24 Lobot                    175   79    25.8
    25 Ackbar                   180   83    25.6
    26 Wicket Systri Warrick     88   20    25.8
    27 Nien Nunb                160   68    26.6
    28 Qui-Gon Jinn             193   89    23.9
    29 Nute Gunray              191   90    24.7
    30 Padmé Amidala            185   45    13.1
    31 Jar Jar Binks            196   66    17.2
    32 Roos Tarpals             224   82    16.3
    33 Sebulba                  112   40    31.9
    34 Darth Maul               175   80    26.1
    35 Ayla Secura              178   55    17.4
    36 Ratts Tyerel              79   15    24.0
    37 Dud Bolt                  94   45    50.9
    38 Ben Quadinaros           163   65    24.5
    39 Mace Windu               188   84    23.8
    40 Ki-Adi-Mundi             198   82    20.9
    41 Kit Fisto                196   87    22.6
    42 Adi Gallia               184   50    14.8
    43 Plo Koon                 188   80    22.6
    44 Gregar Typho             185   85    24.8
    45 Poggle the Lesser        183   80    23.9
    46 Luminara Unduli          170   56.2  19.4
    47 Barriss Offee            166   50    18.1
    48 Dooku                    193   80    21.5
    49 Jango Fett               183   79    23.6
    50 Zam Wesell               168   55    19.5
    51 Dexter Jettster          198  102    26.0
    52 Lama Su                  229   88    16.8
    53 Wat Tambor               193   48    12.9
    54 Shaak Ti                 178   57    18.0
    55 Grievous                 216  159    34.1
    56 Tarfful                  234  136    24.8
    57 Raymus Antilles          188   79    22.4
    58 Sly Moore                178   48    15.1
    59 Tion Medon               206   80    18.9

1.  Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
    отношению массы (mass) к росту (height) персонажей

``` r
library(dplyr)


starwars %>%
  filter(!is.na(height), !is.na(mass)) %>%
  mutate(elongation = mass / (height)) %>%
  select(name, elongation) %>%
  arrange(desc(elongation))
```

    # A tibble: 59 × 2
       name                  elongation
       <chr>                      <dbl>
     1 Jabba Desilijic Tiure      7.76 
     2 Grievous                   0.736
     3 IG-88                      0.7  
     4 Owen Lars                  0.674
     5 Darth Vader                0.673
     6 Jek Tono Porkins           0.611
     7 Bossk                      0.595
     8 Tarfful                    0.581
     9 Dexter Jettster            0.515
    10 Chewbacca                  0.491
    # ℹ 49 more rows

1.  Найти средний возраст персонажей каждой расы вселенной Звездных
    войн.

``` r
library(dplyr)

starwars %>%
  filter(!is.na(birth_year)) %>%
  mutate(age = 100 + birth_year) %>% 
  summarise(mean_age = mean(age))
```

    # A tibble: 1 × 1
      mean_age
         <dbl>
    1     188.

1.  Найти самый распространенный цвет глаз персонажей вселенной Звездных
    войн.

``` r
library(dplyr)

starwars %>%
count(eye_color, sort = TRUE) %>%
slice(1) %>%
pull(eye_color)
```

    [1] "brown"

1.  Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн

``` r
library(dplyr)

starwars %>%
mutate(str_length = nchar(as.character(name))) %>%
summarise(avg_length = mean(str_length, na.rm = TRUE))
```

    # A tibble: 1 × 1
      avg_length
           <dbl>
    1       10.3

## Оценка результата

В результате лабораторной работы мы ознакомились с dplyr.

## Вывод

Таким образом, мы научились основам dplyr
