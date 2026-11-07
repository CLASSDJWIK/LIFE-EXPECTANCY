# DATA CHALLENGE — Gapminder annuale 1952–2025
# Ricostruisce le sei colonne originali e i 142 paesi del campione didattico.
# Le serie annuali aggiornate NON sono un'interpolazione del vecchio dataset.
# Eseguire dalla cartella che contiene questo file.

library(dplyr)
library(tidyr)
library(readr)
library(countrycode)

anni <- 1952:2025

# I 142 paesi e i cinque continenti sono quelli del pacchetto del corso.
paesi <- gapminder::gapminder |>
  distinct(country, continent) |>
  mutate(
    country = as.character(country),
    continent = as.character(continent),
    geo = tolower(countrycode(country, "country.name", "iso3c", warn = FALSE)),
    geo = case_when(
      country == "Congo, Dem. Rep." ~ "cod",
      country == "Congo, Rep." ~ "cog",
      country == "Cote d'Ivoire" ~ "civ",
      country == "Hong Kong, China" ~ "hkg",
      country == "Korea, Dem. Rep." ~ "prk",
      country == "Korea, Rep." ~ "kor",
      country == "Reunion" ~ "reu",
      country == "Sao Tome and Principe" ~ "stp",
      country == "Slovak Republic" ~ "svk",
      country == "Swaziland" ~ "swz",
      country == "Taiwan" ~ "twn",
      country == "West Bank and Gaza" ~ "pse",
      country == "Yemen, Rep." ~ "yem",
      TRUE ~ geo
    )
  )

if (anyNA(paesi$geo) || anyDuplicated(paesi$geo)) {
  stop("Mappatura ISO3 incompleta o duplicata: verificare i nomi dei paesi.")
}

base_url <- "https://raw.githubusercontent.com/open-numbers/"
life_url <- paste0(base_url,
  "ddf--gapminder--life_expectancy/master/",
  "ddf--datapoints--life_expectancy_at_birth--by--geo--time.csv")
pop_url <- paste0(base_url,
  "ddf--gapminder--population/master/",
  "ddf--datapoints--population--by--country--year.csv")
gdp_url <- paste0(base_url,
  "ddf--gapminder--gdp_per_capita_cppp/master/",
  "ddf--datapoints--income_per_person_gdppercapita_ppp_inflation_adjusted--by--geo--time.csv")

# Importiamo e selezioniamo solo le misure e gli anni necessari.
life <- read_csv(life_url, show_col_types = FALSE, progress = FALSE) |>
  transmute(geo = as.character(geo), year = as.integer(time),
            lifeExp = as.double(life_expectancy_at_birth)) |>
  filter(year %in% anni)

population <- read_csv(pop_url, show_col_types = FALSE, progress = FALSE) |>
  transmute(geo = as.character(country), year = as.integer(year),
            pop = as.integer(round(population))) |>
  filter(year %in% anni)

gdp <- read_csv(gdp_url, show_col_types = FALSE, progress = FALSE) |>
  transmute(geo = as.character(geo), year = as.integer(time),
            gdpPercap = as.double(income_per_person_gdppercapita_ppp_inflation_adjusted)) |>
  filter(year %in% anni)

# Ogni coppia (paese, anno) deve essere univoca nelle fonti.
stopifnot(
  !anyDuplicated(life[c("geo", "year")]),
  !anyDuplicated(population[c("geo", "year")]),
  !anyDuplicated(gdp[c("geo", "year")])
)

# Una riga per ogni paese e anno, lasciando NA dove una fonte non ha un dato.
gapminder <- tidyr::crossing(paesi, year = anni) |>
  left_join(life, by = c("geo", "year")) |>
  left_join(population, by = c("geo", "year")) |>
  left_join(gdp, by = c("geo", "year")) |>
  transmute(
    country = factor(country, levels = levels(gapminder::gapminder$country)),
    continent = factor(continent, levels = levels(gapminder::gapminder$continent)),
    year = as.integer(year),
    lifeExp = as.double(lifeExp),
    pop = as.integer(pop),
    gdpPercap = as.double(gdpPercap)
  ) |>
  arrange(country, year)

stopifnot(
  nrow(gapminder) == 142L * length(anni),
  identical(names(gapminder), c("country", "continent", "year",
                                 "lifeExp", "pop", "gdpPercap")),
  dplyr::n_distinct(gapminder$country) == 142L,
  dplyr::n_distinct(gapminder$continent) == 5L,
  identical(sort(unique(gapminder$year)), anni),
  !anyDuplicated(gapminder[c("country", "year")])
)

# Salviamo il dataset (sei colonne) per le due analisi R Markdown.
readr::write_csv(gapminder, "gapminder_1952_2025.csv", na = "")
saveRDS(gapminder, "gapminder_1952_2025.rds")

message("Dataset creato: ", nrow(gapminder), " righe x ", ncol(gapminder), " colonne.")
message("Valori mancanti: ", paste(
  paste(names(gapminder)[4:6], colSums(is.na(gapminder[4:6])), sep = "="),
  collapse = ", "))
