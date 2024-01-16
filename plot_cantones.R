library(tidyverse)
library(rvest)
library(janitor)
library(ggridges)

url <- "https://es.wikipedia.org/wiki/Anexo:Cantones_de_Costa_Rica_por_población"

p <- read_html(url)
p


df <- p |> 
  html_table() |> 
  keep_at(2) |> 
  flatten_df() |> 
  clean_names()

glimpse(df)

convertir_numero <- function(text){
  if_else(
    condition = str_detect(text, "\\,"),
    true = parse_number(text, locale = locale(grouping_mark = ",")),
    false = parse_number(text, locale = locale(grouping_mark = " "))
  )
}

df |> 
  mutate(
    across(
      .cols = area_km2:densidad_poblacion,
      .fns = convertir_numero
    )
  ) |> 
  select(provincia, canton, contains("total_")) |> 
  pivot_longer(
    cols = 3:4, 
    names_to = "variable",
    values_to = "valor",
    names_prefix = "total_"
  ) |> 
  summarise(
    valor = sum(valor, na.rm = TRUE),
    .by = c(provincia, variable)
  ) |> 
  mutate(
    proc
  )

df  



ggplot(
  data = df_plot,
  mapping = aes(
    x = p,
    y = fct_reorder(provincia, p),
    fill = variable
  )
) +
  geom_col() +
  scale_color_okabe_ito() +
  theme_minimal()






















