library(tidyverse)

pelis <- read_rds("peliculas.rds")
pelis

df <- filter(
  pelis, 
  anio == 2022
)

df

df <- arrange(
  df,
  desc(ingresos)
)

view(df)
