library(tidyverse)
library(rvest)
library(glue)
library(janitor)
library(cli)
library(rlang)
library(future.apply)
library(progressr)

plan("future::multisession")
Sys.setenv("_R_USE_PIPEBIND_" = TRUE)

handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )
))

years<- 1990:2023

movies <- map(years, function(year){
  url <- glue("https://www.boxofficemojo.com/year/{year}")
  cli_status(url)
  page <- read_html(url)
  url_movies <-  page |> 
    html_elements(xpath = "//table//a[contains(@href, 'rl')]") |>
    html_attr("href") |> 
    x => glue("https://www.boxofficemojo.com{x}")
  
  
  df_movies <- html_table(page)[[1]] |> 
    clean_names() |> 
    add_column(
      url = url_movies
    ) |> 
    add_column(
      year = year, 
      .before = 1
    )
  
  return(df_movies)
}) |> 
  list_rbind()


df_movies


get_details <- function(url_movie){
  p()
  
  key <- read_html(url_movie) |> 
    html_elements(xpath = "//div[contains(@class, 'mojo-summary-values')]//div/span[1]") |> 
    html_text2()
  
  value <- read_html(url_movie) |> 
    html_elements(xpath = "//div[contains(@class, 'mojo-summary-values')]//div/span[2]")  |> 
    html_text2()
  
  
  x <- tibble(
    id = str_extract(url_movie, "rl\\d+"),
    key = key,
    value = value
  ) |> 
    pivot_wider(id_cols = id, names_from = key, values_from = value)|> 
    clean_names() 
  
  tibble(
    id = x$id,
    presupuesto = x$budget %||% NA,
    clasificacion = x$mpaa %||% NA,
    duracion = x$running_time %||% NA,
    genero = x$genres %||% NA,
    dias_en_cine = x$in_release %||% NA
  )
}

with_progress({
  p <- progressr::progressor(along = movies$url)
  df <- future_lapply(movies$url, get_details)
})

final <- movies |> 
  mutate(
    id = str_extract(url, "rl\\d+")
  ) |> 
  left_join(x, join_by(id == id)) |> 
  filter(!duplicated(id))


datos_peliculas <- final |> 
  rename(
    productora = distributor,
    pelicula = release,
    ingresos = total_gross,
    n_cines = theaters
  ) |> 
  mutate(
    fecha_estreno = paste0(year, release_date),
    fecha_estreno = ymd(fecha_estreno),
    ingresos = parse_number(ingresos),
    n_cines = parse_number(n_cines),
    anio = year(fecha_estreno)
  ) |> 
  select(
    id,
    productora, 
    pelicula, 
    genero,
    ingresos,
    n_cines,
    dias_en_cine,
    duracion,
    clasificacion,
    fecha_estreno,
    year
  ) 
  

urls <- paste0("https://www.boxofficemojo.com/release/",datos_peliculas$id)

url <- urls[1]

extract_score <- function(url){
  p()
  url_imdb <- read_html(url) |> 
    html_element(xpath = "//div[@id = 'title-summary-refiner']//a") |>
    html_attr("href")  |> 
    x => glue("https://www.imdb.com{x}")
  
  p <- read_html(url_imdb)
  
  id <- str_extract(url, "rl\\d+")
  score <- p |> 
    html_element(xpath = "//div[@data-testid = 'hero-rating-bar__aggregate-rating__score']/span") |> 
    html_text2() |> 
    as.numeric()
  
  popular <- p |> 
    html_element(xpath = "//div[@data-testid = 'hero-rating-bar__popularity__score']") |> 
    html_text2() |> 
    parse_number()
  
  votes <- p |> 
    html_element(xpath = "//div[@class = 'sc-bde20123-3 gPVQxL']") |> 
    html_text2()
  
  tibble(
    id,
    score,
    popular,
    votes
  )
}

### presupuesto

urls <- paste0("https://www.boxofficemojo.com/release/",datos_peliculas$id)

extract_director <- function(url){
  p()
  url_imdb <- read_html(url) |> 
    html_element(xpath = "//div[@id = 'title-summary-refiner']//a") |>
    html_attr("href")  |> 
    x => glue("https://www.imdb.com{x}")
  
  p <- read_html(url_imdb)
  id <- str_extract(url, "rl\\d+")
  presupuesto <- p |> 
    html_elements(xpath = "//li[@data-testid = 'title-boxoffice-budget']//span") |> 
    html_text2() |> 
    keep_at(2) 
  
  tibble(id, presupuesto)
}

extract_director <- possibly(extract_director)

with_progress({
  p <- progressr::progressor(along = urls)
  df <- future_lapply(urls, extract_director)
})

df <- list_rbind(df)

df <- df |> 
  mutate(
    unit = str_trim(str_extract(presupuesto, "^\\D+")),
    presupuesto = parse_number(presupuesto),
    presupuesto = presupuesto * conversion_rates_to_usd[unit]
  ) |> 
  select(id,presupuesto)

df <- datos_peliculas |> 
  select(-presupuesto) |> 
  left_join(df) |> 
  filter(!duplicated(id))

df <-  df |> 
  relocate(
    presupuesto,
    .after = ingresos_extrajero
  )
glimpse(df)

write_rds(df, "datos_peliculas.rds")
  
df |> 
  select(
    id, pelicula,
    genero,
    ingresos, domestico, internacional,
    presupuesto,
    n_cines, dias_en_cine,
    duracion,
    edad, 
    calificacion,
    votos,
    popularidad,
    productora,
    director,
    fecha_estreno
  ) 

datos_peliculas|> 
  filter(
    !str_detect(pelicula, "Release|Edition|Anniversary"),
    !presupuesto < 10000
  ) |> 
  write_rds("datos/datos_peliculas.rds")
extract_score <- possibly(extract_score)

with_progress({
  p <- progressr::progressor(along = urls)
  df <- future_lapply(urls, extract_score)
})
conversion_rates_to_usd <- c(
  "$"   = 1,        # Dólar estadounidense a USD
  "£"   = 1.22,     # Libras esterlinas a USD
  "CA$" = 0.75,     # Dólares canadienses a USD
  "A$"  = 0.71,     # Dólares australianos a USD
  "NOK" = 0.10,     # Coronas noruegas a USD
  "FRF" = 0.15,     # Francos franceses a USD (moneda histórica)
  "IEP" = 1.28,     # Libras irlandesas a USD (moneda histórica)
  "DKK" = 0.14,     # Coronas danesas a USD
  "CHF" = 1.05,     # Francos suizos a USD
  "HK$" = 0.13,     # Dólares de Hong Kong a USD
  "DEM" = 0.51,     # Marcos alemanes a USD (moneda histórica)
  "¥"   = 0.0077,   # Yenes japoneses a USD
  "₹"   = 0.013,    # Rupias indias a USD
  "€"   = 1.05,     # Euros a USD
  "R$"  = 0.19,     # Reales brasileños a USD
  "CN¥" = 0.15,     # Yuanes chinos a USD
  "THB" = 0.03,     # Baht tailandés a USD
  "₩"   = 0.00083   # Won surcoreano a USD
)

df <- list_rbind(df)

df |> 
  
units <- c("K" = 1000, "M" = 1000000)

df <- df |> 
  mutate(
    .unit = str_extract(votes, "\\D$"),
    .v2 = parse_number(votes),
    .v3 = .v2 * units[.unit],
    votes = if_else(is.na(.v3),.v2,.v3)
  ) |> 
  select(id,votos = votes)

x <- left_join(
  datos_peliculas |> 
    select(-votos),
  df
)

x <- x |> 
  relocate(
    votos, 
    .after = calificacion
  )
x |> 
  rename(
    calificacion = score,
    popularidad = popular,
    votos = votes,
    edad = clasificacion
  ) |> 
  select(
    id,
    productora,
    pelicula,
    genero, 
    ingresos, 
    n_cines,
    dias_en_cine,
    duracion,
    edad,
    calificacion,
    votos,
    popularidad,
    fecha_estreno
  ) |> 
  filter(!duplicated(id)) |> 
  mutate(
    votos = parse_number(votos)*1000,
    edad = if_else(
      is.na(edad),
      true = "desconocido",
      edad
    )
  )


  x |>
    filter(!str_detect(pelicula,"Re-release")) |> 
    write_rds("datos_peliculas.rds")
