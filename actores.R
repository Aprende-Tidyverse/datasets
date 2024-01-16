library(tidyverse)
library(rvest)
library(janitor)
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

datos_peliculas <- read_rds("datos_peliculas.rds")
urls <- paste0("https://www.boxofficemojo.com/release/",datos_peliculas$id)

extract_actors <- function(url){
  p()
  page <-read_html(url)
  
  url_movie <- page |> 
    html_element(
      xpath = "//div[@class = 'a-box-inner']//a[contains(@href,'tt')]"
    ) |> 
    html_attr("href")
  
  actor_id <- read_html(url_movie) |> 
    html_elements(
      xpath = "//div[@id = 'title_cast_sortable_table_wrapper']//span/a[contains(@href,'nm')]"
    ) |> 
    html_attr("href")
  
  df <- read_html(url_movie) |> 
    html_table() |> 
    keep_at(2) |> 
    list_rbind() |> 
    separate_wider_delim(
      cols = Name,
      delim = "\n\n",
      names = c("name", "character"),
      too_many = "drop"
    ) |> 
    clean_names() |> 
    rename(
      actor = name,
      personaje = character,
      conocido_por = known_for
    )
  
  df <- df |> 
    mutate(
      id_pelicula = str_extract(url, "rl\\d+"),
      id_actor = str_extract(actor_id, "nm\\d+"),
      .before = 1
    )
  return(df)
}

extract_actors <- possibly(extract_actors)

with_progress({
  p <- progressr::progressor(along = urls)
  df <- future_lapply(urls, extract_actors)
})

df <- list_rbind(df)

pelicula_actor <- select(
  df,
  id_pelicula, id_actor, personaje
)

write_rds(pelicula_actor,"pelicula_actor.rds")
actor <- select(
  df,
  id_actor, 
  actor
) |> 
  filter(
    !duplicated(id_actor)
  )
write_rds(actor,"actor.rds")

