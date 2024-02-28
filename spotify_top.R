library(tidyverse)
library(httr2)
library(glue)

api_key <- "BQDbkDWbx7MpwZhi6Gx-sSbl0mq5hPLBlmECc68b1fx7FaFcQZlgQc2Klz2zm-9p51mGOCDXjOXd5xSGpNnC3n1QiVoxoQ-MoDZDEBJV0ycrv5odVE0"

playlist <- c(
  "canada" = "37i9dQZEVXbKj23U1GF4IR",
  "estados unidos" = "37i9dQZEVXbLRQDuF5jeBp",
  "mexico" = "37i9dQZEVXbO3qyFxbkOE1",
  "costa rica" = "37i9dQZEVXbMZAjGMynsQX",
  "panama" = "37i9dQZEVXbKypXHVwk1f0",
  "colombia" = "37i9dQZEVXbOa2lmxNORXQ",
  "brasil" = "37i9dQZEVXbMXbN3EUUhlg",
  "chile" = "37i9dQZEVXbL0GavIqMTeb",
  "peru" = "37i9dQZEVXbJfdy5b0KP7W",
  "argentina" = "37i9dQZEVXbMMy2roB9myp",
  "alemania" = "37i9dQZEVXbJiZcmkrIHGU",
  "francia" = "37i9dQZEVXbIPWwFssbupI",
  "reino unido" = "37i9dQZEVXbLnolsZ8PSNw",
  "italia" = "37i9dQZEVXbIQnj7RRhdSX",
  "españa" = "37i9dQZEVXbNFJfN1Vw8d9",
  "rusia" = "5EqWNbNzXLPl3TGF21GZtO",
  "ucrania" = "37i9dQZEVXbKkidEfWYRuD",
  "hongkong" = "37i9dQZEVXbLwpL8TjsxOG",
  "india" = "37i9dQZEVXbLZ52XmnySJg",
  "japon" = "37i9dQZEVXbKXQ4mDTEBXq",
  "corea" = "37i9dQZEVXbNxXF4SkHj9F",
  "arabia saudi" = "37i9dQZEVXbLrQBcXqUtaC",
  "israel" = "37i9dQZEVXbJ6IpvItkve3",
  "nigeria" = "37i9dQZEVXbKY7jLzlJ11V",
  "egipto" = "37i9dQZEVXbLn7RQmT5Xv2",
  "subafrica" = "37i9dQZEVXbMH2jvi6jvjk",
  "nueva zelanda" = "37i9dQZEVXbM8SIrkERIYl"
)

get_playlist <- function(playlist){
  x <- request(glue("https://api.spotify.com/v1/playlists/{playlist}")) |>
    req_headers(
      `Content-Type` = "application/json",
      `Authorization` = glue("Bearer {api_key}")
    ) |> 
    req_perform() |> 
    resp_body_json()
  
  df <- map(x$tracks$items, function(track){
    tibble(
      pais = str_remove(x$name,"Top 50 - "),
      album = track$track$album$name,
      fecha_lanzamiento_album = track$track$album$release_date,
      cantidad_canciones_album = track$track$album$total_tracks,
      id_album = track$track$album$id,
      cancion = track$track$name,
      artista = map_chr(track$track$artists,~.x$name),
      id_artista = map_chr(track$track$artists,~.x$id),
      duracion  = track$track$duration_ms,
      popularidad = track$track$popularity,
      id = track$track$id
    )
  }) |> 
    list_rbind() |> 
    mutate(
      posicion = consecutive_id(cancion),
      .before = 1
    ) |> 
    mutate(
      across(pais:artista, str_to_title)
    )
  
  uri_track <- df |> 
    distinct(id) |> 
    pull(id) |> 
    paste0(collapse = ",") |> 
    URLencode(reserved = TRUE)
  
  x <- request(glue("https://api.spotify.com/v1/audio-features?ids={uri_track}")) |>
    req_headers(
      `Content-Type` = "application/json",
      `Authorization` = glue("Bearer {api_key}")
    ) |> 
    req_perform() |> 
    resp_body_json()
  
  df2 <- x$audio_features |> 
    map(as_tibble) |> 
    list_rbind() 
  
  left_join(df,df2)
}

df <- map(playlist, get_playlist)

df <- list_rbind(df)

cancion <- df |> 
  select(
    id,
    titulo = cancion,
    id_album
  )

df |> 
  select(
    id = id_album,
    nombre = album,
    id_
  )



