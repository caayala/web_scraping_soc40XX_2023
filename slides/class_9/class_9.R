#| echo = FALSE
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr)
library(knitr)

opts_chunk$set(cache.path = "class_9_files/class_9_cache/html/")



resp <- GET('https://es.wikipedia.org/w/api.php',
            query = list(action = 'query',
                         list = 'search',
                         srsearch = 'web scraping',
                         srlimit = 5,
                         format = 'json'),
            add_headers('Accept-Encoding' = 'gzip'))

# Leo el json que está dentro de la respuesta a la petición.
df <- content(resp, as = 'text') |> 
  jsonlite::fromJSON()

df |> str()



df$query$search |> as_tibble()



resp <- GET('https://es.wikipedia.org/w/api.php',
            query = list(action = 'parse',
                         page = 'Web scraping', # contenido de título de página
                         prop = 'text', # html de retorno
                         format = 'json'),
            add_headers('Accept-Encoding' = 'gzip'))

df <- content(resp, as = 'text') |> jsonlite::fromJSON()

str(df, 3)



df$parse$text$`*` |> 
  read_html() |> html_text() |> 
  cat()



resp <- GET('https://es.wikipedia.org/w/api.php',
            query = list(action = 'query',
                         titles = 'Web scraping|Python', # Varios títulos a la ves.
                         prop = 'info|categories|iwlinks', # Información a obtener.
                         format = 'json'),
            add_headers('Accept-Encoding' = 'gzip'))

df <- content(resp, as = 'text') |> jsonlite::fromJSON()

str(df, 3)



df$query$pages |> 
  enframe() |> 
  unnest_wider(value)



library(WikipediR)

info_wiki <- page_info(
  language = 'es', 
  project = 'wikipedia', 
  page = 'Web_scraping|Python') 



str(info_wiki, 3)



info_wiki$query$pages |> 
  enframe() |> 
  unnest_wider(value)



cont_wiki <-  page_content(language = 'es', 
                           project = 'wikipedia', 
                           page_name = 'Web_scraping')

str(cont_wiki) # Una lisa parse con 4 elementos dentro



cont_wiki$parse$text$`*` |> # Texto del requerimiento
  read_html() |> html_text() |> 
  cat()



s_met <- GET('https://api.spotify.com/v1/search',
             query = list(q = 'genre:metal', 
                          market = 'CL', 
                          type = 'artist', 
                          limit = 8),
             accept_json(),
             add_headers(Authorization = str_glue("Bearer {spotifyr::get_spotify_access_token()}")))

df_met <- s_met |> content('text') |> 
  jsonlite::fromJSON()

df_met$artists$items |> 
  as_tibble() |> unpack(followers, names_repair = 'minimal') |>
  select(id, name, popularity, total)


#| eval: false
#| echo: false
#|
# Para pedir token directamente por la API.
resp_token <- POST("https://accounts.spotify.com/api/token", 
                   accept_json(), 
                   authenticate(Sys.getenv('SPOTIFY_CLIENT_ID'), 
                                Sys.getenv('SPOTIFY_CLIENT_SECRET')), 
                   body = list(grant_type = "client_credentials"),
                   encode = "form")

resp_token |> content()



library(spotifyr)

df_met <- get_genre_artists('metal', limit = 8)
df_met |> 
  select(id, name, popularity, followers.total)



df_pod <- search_spotify(q = 'podcast', type = 'show', market = 'CL', limit = 10)

df_pod |> 
  select(id, name, total_episodes, description)



df_top <- get_playlist('37i9dQZEVXbL0GavIqMTeb') # ID de la lista.
df_top_track <- df_top$tracks$items |> as_tibble()

suppressMessages(
  # Seleccionar y limpiar solo alguna de las variables disponibles
  df_top_track_sel <- df_top_track |> 
    mutate(track.id, track.name, track.popularity, track.album.release_date, 
           name = map(track.album.artists, 'name'),
           .keep = 'none') |> 
    unnest_wider(col = name, names_sep = '_')
)

df_top_track_sel |> head()



df_track_af <- get_track_audio_features(df_top_track_sel$track.id)

head(df_track_af)



df_top_track_sel <- bind_cols(df_top_track_sel, 
                              df_track_af)

gg <- df_top_track_sel |> 
  mutate(pos = row_number()) |>
  pivot_longer(cols = c(danceability, 
                        energy, 
                        speechiness, 
                        acousticness, 
                        liveness),
               names_to = 'variable', 
               values_to = 'valor') |> 
  ggplot(aes(x = variable, 
             y = valor, 
             colour = track.popularity)) +
  geom_point(aes(size = rev(pos)),
             alpha = 0.5,
             show.legend = FALSE) + 
  theme_minimal() +
  labs(title = 'Características de las 100 canciones en Chile',
       subtitle = 'Lista Top 50 — Chile, Spotify', 
       x = NULL)


#| fig-height: 7
gg



library(rtoot)

custom_instance <- 'lile.cl'

lile_instance <- get_instance_general(instance = custom_instance, anonymous = TRUE)

lile_instance |> str(2)



lile_peers <- get_instance_peers(instance = custom_instance, token = NULL, anonymous = TRUE)

head(lile_peers)



lile_peers[grepl('chile', lile_peers)]



lile_peers[grepl('mastodon.social', lile_peers)]



get_instance_trends(
  instance = custom_instance,
  token = NULL,
  limit = 10,
  anonymous = TRUE
)



rtoot(
  endpoint = '/api/v2/search',
  params = list(q = 'twitter'),
  token = Sys.getenv('MSTDN_SOCIAL_KEY'),
  instance = 'mstdn.social',
  anonymous = FALSE
)



toot_stream <- stream_timeline_public(
  timeout = 30,
  local = FALSE,
  file_name = NULL,
  append = TRUE,
  instance = custom_instance,
  token = NULL,
  anonymous = FALSE,
  verbose = TRUE
)



# parse_stream("/var/folders/hr/tjq1vv1s0_l0vn12krk6kq8h0000gn/T//RtmpTd6kNn/stream_tootsf6dd55d17d96.json")


#| include: false

# Extraer código R
knitr::purl('class_9.qmd',
            output = 'class_9.R',
            quiet = TRUE)

