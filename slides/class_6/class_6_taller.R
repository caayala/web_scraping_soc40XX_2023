# title: "Lecturas de sitios web"
# subtitle: "Web Scraping y acceso a datos desde la web"
# author: Cristián Ayala
# date: 2023-06-15

suppressPackageStartupMessages(library(tidyverse))
library(httr)
library(rvest)

# Ejemplo 1: Moodle Ciencias Sociales UC ----
# 
# Login a sitio web

url_outh <- 'https://cursosonline.cienciassociales.uc.cl'

## Usando rvest ----
# 
# Creamos una sesión en con las credenciales de ingreso
s <- session(url = url_outh)

s_form <- html_form(s)[[1]]
s_form <- html_form_set(s_form,
                        username = Sys.getenv('MOODLE_CCSS_UC_ID'),
                        password = Sys.getenv('MOODLE_CCSS_UC_PASS'))

session_submit(s, s_form)


### Cumpleaños ----
# 
# Link para la recepción de cumpleaños
url_captura <- 'https://cursosonline.cienciassociales.uc.cl/course/view.php?id=66'
parse_url(url_captura)

# Crear link para capturar participantes del curso
# 
# URL base
url_portal_render <- parse_url('https://cursosonline.cienciassociales.uc.cl')
# path
url_portal_render$path <- 'user/index.php'
# variables
url_portal_render$query <- list(id = 66)

# Construcción de URL
url_portal_render_web <- build_url(url_portal_render)

url_portal_render_web

# Salto desde sesión a la URL recién construida.
html_web_scraping <- session_jump_to(s,
                                     url_portal_render_web)

# Tabla de participantes.
html_web_scraping |> 
  read_html() |>
  html_elements('.table-dynamic') |> 
  html_table()


### Anuncios ----

url_captura <- 'https://cursosonline.cienciassociales.uc.cl/?redirect=0'
parse_url(url_captura)


html_web_scraping_categoria <- session_jump_to(s,
                                               url_captura)

l_categorias <- html_web_scraping_categoria |> 
  read_html() |>
  html_elements('.category')

df <- tibble(h5 = l_categorias |> 
               html_element('h5') |> 
               html_text2(),
             comentarios = l_categorias |> 
               html_element('h5 .badge-sq ') |> 
               html_text(),
             categoria = l_categorias |> 
               html_element('.with_children') |> 
               html_attr('data-depth'),
             link = l_categorias |> 
               html_element('h5 a') |> 
               html_attr('href'))

df |> 
  head(20)

# Ejemplo 2: Ciudades Amigables ----
# 

# EJERCICIO:
# Capturar la información de esta página
url_home <- 'https://www.ciudadesamigables.cl/comunas-amigables/'

# Certificado de SSH inválido. No ha sido renovado.
session(url_home)

# Puedo leer la web evitando la verificación del certificado.
home_s <- session(url_home,
                  config = config(ssl_verifypeer = FALSE))

status_code(home_s)

home_html <- home_s |> 
  read_html() 

# EJERCICIO:
# ¿Está la tabla de interés?

# No encontramos lo que buscamos:
home_html |> 
  html_elements('.overflow-auto')
  
# No hay un formulario.
html_form(home_html)


# EJERCICIO:
# Buscar el *request* que entrega la información con los datos municipales.

# El servidor entrega resultados desde:
url_consulta <- 'https://www.ciudadesamigables.cl/api/comunas-filters/'

home_input <- home_html |> 
  html_elements('input')

home_input

# EJERCICIO:
# Construir y capturar ese link. ¿Qué sucede?

web_token <- setNames(home_input |> 
                        html_attr('value'),
                      paste0('X-', 
                             home_input |> 
                               html_attr('name')))

x_data <- session_jump_to(home_s,
                          url_consulta,
                          add_headers('X-Requested-With' = 'XMLHttpRequest',
                                      web_token),
                          accept_json(),
                          query = list(page = 1,
                                       status = '1|2|3|4|5|6',
                                       cycle = '1|2'))

json_comunas <- x_data$response |> 
  content(type = 'text') |> 
  jsonlite::fromJSON()

json_comunas$comunas |> 
  as_tibble()

## Captura de toda la información de las comunas ----

# Páginas de comunas
pages <- 1:39

# Función para capturar la información

# EJERCICIO:
# Construir función para capturar las sucesivas páginas con información.

json_page <- function(.page){
  data <- session_jump_to(home_s,
                          url_consulta,
                          add_headers('X-Requested-With' = 'XMLHttpRequest',
                                      web_token),
                          accept_json(),
                          query = list(page = .page,
                                       status = '1|2|3|4|5|6',
                                       cycle = '1|2'))
  data <- data$response |> 
    content(type = 'text') |> 
    jsonlite::fromJSON()
  
  # Pausa para no recargar el servidor.
  Sys.sleep(1)
  
  data$comunas
}

df_comunas <- map_dfr(1:2, 
                      json_page)

df_comunas |> 
  head(3)

df_comunas |> 
  write_excel_csv('slides/class_6/class_6_taller/df_comunas_amigables.csv')


# Ejemplo 3: YouTube Data API ----
# 
# Construcción de llamados a APIs como de YouTube
# https://developers.google.com/youtube/v3/
# 
# Crear credenciales en 
# https://console.cloud.google.com/apis/credentials

f_date_for_api <- function(.date){
  .date |>
    with_tz(tzone = 'UCT') |> # Cambio de zona para pasar de hora chilena a UCT.
    format('%Y-%m-%dT%H:%M:%SZ')
}

## Búsqueda usando key ----
# 
# Documentación:
# https://developers.google.com/youtube/v3/docs/search

response_youtube_key <- GET(
  "https://youtube.googleapis.com/youtube/v3/search",
  query = list(q = 'Encuestas en chile',
               publishedAfter = f_date_for_api(as.Date('2023-04-01')),
               part = 'snippet',
               maxResults = 15,
               order = 'viewCount',
               regionCode = 'CL',
               key = Sys.getenv('YOUTUBE_KEY')),
  accept_json()
)

response_youtube_key

df_videos <- response_youtube_key |> 
  content('text') |> 
  jsonlite::fromJSON()

df_videos |> 
  str(2)

df_videos_items <- df_videos$items |> 
  as_tibble()

df_videos_items |> 
  str(2)

df_videos_items_snippet <- df_videos_items |> 
  unnest_wider(col = c(snippet, id),
               names_repair = 'unique')

df_videos_items_snippet |> 
  str(2)

df_videos_items_snippet |> 
  glimpse()

df_videos_items_snippet |> 
  select(videoId, publishedAt, title)


## Información de videos usando key ----

# Documentación:
# https://developers.google.com/youtube/v3/docs/videos/

l_videos <- df_videos_items_snippet$videoId

response_videos_key <- GET(
  "https://youtube.googleapis.com/youtube/v3/videos",
  query = list(id = paste0(l_videos,
                           collapse = ','),
               part = paste0(c('snippet',
                               'contentDetails',
                               'statistics'),
                             collapse = ','),
               key = Sys.getenv('YOUTUBE_KEY')),
  accept_json()
)

response_videos_key |> 
  str(2)

# Procesamiento de la respuesta
df_videos_key <- response_videos_key |> 
  content(as = 'text') |> 
  jsonlite::fromJSON()

df_videos_stats <- df_videos_key$items |> 
  as_tibble() 

df_videos_stats |> 
  glimpse()

# Abrir las columnas de información
df_videos_stats <- df_videos_stats |> 
  unnest_wider(col = c(snippet,
                       contentDetails, 
                       statistics))

df_videos_stats |> 
  glimpse()


df_videos_stats <- df_videos_stats |> 
  mutate(across(c(viewCount, likeCount, favoriteCount, commentCount),
                as.integer))

df_videos_stats |> 
  ggplot(aes(y = str_trunc(title, 80) |> 
               str_wrap(50) |> 
               fct_reorder(viewCount),
             x = viewCount,
             fill = channelTitle)) +
  geom_col() +
  scale_x_continuous(labels = scales::number) +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(title = NULL,
                             keywidth = unit(3, 'mm'), 
                             keyheight = unit(5, 'mm'), 
                             nrow = 3))


## Búsqueda usando oauth v2.0 ----
# 

# Configurar el flujo con OAuth 2.0
oauth_flow <- httr::oauth_app(
  appname = "YouTube for scraping",
  key = Sys.getenv("YOUTUBE_CLIENT_ID"),
  secret = Sys.getenv("YOUTUBE_CLIENT_SECRET")
)

# Datos para la autentificación
oauth_flow |> str()

# Direcciones web para autentificarse en Google.
httr::oauth_endpoints("google")

# httr provee otras direcciones de servicios habituales.
httr::oauth_endpoints("facebook")

# Pido el código de autorización para esta app.
youtube_token <- httr::oauth2.0_token(oauth_endpoints("google"), 
                                      oauth_flow,
                                      scope = "https://www.googleapis.com/auth/youtube.force-ssl"
)

youtube_token

# Use the access token to access the YouTube Data API
response_youtube_oauth <- GET("https://youtube.googleapis.com/youtube/v3/search",
                              query = list(q = 'Encuestas en chile',
                                           part = 'snippet',
                                           maxResults = 25,
                                           order = 'viewCount'),
                              config(token = youtube_token))

# Print the results
response_youtube_oauth %>%
  content() |> 
  str(2)
