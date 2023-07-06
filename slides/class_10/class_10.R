#| echo: false
#| warning: false
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr)
library(knitr)

opts_chunk$set(cache.path = "class_10_files/class_10_cache/html/")



url <- 'https://admision.mineduc.cl/vitrina-vue/'

read_html(url) |> 
  html_element('body')



# remotes::install_github("rstudio/chromote") # versión de desarrollo.
library(chromote)
try(b <- ChromoteSession$new()) # Nuevo navegador.
try(b1 <- b$new_session()) # Nueva "ventana".

# Ir a sitio web de interés
try(b1$Page$navigate("https://admision.mineduc.cl/vitrina-vue/"))
Sys.sleep(5) # Tiempo para que la página cargue.



# Se puede ver lo que hace esta "ventana":
if(interactive()){
  b1$view()
}



try(x1 <- b1$DOM$getDocument()) # Documento en R

try(x1$root |> str(2))



try(select_reg  <- b1$DOM$querySelectorAll(x1$root$nodeId, "select[id=select-region]"))
try(select_com  <- b1$DOM$querySelectorAll(x1$root$nodeId, "select[id=select-comuna]"))
try(opt_reg_ids <- b1$DOM$querySelectorAll(x1$root$nodeId, "select[id=select-region] option"))

try(opt_reg_ids$nodeIds |> unlist())



try(
  b1$DOM$getOuterHTML(opt_reg_ids$nodeIds[[1]])[[1]] |>
    read_html() |>
    html_text()
)



try(b1$DOM$getAttributes(opt_reg_ids$nodeIds[[2]])) # segunda opción



try(l_region_chr <- map(opt_reg_ids$nodeIds,
                        \(x) b1$DOM$getOuterHTML(x)[[1]] |> 
                          read_html()))

try(length(l_region_chr))



map_chr(l_region_chr, html_text)



b1$DOM$getAttributes(opt_reg_ids$nodeIds[[2]])$attributes |> unlist()



try(l_region_int <- map_chr(opt_reg_ids$nodeIds,
                            \(x) b1$DOM$getAttributes(x)$attributes[[4]]))
try(length(l_region_int))



try(l_region_int)



# A google
b1$Page$navigate(url = 'https://www.google.cl')



# Historia
nav_history <- b1$Page$getNavigationHistory()

nav_history |> str()



# Vuelvo a página de inicio
b1$Page$navigateToHistoryEntry(entryId = nav_history$entries[[2]]$id)



try(b1$parent$close())



comuna <- 'ALTO_HOSPICIO'

resp <- GET(url = 'https://apisae.mineduc.cl/sae-api-vitrina/v1/establecimientos',
            query = list(comuna = comuna),
            accept_json(),
            add_headers('accept-encoding' = 'gzip'))



resp |> content('text') |> jsonlite::fromJSON() |> str(1)



f_comuna_escuelas <- function(.comuna){
  # Recepción de la respuesta
  response <- GET(url = 'https://apisae.mineduc.cl/sae-api-vitrina/v1/establecimientos',
                  query = list(comuna = comuna),
                  accept_json(),
                  add_headers('accept-encoding' = 'gzip'))
  
  response |> 
    content('text', encoding = 'UTF-8') |> 
    jsonlite::fromJSON()
}



js_comunas <- read_html('https://admision.mineduc.cl/vitrina-vue/static/js/app.08fc6ac089af64dc7303.js')

comunas <- js_comunas |> html_text() |> 
  str_extract_all('nombreComuna:\"(.*?)\"')

comunas[[1]] |> 
  str_remove_all('nombreComuna:\\\"|\\\"') |> 
  head()



comunas <- comunas[[1]] |> 
  str_remove_all(r'(nombreComuna:\"|\")') # R 4.0 soporta "raw strings"

comunas |> head()



comunas[1:20]



df <- f_comuna_escuelas(comunas[1])



l_escuelas <- map(comunas[1:3], 
                  f_comuna_escuelas)

df_escuelas <- l_escuelas |> 
  list_rbind() |> as_tibble()

df_escuelas |> head()


#| echo: true
f_rbd_detalle <- function(.rbd){
  jsonlite::fromJSON(str_glue(
    'https://apisae.mineduc.cl/sae-api-vitrina/v1/establecimientos/{.rbd}/'
  ))
}

df_rbd_detalle <- f_rbd_detalle(
  df_escuelas$rbd[[1]]
  )


#| echo: true
df_rbd_detalle |> str(1)


#| cache = TRUE
l_rbd_detalle <- map(df_escuelas$rbd[1:3], 
                     f_rbd_detalle)

df_rbd_detalle <- l_rbd_detalle |> 
  enframe() |> 
  unnest_wider(value)

head(df_rbd_detalle)


#| echo = FALSE,
#| include = FALSE

# Extraer código R
knitr::purl('class_10.qmd',
            output = 'class_10.R',
            quiet = TRUE)

