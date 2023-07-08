# title: "Clase 11: GitHub Actions"
# subtitle: "Web Scraping y acceso a datos desde la web"
# author: Cristián Ayala
# date: 2023-07-11

# Cargar paquetes. Solo los necesarios.

library(rvest)

url <- 'https://www.latercera.com/lo-ultimo/page/'

# Función para obtener elementos de interés de cada artículo.
fun_parse_articulos <- function(.html){
  # Captura de artículos dentro de la sección de clase .top-mainy
  html_art <- .html |> 
    html_elements('.top-mainy article')
  
  html_headline <- html_art |> 
    html_element('.headline')
  
  list(
    titulo      = html_headline |> html_text(),
    url_noticia = html_headline |> html_element('a') |> html_attr('href'),
    tag         = html_headline |> html_element('.tag') |> html_text(),
    texto       = html_art |> html_element('.deck p') |> html_text(),
    url_imagen  = html_art |> html_element('img') |> html_attr('src'),
    periodista  = html_art |> html_element('.byline') |> html_text(),
    tiempo      = html_art |> html_element('.time') |> html_text()
  ) |> 
    as.data.frame()
}


# Prueba de página 1 ----

if(FALSE){
  # Lectura de sitio web
  html_1 <- read_html(paste0(url, '1'))
  
  df_1 <- fun_parse_articulos(html_1) 
}


# Lectura de 10 páginas ----

l_url <- paste0(url, 1:10)

# Lectura de las 10 páginas.
l_html <- lapply(l_url, 
                 function(.url){
                   Sys.sleep(1) # Pausa de 1 segundo
                   read_html(.url)
                 })


# Procesamiento ----
l_contenido <- lapply(l_html,
                      fun_parse_articulos)

# Generar solo una tabla.
df_contenido <- do.call(rbind, 
                        l_contenido)


# Correcciones ----

# Remover texto contenido dentro de nodos.
fun_remove <- function(contenido, texto_a_rm){
  lapply(seq_along(contenido), 
         \(i) sub(texto_a_rm[[i]], "", contenido[[i]])
  ) |> 
    unlist()
}

# Quitar texto de tiempo en el campo de periodistas.
df_contenido$periodista <- fun_remove(df_contenido$periodista, df_contenido$tiempo)
# Quitar texto de tag en el campo de título.
df_contenido$titulo <- fun_remove(df_contenido$titulo, df_contenido$tag)

# Fecha de captura.
df_contenido$fecha_captura <- format(Sys.Date(), '%Y-%m-%d')


# Grabación ----

# Guardar datos en archivo de texto.
# Se agregan al final del archivo creado en la primera captura de información.
write.table(df_contenido,
            file = 'slides/class_11/class_11_files/df_contenido.csv',
            quote = TRUE,
            row.names = FALSE,
            qmethod = 'double',
            fileEncoding = "utf8",
            sep = ';',
            dec = ',',
            append = TRUE)

# Para leer archivo
# df_test <- read.csv('slides/class_11/class_11_files/df_contenido.csv',
#                     header = TRUE,
#                     sep = ';') |> 
#   tibble::as_tibble()


# Preparar GitHub Actions ----

# Ayuda desde usethis::use_github_action('check-release')

# usethis::use_github_action('check-release')
# 
# > usethis::use_github_action('check-release')
# ✔ Setting active project to '~/Dropbox (DESUC)/Documentos/Clases/UC - Web scraping 2023/web_scraping_soc40XX_2023'
# ✔ Creating '.github/'
# ✔ Adding '^\\.github$' to '.Rbuildignore'
# ✔ Adding '*.html' to '.github/.gitignore'
# ✔ Creating '.github/workflows/'
# ✔ Saving 'r-lib/actions/examples/check-release.yaml@v2' to '.github/workflows/R-CMD-check.yaml'
# • Learn more at <https://github.com/r-lib/actions/blob/v2/examples/README.md>.
# ✔ Adding R-CMD-check badge to 'README.Rmd'
# • Re-knit 'README.Rmd' with `devtools::build_readme()`
