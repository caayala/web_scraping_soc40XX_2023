#| echo: false
suppressPackageStartupMessages(library(tidyverse))
library(knitr)

# opts_chunk$set(cache.path = "class_1_files/class_1_cache/html/")



library(rvest)
url <- 'class_1_files/mi_primer_scraping.html'

if(interactive()){
  url <- 'slides/class_1 test/class_1_files/mi_primer_scraping.html'
}

html <- read_html(x = url)
html



html |> html_element('body')



html |> html_element('p')



html |> html_element('#first') |> html_text()



html |> html_element('img') |> html_attr('src')



html |> html_element('img') |> html_attrs()



html |> html_element('body') |> html_children() |> html_name()


#| cache = TRUE
url <- 'https://es.wikipedia.org/wiki/Star_Wars'
html <- read_html(url)

df_tablas <- html |> 
  html_elements('table.wikitable') |>  html_table()

df_tablas |> str(1)



df_tablas[[6]] |> head(3) # Recaudación



df_tablas[[7]] |> head(3) # Evaluación



df_recaudacion <- df_tablas[[6]]
names(df_recaudacion) <- as.character(df_recaudacion[1, ])
  
df_recaudacion <- df_recaudacion |> 
  filter(str_detect(Película, 'Star Wars')) |> 
  mutate(across(3:5, \(x) str_remove_all(x, '\\.')), # Eliminar '.' en números
         across(3:5, as.integer))


#| echo = FALSE
df_recaudacion$Película <- df_recaudacion$Película |> 
  str_remove('Star Wars: ') |> 
  str_extract('(?<= - ).*(?=\\[)|(^.*(?=:))')

df_recaudacion$Película[df_recaudacion$Película == 'The Empire Strikes Back'] <- 'Empire Strikes Back'
df_recaudacion$Película[df_recaudacion$Película == 'Rise of Skywalker']       <- 'The Rise of Skywalker'

df_recaudacion # Dejo fuera "The Clone Wars"



df_critica <- df_tablas[[7]]
names(df_critica) <- as.character(df_critica[1, ])
  
df_critica <- df_critica |> 
  filter(!(Película %in% c('Película', 'Promedio'))) |> 
  mutate(across(2:7, \(x) str_extract(x, '\\d.?\\d')), # capturo las notas
         across(6:7, \(x) str_replace(x, '\\,', '\\.')), # reemplazo ',' por '.'
         across(2:5, as.integer)) # transformo strings a números.



df_critica


#| fig.dim=c(7, 4)
df_cyr <- left_join(df_critica, df_recaudacion, by = 'Película')

ggplot(df_cyr,
       aes(x = Total, y = General)) + 
  geom_point(size = rel(3)) +
  ggrepel::geom_label_repel(aes(label = Película)) +
  scale_x_continuous('Millones de dólares', labels = ~scales::dollar(., scale = 0.000001)) + 
  labs(title = 'Star Wars: Relación entre recaudación total y crítica (Rottentomatos)') +
  theme_minimal()


#| echo = FALSE,
#| include = FALSE

# Extraer código R
knitr::purl('class_1.qmd',
            output = 'class_1.R',
            quiet = TRUE)

