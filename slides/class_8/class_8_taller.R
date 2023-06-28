# title: "Clase 8: Taller de dplyr y googlesheet4"
# subtitle: "Web Scraping y acceso a datos desde la web"
# author: Cristián Ayala
# date: 2023-06-29

suppressPackageStartupMessages(library(tidyverse))
library(labelled)
library(knitr)


# Ejemplo 1: Programación con dplyr ----

df <- data.frame(var_1 = c(1, 2, 3, 4),
                 var_2 = c(9, 1, 1, 1),
                 var_x = c('a', 'a', 'a', 'b'))

# Creación de variable programática
var_1 <- 'var_2'

## Data masking ----
# 
# ¿Cómo distingo una variable programática de una variable en una base de datos (df)?
# ¿Cómo accedo a la columna en una data.frame?

### En R base ----
df[['var_1']]

df$var_1 # Acá hay algo raro!
         # R revisa primero en la data.frame si existe columna llamada `var_1`.

df[[var_1]] # Aquí entiende `var_1` como una variable en el sistema.

# Calcular una nueva variable
df[['var_1']] / df[['var_2']]

df$var_1 / df$var_2 # tengo que indicar desde donde viene la variable.


### En dplyr ----
pull(df, var_1) # ¿Cómo distingue entre la columna en df y la variable?

pull(df, 'var_1') 

pull(df, !!var_1) # Evaluar explicitamente var_1 para indicar que es una variable con !! (bang bang)

# Calcular una nueva variable
mutate(df, v_div = var_1 / var_2, .keep = 'none')

try(mutate(df, v_div = 'var_1' / 'var_2', .keep = 'none')) # Error: va a crear una variable dividiendo characters.

try(mutate(df, v_div = !!var_1 / var_2, .keep = 'none')) # Extra: no funciona porque lo evalúa como character.

mutate(df, v_div = !!sym(var_1) / var_2, .keep = 'none') # Extra: ¡funciona! Es un símbolo de de la variable.


### Programación ----
# 
# El desafío es cuando quiero hacer una función.

head(mtcars)

# Quiero saber `mpg`, `disp` y `hp` promedio por cilindrada `cyl`.
# 
# Un ejemplo para ello:
mtcars |> 
  group_by(cyl) |> 
  summarise(mpg = mean(mpg))

mtcars |> 
  summarise(mpg = mean(mpg),
            .by = cyl) # dplyr 1.1


# Una función que tome una variable y haga esto.
mean_var_1 <- function(.df, .var){
  .df |> 
    group_by(cyl) |> 
    summarise(.var = mean(.var))
}

# La tentación natural, no funciona.
mtcars |> 
  mean_var_1(mpg)

# El siguiente paso, tampoco.
mtcars |> 
  mean_var_1('mpg')


# Se debe considerar que tipo de variable es la que paso a la función.
# Es el nombre de una de las columnas
mean_var_2 <- function(.df, .var){
  .df |> 
    group_by(cyl) |> 
    summarise(.var = mean({{ .var }})) # embrace .var
}

# Bien: pero... ¿qué pasa con el nombre?
mtcars |> 
  mean_var_2(mpg)


mean_var_3 <- function(.df, .var){
  .df |> 
    group_by(cyl) |> 
    summarise("{{ .var }}" := mean({{ .var }})) # ojo con ":="
}

# Función completa. 
mtcars |> 
  mean_var_3(mpg)

# Podemos usar la función creada para procesar varias variables a la vez.

# Vector de caracteres
var_chr <- c('mpg', 'disp', 'hp')

map(var_chr, ~mean_var_3(mtcars, .)) # Error: No interpreta la variable.

map(var_chr, ~mean_var_3(mtcars, !!.)) # Error: Interpreta la variable como texto.

# De characters a symbol `syms`
var_chr_syms <- syms(var_chr) 

map_dfr(var_chr_syms, ~mean_var_3(mtcars, !!.))


# Vector de variables en la data.frame.
try(var_exprs <- c(mpg, disp, hp)) # No conoce esas variables.

# De variables a symbol `exprs`
var_exprs <- rlang::exprs(mpg, disp, hp) 

map_dfr(var_exprs, ~mean_var_3(mtcars, !!.)) # Error: Interpreta la variable como texto.



### Usando `across()` ----

# Extra: método más rápido para considerar varias variables a la vez.
mtcars |> 
  group_by(cyl) |> 
  summarise(across(c(mpg, disp, hp), mean))

mean_var_4 <- function(.df, .vars){
  .df |> 
    group_by(cyl) |> 
    summarise(across({{ .vars }}, mean))
}

mtcars |> 
  mean_var_4(c(mpg, disp, hp))

### Ejercicio ----
# 
# Crear función `mean_var_5` basada en `mean_var_4` que tenga un argumento adicional 
# para modificar la variable de agrupación

mean_var_5 <- function(.df, .vars, .group){
  
  
  
}

mtcars |> 
  mean_var_5(mpg, carb)


# Tip para funciones sencillas: *passing the dots*

mean_var_6 <- function(.df, ...){
  .df |> 
    group_by(cyl) |> 
    summarise(across(c(...), mean))
}

mtcars |> 
  mean_var_6('mpg', disp, 'hp')



# Ejemplo 2: Análisis de datos en Google Sheets ----
# 

library(googledrive)
library(googlesheets4)

# 1. Leer los datos de Bicentenario
# 
url_sheet <- 'https://docs.google.com/spreadsheets/d/1rujigrFqSaSLjsSvrMZY4IU3CSLiM_ymd-wtst9BYNQ/'
# 
# Base de respuestas
df_base <- read_sheet(ss = url_sheet, sheet = 'bicentenario_2020.csv')

head(df_base)

# Base de variables
df_var  <- read_sheet(url_sheet, sheet = 'variables', col_types = 'c')

head(df_var)


# Cambio de nombre de variables y agrego etiquetas
# 
# Visto en clase pasada
df_var_orden <- df_var[match(names(df_base), df_var$bicentenario_2020.csv), ]

names(df_base) <- df_var_orden$variables
var_label(df_base) <- df_var_orden$bicentenario_2020.csv

head(df_base, 2)

var_label(df_base)


## Perdidos ----
# Sustituir valores 9 por NA en las preguntas de interés.
table(df_base$s1_1, useNA = 'ifany')

df_base <- df_base |> 
  mutate(across(starts_with('s1'), ~na_if(., 9)))


## Categoría de respuestas ----
# 
df_niv <- read_sheet(url_sheet, sheet = 'niveles')
  
df_niveles <- df_niv |> 
  tidyr::nest(.by = tipo,
              .key = 'niveles') |> 
  rowwise() |> 
  mutate(niveles_vec = list(pull(niveles, valor, etiqueta)))


v_values <- df_var_orden |> 
  filter(!is.na(tipo)) |> 
  pull(tipo, variables)

# Lista de variables con sus etiquetas.

value_list <- structure(df_niveles$niveles_vec[match(as.character(v_values), 
                                                 df_niveles$tipo)],
                        names = names(v_values))
value_list

# Agregar valores según vector anterior.

df_base <- df_base |> 
  labelled::set_value_labels(!!!value_list)


## Rango de preguntas ----
df_base |> 
  reframe(across(starts_with('s1'), 
                 \(x) range(x, na.rm = TRUE)))


# Función para cálculo de promedio de preguntas según variable de segmentación.
f_mean_seg_vars_1 <- function(.df, .seg, .vars){
  .df |> 
    group_by({{ .seg }}) |> 
    summarise(across({{ .vars }}, 
                     \(x) mean(x , na.rm = TRUE)))
}

df_base |> 
  f_mean_seg_vars_1(tramo_edad, .vars = c(s1_1, s1_2, s1_3))

# Quiero una función para graficar -> tidydata

f_mean_seg_vars_2 <- function(.df, .seg, .vars){
  df <- .df |> 
    group_by({{ .seg }}) |> 
    summarise(across({{ .vars }}, 
                     \(x) mean(x , na.rm = TRUE)))
  
  df |> 
    pivot_longer(cols = {{ .vars }},
                 names_to = 'variable', 
                 values_to = 'mean')
}

df_base |> 
  f_mean_seg_vars_2(tramo_edad, 
                    .vars = c(s1_1, s1_2, s1_3))

# Agregar etiqueta de la variable sería útil

f_mean_seg_vars_3 <- function(.df, .seg, .vars){
  
  l_labels <- var_label(.df) |> 
    unlist()
  
  df <- f_mean_seg_vars_2(.df, 
                          .seg = {{ .seg }}, 
                          .vars = {{ .vars }})
  
  df |> 
    mutate(var_label = l_labels[variable],
           var_label = str_extract(var_label, '(?<= \\().*(?=\\))'),
           .after = variable)
}

df_mean_edad <- df_base |> 
  f_mean_seg_vars_3(tramo_edad, .vars = c(s1_1, s1_2, s1_3))

## Guardar información en googlesheet

df_mean_edad$tramo_edad <- as_factor(df_mean_edad$tramo_edad)

write_sheet(df_mean_edad,
            ss = url_sheet,
            sheet = 'caayala') # poner nombre de usuario de su correo electrónico aquí.


## Subir archivo a gogledrive ----

# Creo un gráfico
ggplot(df_mean_edad,
       aes(y = tramo_edad,
           x = mean)) +
  geom_col(fill = '#f15e52') +
  facet_wrap(facets = vars(var_label),
             dir = 'v') +
  coord_cartesian(xlim = c(1, 4),
                  expand = FALSE) + 
  theme(strip.placement = 'outside',
        strip.text = element_text(size = 15)) +
  labs(title = 'Gráfico de prueba',
       subtitle = str_wrap('Le voy a enumerar una serie de metas que Chile se ha propuesto para el futuro. Pensando en un plazo de 10 años, ¿Ud. cree que se habrán alcanzado estas metas, se habrá avanzado, se seguirá igual que ahora o se habrá retrocedido?',
                           60),
       y = 'Tramos de edad') + 
  theme_minimal() +
  theme(plot.subtitle = element_text(size = rel(.6)),
        plot.title.position = 'plot')

# Guardo el gráfico como png
ggsave('slides/class_8/class_8_taller/gg_test.png',
       plot = last_plot(), # explícitamente guarda el último gráfico.
       width = 4, height = 6, units = 'cm',
       scale = 2)

# Ubico la carpeta en la que quiero dejar el gráfico.
sd_path <- drive_get("Web Scraping")

# Subo el archivo
googledrive::drive_upload(media = 'slides/class_8/class_8_taller/gg_test.png',
                          path = sd_path, # también se puede usar "Web Scraping/"
                          overwrite = TRUE)

# Lamentablemente cambia el id del archivo creado.
