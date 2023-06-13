#| echo: false
#|
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr)
library(knitr)

opts_chunk$set(cache.path = "class_5_files/class_5_cache/html/")


#| fig-alt: 'Relación entre pasos para capturar sitios webs y funciones en R'
#| out-width: '75%'
#| echo: false
#|
knitr::include_graphics('https://github.com/yusuzech/r-web-scraping-cheat-sheet/raw/master/resources/functions_and_classes.png')


#| echo: true
text <- "¡Hola! Soy Cristián y tengo 44 años"

(text_url <- utils::URLencode(text))


#| echo: true
(utils::URLdecode(text_url))


#| echo: true
httr::GET(url = 'https://blog.desuc.cl/test.html') |> httr::http_status()


#| echo: true
rvest::session(url = 'https://blog.desuc.cl/')


#| echo: true
s_blog <- GET(url = 'https://blog.desuc.cl')

s_blog$request


#| fig-alt: 'Ejemplo de headers'
#| out-width: '70%'
#| echo: false
#|
knitr::include_graphics('class_5_files/request_blog.png')


#| echo: true
s_blog |> headers() |> enframe() |> 
  mutate(value = as.character(value))


#| echo: true
(resp <- GET('http://httpbin.org/get',
             query = list(var1 = "valor1", 
                          var2 = "valor2"), #URL values
             authenticate('usuario', 'clavesegura'),
             user_agent(agent = 'hola mundo'),
             set_cookies(a = 1),
             accept_json(),
             add_headers(class = "Web Scraping UC", year = 2023)))


#| echo: true
(git_cookies <- GET('http://github.com') |> cookies())


#| echo: true
git_cookies |> pull(value, name) |> str_trunc(width = 70)


#| echo: true
GET('http://httpbin.org/user-agent')


#| echo: true
read_html('http://httpbin.org/user-agent')


#| echo: true
GET('http://httpbin.org/cookies/set?c1=tritón&c2=crema')


#| echo: true
GET('http://httpbin.org/cookies') |> 
  cookies() |> as_tibble() |> 
  select(domain, name, value)


#| echo: true
GET('http://httpbin.org/cookies',
    set_cookies('x1' = 'vino'))


#| echo: true
GET('http://httpbin.org/cookies') |> 
  cookies() |> as_tibble() |> 
  select(domain, name, value)


#| echo: true
f_container_row <- function(.get){
  .get |> 
    read_html() |> 
    html_element('.container .row') |> 
    html_text() |> str_squish()
}

try(GET('https://www.scrapethissite.com/pages/advanced/?gotcha=headers') |> 
      f_container_row())


#| echo: true
try(GET('https://www.scrapethissite.com/pages/advanced/?gotcha=headers',
    accept('text/html')) |> 
      f_container_row())


#| echo: true
GET('https://www.scrapethissite.com/pages/advanced/?gotcha=headers',
    user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.5 Safari/605.1.15'),
    accept('text/html')) |> 
  f_container_row()


#| echo: true
form <- httr::POST('http://quotes.toscrape.com/login',
                   body = list(username = 'test', 
                               password = 'test'))

form |> read_html() |> html_element('body form') |> html_text2()


#| echo: true
resp_login <- GET('http://quotes.toscrape.com/login') |> 
  read_html() |> 
  html_elements('form input')

resp_login


(csrf_token_value <- resp_login[[1]] |> html_attr('value'))


#| echo: true
resp_form <- httr::POST('http://quotes.toscrape.com/login',
                        body = list(username = 'test', 
                                    password = 'test',
                                    csrf_token = csrf_token_value))

resp_form |> read_html() |> html_elements('.quote') |> html_text2() |> _[1:3]



s <- rvest::session('http://quotes.toscrape.com/login')
f <- html_form(s)[[1]]

f_llena <- html_form_set(form = f,
                         username = 'test', 
                         password = 'test2')

f_respuesta <- session_submit(x = s,
                              form = f_llena)

f_respuesta |> read_html() |> 
  html_elements('.quote') |> 
  html_text2() |> _[1:3]


#| include: false

# Extraer código R
knitr::purl('class_5.qmd',
            output = 'class_5.R',
            quiet = TRUE)

