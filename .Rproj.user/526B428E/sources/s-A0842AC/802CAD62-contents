# Funciones que tienen por objetivo detectar fallas en la conexión a internet
# o en la conexión con la API

checkInternet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(),msg = "Error: no se detecta conexión a internet.")
}
checkResponseStatus <- function(response){
  attempt::stop_if_not(.x=response$status_code,
                       .p = ~ .x == 200,
                       msg = "Error: hubo algún problema en la respuesta de la API")
}


