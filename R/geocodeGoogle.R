#' Convertí tus direcciones a puntos usando APIs gratuitas y del gobierno argentino
#' @param datos obligatorio: data.frame o data.table con los datos a geolocalizar
#' @param col_key obligatorio: columna con la key para usar en cada una de las filas. Útil para superar las restricciones de Google
#' @param cols_id obligatorio:
#' @param cols_query obligatorio: Columnas que van a agregarse a la consulta de google
#' @param col_region opcional: Es posible agregar una columna con la región para forzar resultados en esa región (por ejemplo "AR")
#' @import data.table
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @rdname geocodeDirecciones
#'
#' @return Un objeto data.frame con información sobre la ubicación espacial de direcciones de Argentina
#' @examples
#' \dontrun{
#' df <- data.frame(direccion=c("Figueroa Alcorta 7597","Pedro Goyena 330"),
#'             provincia=c("CABA","CABA"))
#' df$id <- 1:nrow(df)
#' df$key <-"alkfnaf901ehfassafqZ"
#' geocodeGoogle(datos=df,col_id="id",col_key="key", cols_query=c("direccion","provincia"),region=NULL)
#'
#' # Restringiendo resultados a Argentina
#' df$reg <- "AR"
#' geocodeGoogle(datos=df,col_id="id",col_key="key", cols_query=c("direccion","provincia"),region="reg")
#'
#' # Hay que cambiar el valor de la key por uno real para que funcione
#' }

geocodeGoogle <- function(datos =NULL,
                               col_key=NULL,
                               col_id=NULL,
                               cols_query=NULL,
                               col_region=NULL) {

  attempt::stop_if(.x = class(datos),.p = ~ !any(.x %in% c('data.frame','data.table')),
                   msg = "Revisá si pasaste los datos en el parámetro 'datos'.\nTenés que pasar un objeto que sea clase 'data.frame' o 'data.table'")
  datos <- as.data.frame(datos)
  if(is.null(col_region)) {
  mustExist <- c(col_id,cols_query,col_key)
  } else {
    mustExist <- c(col_id,cols_query,col_key,col_region)
  }
  faltantes <- mustExist[!mustExist %in% colnames(datos)]
  attempt::stop_if(.x = length(faltantes)>0, .p = ~ .x == TRUE,
                   msg = paste("Hay algunos nombres de columnas que no se encuentran\n en los datos que pasaste:",
                               paste0(sapply(faltantes,function(x){paste("'",x,"'",sep = "")}),collapse=","),sep=" "))

  geocoding <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  if(is.null(col_region)) {
  datos$query <- paste(geocoding, do.call(paste, c(datos[cols_query], sep="+")),"&key=",datos$key,sep="")
  } else {
  datos$query <- paste(geocoding, do.call(paste, c(datos[cols_query], sep="+")),"&region=",datos$col_region,"&key=",datos$key,sep="")
  }
  datos$query <- gsub(pattern = " ",replacement = "%20",x = datos$query)
  datos$query <- gsub(pattern = "#",replacement = "",x = datos$query)
  salida <- list()
  pb <- txtProgressBar(min = 1, max = nrow(datos), style = 3)
  for(i in 1:nrow(datos)) {
    setTxtProgressBar(pb, i)
    devolucion<-httr::GET(datos$query[i])
    json <- httr::content(devolucion,"text")
    output <- as.data.table(jsonlite::fromJSON(json))[,id:=datos$id[i]]
    salida <- c(salida,list(output))
  }

salida <- as.data.frame(rbindlist(salida,fill = TRUE))
return(salida)
}


