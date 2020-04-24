#' Convertí tus direcciones a puntos usando APIs gratuitas y del gobierno argentino
#' @param datos OBLIGATORIO: Es un data.frame/data.table/tibble que contiene información sobre las direcciones a geolocalizar  (data.frame/data.table/tibble)
#' @param col_id OBLIGATORIO: Nombre de la columna que contiene un id único por dirección. Aunque la función no hace un chequeo de que sea único, es necesario que así lo sea para poder unir la devolución de puntos, que puede ser múltiple. (Vector numérico o character)
#' @param col_direccion OBLIGATORIO: Nombre de la columna que contiene la dirección a geolocalizar y normalizar. Por ejemplo, que contenga valores como "Figueroa Alcorta 7597".  (Vector character)
#' @param col_provincia Nombre de la columna que contiene la provincia de la direccion geolocalizar y normalizar. Por ejemplo, que contenga valores como "Ciudad de Buenos Aires". (Vector character)
#' @param col_departamento Nombre de la columna que contiene el departamento de la direccion geolocalizar y normalizar. Por ejemplo, que contenga valores como "Comuna 11" o "Berazategui". (Vector character)
#' @param api Texto que dice que API usar. "datosgobar" es la API del gobierno nacional (https://datosgobar.github.io/georef-ar-api/). "gcba") es la API del gobierno de la Ciudad de Buenos Aires (pendiente de implementación)
#' @param max_returns Cantidad de devoluciones máximas de geolocalización por dirección.
#'
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
#' geocodeDirecciones(datos=df,col_direccion="direccion",col_provincia="provincia")
#' }

geocodeDirecciones <- function(datos =NULL,
                               col_id=NULL,
                               col_direccion="direccion",
                               col_provincia=NULL,
                               col_departamento = NULL,
                               api="datosgobar",
                               max_returns=1) {

  # attempt::stop_if(.x = api,.p = ~ !.x %in% c("datosgobar","gcba")
                   # msg = paste0("El valor de API es incorrecto. Debe ser ",paste(c("'datosgobar'","'gcba'"),collapse = " o ")))
  urlApi <- switch(api,
                   "datosgobar" = "http://apis.datos.gob.ar/georef/api/direcciones",
                   "gcba" = "square")


attempt::stop_if(.x = class(datos),.p = ~ !any(.x %in% c('data.frame','data.table')),
                 msg = "Revisá si pasaste los datos en el parámetro 'datos'.\nTenés que pasar un objeto que sea clase 'data.frame' o 'data.table'")
datos <- as.data.frame(datos)
mustExist <- c(col_id,col_direccion,col_provincia,col_departamento)
faltantes <- mustExist[!mustExist %in% colnames(datos)]
attempt::stop_if(.x = length(faltantes)>0, .p = ~ .x == TRUE,
                 msg = paste("Hay algunos nombres de columnas que no se encuentran\n en los datos que pasaste:",
                              paste0(sapply(faltantes,function(x){paste("'",x,"'",sep = "")}),collapse=","),sep=" "))

if(nrow(datos)>1) {
  pb <- txtProgressBar(min = 1, max = nrow(datos), style = 3)
  salida<-rbindlist(lapply(1:nrow(datos),function(x){
    setTxtProgressBar(pb, x)

    out <- geocodeDir(direccion=datos[x,col_direccion],
              provincia=datos[x,col_provincia],
              departamento =datos[x,col_departamento],
              limit = max_returns,
              urlApi = urlApi)
    cbind(id=datos[x,col_id],out)

  }),fill = TRUE)
}

return(data.frame(salida))
}
