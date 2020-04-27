#' Usando la API de distancia de Google
#' La API de dsitancia de Google nos permite conocer la distancia y tiempo de transporte, entre muchas otras varaibles,
#' en viajes desde cualquier punto de origen y destino. La función distanceGoogle() nos permite comunicarnos de manera
#' fácil y organizada con Google.
#' Por default, la función espera dos objetos sf en "origins" y "destination" con una columna que identifique
#' a cada uno de los puntos de manera individual en cada uno de los data.frames
#' La función devuelve un data.frame con dos columnas con los IDs de ambos objetos sf y lo que haya devuelvo la
#' API de google. Próximamente se incluirá la posibilidad de usar direcciones no geocodificadas.
#' @param origins obligatorio: objeto sf o data.frame/data.table con los puntos o direcciones de inicio de viaje
#' @param destination obligatorio: objeto sf o data.frame/data.table con los puntos o direcciones de destino de viaje. Debe tener una sola fila
#' @param idCols obligatorio: Vector character de dos posiciones con información sobre el nombre de las columnas que contiene el ID en origins y destination (respectivamente)
#' @param travelMode obligatorio: Nombre del tipo de distancia a buscar. "driving" para viajes en auto, "bicycling" para viajes en bicileta, "walking" para viajes caminando y "transit" para viajes en transporte público
#' @param arrival_time opcional: Tiempo de llegada para el viaje. Debe ser un vector numérico y estar en formato Epoch/Timestamp. Ver en https://www.epochconverter.com/
#' @param addressCols opcional: Si se envía un vector character de dos posiciones com información sobre las columnas que contienen las direcciones, se usarán estas en lugar de los puntos espaciales en la consulta a Google (PENDIENTE DE IMPLEMENTACIÓN)
#' @param key obligatorio: vector de character con la key de Google API
#' @import data.table
#' @import sf
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @rdname distanceGoogle
#'
#' @return Un objeto data.frame con información sobre la ubicación espacial de direcciones de Argentina
#' @examples
#' \dontrun{
#' # df <- data.frame(direccion=c("Estadio Monumental","Cervecería Baum","FCE - UBA"),
#'                    lat=c(-34.5453444,-34.6258789,-34.6002407),
#'                    long=c(-58.4498438,-58.4319205,-58.3983398))
#' df$id <- 1:nrow(df)
#'
#' df <- st_as_sf(df,coords=c("long","lat"),crs=4326)
#'
#'
# distancias <- distanceGoogle(origins=df[!df$direccion == "Estadio Monumental",],
#                destination = df[df$direccion == "Estadio Monumental",],
#                idCols=c("id","id"),
#                key="afafaf31fafada",
#                travelMode="transit")
# # Usando un tiempo de llegada
#' distancias <- distanceGoogle(origins=df[!df$direccion == "Estadio Monumental",],
#'                              destination = df[df$direccion == "Estadio Monumental",],
#'                              idCols=c("id","id"),
#'                              key="afafaf31fafada",
#'                              travelMode="transit",
#'                              arrival_time = 1590494400)
#' # Hay que cambiar el valor de la key por uno real para que funcione
#' }

distanceGoogle <- function(origins=NULL,destination=NULL,travelMode="driving",idCols=NULL,addressCols=NULL, arrival_time=NULL,key=NULL) {

  attempt::stop_if(.x = key,.p = ~ !(is.character(.x) & is.vector(.x)),
                   msg = "Es necesario escribir la key de Google Api en el parámetro 'key'. Debe ser un character vector")

    correctIdCols <- ifelse(length(idCols)==2 & idCols[1] %in% colnames(origins) & idCols[2] %in% colnames(destination),TRUE,FALSE)
    attempt::stop_if(.x = correctIdCols,.p = ~ !.x,
                     msg = "Los nombres de columnas que pusiste en el parámetro idCols no existen en los objetos que pasaste como origins y/o destinations")

    attempt::stop_if(.x=nrow(destination), .p =  ~ !(.x==1),
                    msg="Hay más de una dirección en el dataset de 'destinations'. Actualmente esta función acepta solo una dirección de llegada")
    idOrigins <- origins[[idCols[1]]]
    idDestination <- destination[[idCols[2]]]
    if(is.null(addressCols)) {
      sfObjects <- ifelse(inherits(origins,"sf") & inherits(destination,"sf"),TRUE,FALSE)
      attempt::stop_if(.x = sfObjects,.p = ~ !.x,
                       msg = "Tanto 'origins' como 'destination' tienen que ser objetos de la clase sf.\nSi querés usar data.frames o data.tables tenés que aclarar los nombres de las columnas de 'orogins' y 'destination' que van a ser usadas en la consulta a Google")
      allPoints <- all(st_geometry_type(origins) %in% "POINT" & st_geometry_type(destination) %in% "POINT")
      attempt::stop_if(.x = allPoints,.p = ~ !.x,
                       msg = "Tanto 'origins' como 'destination' tienen que ser puntos. No se aceptan otro tipo de geometries")
      all4326 <- all( st_crs(origins)==4326 & st_crs(destination)==4326)
      attempt::stop_if(.x = all4326,.p = ~ !.x,
                       msg = "Tanto 'origins' como 'destination' tienen que tener CRS EPSG 4326. Transformalo usando st_transform() o definilo usando st_crs()")
      if(nrow(origins)>1){
      origins <- as.data.frame(st_coordinates(origins)[,c(2,1)])
      origins$forURL <- paste(origins$Y,origins$X,sep=",")
      } else{
        origins <- paste(st_coordinates(origins)[,c(2,1)],collapse=",")
      }
      # origins <- paste(origins$forURL,collapse = "|")
      destination <- paste(st_coordinates(destination)[,c(2,1)],collapse=",")
    } else {
      correctAdressCols <- ifelse(length(addressCols)==2 & addressCols[1] %in% colnames(origins) & addressCols[2] %in% colnames(destination),TRUE,FALSE)
      attempt::stop_if(.x = correctAdressCols,.p = ~ !.x,
                       msg = "Si querés usar columnas de un data.frame o data.table, fijate que hayas escrito bien los nombres de las columnas en el parámetro 'addressCols'")
    }
    correctTravelMode <- travelMode %in% c("driving","walking","bicycling","transit")
    attempt::stop_if(.x=correctTravelMode, .p =  ~ !.x,
                     msg="Incorrecto valor para el parámetro 'travelMode'. Tiene que ser 'driving', 'walking','bicycling' o 'transit'")

    if(is.null(arrival_time)){
    urlDistance <- "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&"
    } else {
      urlDistance <- paste("https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&arrival_time=",arrival_time,"&",sep="")
    }
    results <- list()

    pb <- txtProgressBar(min = 1, max = nrow(origins), style = 3)
    for(i in 1:nrow(origins)) {
      setTxtProgressBar(pb, i)
      consulta <- paste(urlDistance,"origins=",origins$forURL[i],"&destinations=",destination,"&mode=",travelMode,"&key=",key,sep="")
      prueba <- httr::GET(consulta)
      respuesta <- httr::content(prueba)
      results<-c(results,list(as.data.table(t(unlist(httr::content(prueba))),keep.rownames = TRUE)))
    }
    results <- rbindlist(results,fill = TRUE)
    results$idOrigins <- idOrigins
    results$idDestination <- idDestination
    return(as.data.frame(results))
}


