#' @import data.table
#' @importFrom attempt stop_if_all
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content

geocodeDir <- function(direccion,provincia=NULL,departamento=NULL, limit=1,urlApi=NULL) {
  if (gsub("[^0-9.-]", "", direccion)=="0") {
    return(data.table(codigoAPI='Error'))
    }

  args <- list(direccion=direccion,provincia = provincia,departamento=departamento,max = limit)
  args <- args[!sapply(args,function(x) { (is.null(x) | class(x) %in% c('data.frame','data.table'))})]

  checkInternet()
  response <-httr::GET(urlApi,query=args)
  if(!response$status_code==200) {
    return(data.table(codigoAPI='Error'))
  }
  # checkResponseStatus(response)

  jsonRes <- jsonlite::fromJSON(httr::content(response,'text',encoding = 'UTF-8'))
  nMatchAPI<-jsonRes$cantidad
  if(nMatchAPI==0) {
    return(data.table(nMatchAPI,codigoAPI="Exito"))
  } else {
    geocodeResult <- data.table(jsonRes$direcciones)[,c("calle.nombre","departamento.nombre","localidad_censal.nombre","nomenclatura","ubicacion.lat","ubicacion.lon")]
  }
  return(data.table(geocodeResult,nMatchAPI,codigoAPI="Exito"))
}
