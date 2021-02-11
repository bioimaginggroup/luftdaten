#' Initialize Database
#'
#' @param name name of database
#' @param start start date, e.g. "2018-03-01"
#' @param verbose print additional information
#'
#' @export
#' @import DBI RSQLite
initializeDB<-function(name,start,verbose=FALSE)
{
  start<-as.character(as.Date(start)-1 ) 
  name<-paste0(getwd(),"/",name,".sqlite")
  if (verbose)cat(paste0("Initializing database ",name,"; lastdate set to ", start, "\n"))
  db <- dbConnect(SQLite(), name)
  dbWriteTable(db,"lastdate",data.frame("lastdate"=start),overwrite=TRUE)
  temp0<-data.frame("id"=integer(),"sensor_id"=integer(),"sensor_type"=character(),
                    "location"=integer(),"lat"=double(),"lon"=double())
  dbWriteTable(db,"locid",temp0,append=TRUE)
  temp0<-data.frame("id"=integer(), "locid"=integer(), "timestamp"=as.Date(character()))
  dbWriteTable(db,"messungen",temp0,append=TRUE)
  dbDisconnect(db)
}

