#' Write luftdaten to mysqlite database
#'
#' @param start start date, e.g. "2016-10-01"
#' @param end enddate, e.g. "2019-12-31"
#' @param source folder with csv files
#' @param sensortype vector with sensortypes to process
#' @param dbname character, file name of database (for backup)
#' @param verbose print more information
#' 
#' @import RSQLite DBI
#'
#' @return no return
#' @export
LD_sql2<-function(start, end=Sys.Date()-1, source="/media/schmid/local/data/luftdaten/", 
                 sensortype=c(), dbname="db/luftdaten",verbose=FALSE)
{
  starttime=proc.time()[3]
  x=as.Date(start)
  end<-as.Date(end)
  while (x<=end)
  {
    cat(paste0("Processing ",x,"..."))
    files<-list.files(paste0(source,"/",x))
    files<-files[grep(pattern = ".csv",files)]
    if (length(sensortype)>0){
      f0<-c()
      for (s in sensortype)f0<-c(f0,files[grep(pattern = s,files)])
      files<-f0
    }
    
    for (file in files)
      {
      if (verbose)print(file)
        temp<-paste0(source,"/",x,"/",file)
        temp<-read.csv(temp,sep=";",header=TRUE)
        if(dim(temp)[1]>0)
          {
          sensor_type=as.character(unique(temp$sensor_type))
          db<-dbConnect(SQLite(),paste0(dbname,sensor_type,".sqlite"))
          test<-array(0,c(0,1))
          try(test<-dbGetQuery(db,paste0("SELECT * from processed where file='",file,"'")),silent=TRUE)
          if (dim(test)[1]>0){
            dbDisconnect(db)
            if (verbose)print("already done!")
          }
          else
            {
              #if (verbose)print("let's do it!")
              sensorid=unique(temp$sensor_id)
          if(is.na(sensorid)|sensorid<1)break
          temp$lat[is.na(temp$lat)]=0
          temp$lon[is.na(temp$lon)]=0
          
          loc=unique(cbind(temp$lat,temp$lon,temp$location))
          loclength=dim(loc)[1]
          if (loclength==0)break
          locid=rep(NA,loclength)
          for (j in 1:loclength)
            {
              sql=paste0("select * from locid where lat='",loc[j,1],"' AND lon='",loc[j,2],"' AND location='",loc[j,3],"' AND sensor_id='",sensorid,"'")
              test<-try(quera<-dbGetQuery(db,sql),silent=TRUE)
              if (class(test)=="try-error")
              {
                temp0<-data.frame("id"=1,"sensor_id"=sensorid,"sensor_type"=sensor_type,
                                  "location"=loc[j,3],"lat"=loc[j,1],"lon"=loc[j,2])
                dbWriteTable(db,"locid",temp0)
                locid[j]=1
              }
              else{  
              if (dim(quera)[1]==0)
                {
                    locid[j]<-dbGetQuery(db,"select max(id) from locid")[1,1]+1
                    if (is.na(locid[j]))locid[j]=1
                    temp0<-data.frame("id"=locid[j],"sensor_id"=sensorid,"sensor_type"=sensor_type,
                                      "location"=loc[j,3],"lat"=loc[j,1],"lon"=loc[j,2])
                    dbWriteTable(db,"locid",temp0,append=TRUE)
              }
              else
              {
                locid[j]=quera$id
              }
              }
          }
          
          n<-dim(temp)[1]
          if (loclength==1)
          {
            lloc<-rep(1,loclength)    
          }
          else
          {
            lloc<-unlist(lapply(1:n,function(i){for (j in 1:loclength)if (all(temp[i,c(4,5,3)]==loc[j,]))return(j)}))
          }
          locid<-locid[lloc]
          
          nam<-names(temp)
          temp<-temp[,-which(names(temp)=="sensor_id")]
          temp<-temp[,-which(names(temp)=="sensor_type")]
          temp<-temp[,-which(names(temp)=="location")]
          temp<-temp[,-which(names(temp)=="lat")]
          temp<-temp[,-which(names(temp)=="lon")]
          if (any(nam=="X"))temp<-temp[,-which(names(temp)=="X")]
          
          if (!is.null(dim(temp)))
          {
            temp$locid <- locid
            dbWriteTable(db,"data",temp,append=TRUE)
          }
          
          dbWriteTable(db,"processed",data.frame("file"=file),append=TRUE)

          dbDisconnect(db)
            }
        }
    }
    x<-x+1
    timing<-round(proc.time()[3]-starttime)
    starttime<-proc.time()[3]
    m<-floor(timing/60)
    s<-round(timing%%60)
    h<-floor(m/60)
    m<-m%%60
    if (m<10)m<-paste0("0",m)
    if (s<10)s<-paste0("0",s)
    timing<-paste0(h,":",m,":",s)
    cat(paste("finished. Processing time:",timing,"\n"))

  }
}