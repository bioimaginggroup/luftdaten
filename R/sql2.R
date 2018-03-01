#' Write luftdaten to mysqlite database
#'
#' @param db mysqlite database
#' @param end enddate, e.g. "2019-12-31"
#' @param source folder with csv files
#' @param save boolean, should database be backuped monthly, 2: save daily
#' @param folder folder with database for backups
#' @param dbname character, file name of database (for backup)
#' @param verbose print more information
#'
#' @return no return
#' @export
LD_sql2<-function(db, end=Sys.Date()-1, source="/media/schmid/local/data/luftdaten/", 
                 save=TRUE, sensortype=list(),
                 folder="./",dbname="db/luftdaten",verbose=FALSE)
{
  starttime=proc.time()[3]
  x=as.Date(paste(dbReadTable(db,"lastdate")))
  end<-as.Date(end)
  while (x<end)
  {
    x<-x+1
    cat(paste0("Processing ",x,"..."))
    files<-list.files(paste0(source,"/",x))
    files<-files[grep(pattern = ".csv",files)]
    for (file in files)
      {
        temp<-paste0(source,"/",x,"/",file)
        temp<-read.csv(temp,sep=";",header=TRUE)
        if(dim(temp)[1]>0)
          {
          sensortype=as.character(unique(temp$sensor_type))
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
              quera<-dbGetQuery(db,sql)
              if (dim(quera)[1]==0)
                {
                    locid[j]<-dbGetQuery(db,"select max(id) from locid")[1,1]+1
                    if (is.na(locid[j]))locid[j]=1
                    temp0<-data.frame("id"=locid[j],"sensor_id"=sensorid,"sensor_type"=sensortype,
                                      "location"=loc[j,3],"lat"=loc[j,1],"lon"=loc[j,2])
                    dbWriteTable(db,"locid",temp0,append=TRUE)
              }
              else
              {
                locid[j]=quera$id
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
          
          quera<-dbGetQuery(db,paste0("select max(id) from messungen"))[1,1]
          if (is.na(quera))quera=0
          temp0<-data.frame("id"=quera+(1:n), "locid"=locid, "timestamp"=as.character(temp$timestamp))
          dbWriteTable(db,"messungen",temp0,append=TRUE)

          nam<-names(temp)
          temp<-temp[,-which(names(temp)=="sensor_id")]
          temp<-temp[,-which(names(temp)=="sensor_type")]
          temp<-temp[,-which(names(temp)=="location")]
          temp<-temp[,-which(names(temp)=="lat")]
          temp<-temp[,-which(names(temp)=="lon")]
          temp<-temp[,-which(names(temp)=="timestamp")]
          
          
          dbWriteTable(db,sensortype,temp,append=TRUE)
          
          if(0){          
          if(any(nam=="P1"))
          {
            P1<-temp$P1
            if(is.null(P1))P1<-rep(NA,n)
            durP1<-temp$durP1
            if(is.null(durP1))durP1<-rep(NA,n)
            ratioP1<-temp$ratioP1
            if(is.null(ratioP1))ratioP1<-rep(NA,n)
            P2<-temp$P2
            if(is.null(P2))P2<-rep(NA,n)
            durP2<-temp$durP2
            if(is.null(durP2))durP2<-rep(NA,n)
            ratioP2<-temp$ratioP2
            if(is.null(ratioP2))ratioP2<-rep(NA,n)
            temp0<-data.frame("id"=quera+(1:n), "P1"=P1, "durP1"=durP1, "ratioP1"=ratioP1, "P2"=P2, "durP2"=durP2, "ratioP2"=ratioP2)
            dbWriteTable(db,"P",temp0,append=TRUE)
          }

          if(any(nam=="temperature"))
          {
            temperature<-temp$temperature
            if(is.null(temperature))temperature<-rep(NA,n)
            humidity<-temp$humidity
            if(is.null(humidity))humidity<-rep(NA,n)
            temp0<-data.frame("id"=quera+(1:n), "temperature"=temperature, "humidity"=humidity)
            dbWriteTable(db,"temphum",temp0,append=TRUE)
          }

          if(any(nam=="pressure"))
          {
            pressure<-temp$pressure
            if(is.null(pressure))pressure<-rep(NA,n)
            altitude<-temp$altitude
            if(is.null(altitude))altitude<-rep(NA,n)
            pressure_sealevel<-temp$pressure_sealevel
            if(is.null(pressure_sealevel))pressure_sealevel<-rep(NA,n)
            temp0<-data.frame("id"=quera+(1:n), "pressure"=pressure, "altitude"=altitude, "pressure_sealevel"=pressure_sealevel)
            dbWriteTable(db,"pressure",temp0,append=TRUE)
          }

          for (i in nam)
          {
            if (!any(i==c("sensor_id","sensor_type", "location", "lat", "lon", "timestamp", "pressure", "altitude",
                "pressure_sealevel", "temperature", "humidity", "P1", "durP1", "ratioP1", "P2", "durP2", "ratioP2","X")))
              print(c(file,i))
          }
          }
        }
    }
    timing<-round(proc.time()[3]-starttime)
    starttime<-proc.time()[3]
    m<-floor(timing/60)
    s<-round(timing%%60)
    h<-floor(m/60)
    m<-m%%60
    timing<-paste(s)
    if (h>0|m>0)timing<-paste0(m,":",timing)
    if (h>0)timing<-paste0(h,":",timing)
    
    cat(paste(" finished. Processing time:",timing,"\n"))
      dbWriteTable(db,"lastdate",data.frame("lastdate"=paste(x)),overwrite=TRUE)
      if (save)
      {
      if ((save<2&format(x+1,"%d")=="01")|save==2)
          fs::file_copy(paste0(folder,"/",dbname,".sqlite"), paste0(folder,"/",dbname,".",x,".sqlite"))
      }
      
  }
}


