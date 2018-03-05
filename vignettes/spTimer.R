---
title: "Use spTimer from Sahu et al."
output: html_notebook
---

Only Munich, Only a couple of hours

```{r find munich bbox}
library(osmdata)
query <- opq(bbox='Munich, Germany')
bbox<-as.numeric(strsplit(query$bbox,",")[[1]])
```

```{r load data from munich for certain time}
library(RSQLite)
db<-dbConnect(SQLite(),"db/CopyOf2018-SDS011.sqlite")
P1<-dbGetQuery(db,paste0("SELECT dataP.locid,time(dataP.timestamp),locid.lat,locid.lon,dataP.P1 from dataP,locid where date(timestamp)='2018-02-28' and locid.id=dataP.locid and locid.lat<",bbox[3]," AND locid.lat>",bbox[1]," AND locid.lon>",bbox[2], " AND locid.lon<",bbox[4]))
print(dim(P1))
locid<-dbGetQuery(db,paste0("SELECT locid.id,locid.lat,locid.lon from dataP,locid where date(timestamp)='2018-02-28' and locid.id=dataP.locid and locid.lat<",bbox[3]," AND locid.lat>",bbox[1]," AND locid.lon>",bbox[2], " AND locid.lon<",bbox[4]))
locid<-unique(locid)
dbDisconnect(db)
```

We have 114 locations with 62931 observations. 
Plotting locations:

```{r plot locations in munich}
if (0){
#Notice: Osmar with local file is very slow, because file os so big.
      ctown <- get_osm(bb, source = src)
      for (j in c("motorway","trunk","primary","secondary","tertiary"))
        {
        road <- find(ctown, way(tags(k == "highway" & v==j)))
        road <- find_down(ctown, way(road))
        road <- subset(ctown, ids = road)
        plot_ways(road, add=TRUE)
        }
        road <- find(ctown, way(tags(k == "highway" & v=="residential")))
        road <- find_down(ctown, way(road))
        road <- subset(ctown, ids = road)
        plot_ways(road, add=TRUE, col=grey(.6))
}
```


```{r plot locations in munich}
luftdaten::plot_loc(locid,range=as.numeric(bbox[c(2,4,1,3)]))
query <- add_osm_feature(query, key='highway', value='!residential')
test<-osmdata_sp(query)
sp::plot(test$osm_lines, add=TRUE)
```


Reduce time, try afternoon 12pm top 4pm
```{r}
times<-as.POSIXct(P1$`time(dataP.timestamp)`,format="%H:%M:%S")
P1<-P1[times<paste(Sys.Date(),"15:00:00")&times>paste(Sys.Date(),"12:00:00"),]
print(dim(P1))
print(length(unique((P1$locid))))
```

construct a matrix for spTimer, ten-minutes means (gives )

locations<-sort(unique(locid$id))[-55]
L<-length(locations)
T<-144
data<-rep(NA,L*T)
counter<-0
# bad coding ahead
    for (j in 1:L)
    {
      for (t in 1:T)
      {
        find<-(times<as.POSIXct(paste(Sys.Date(),"12:00:00"))+360*t)
        find<-find&(times>=as.POSIXct(paste(Sys.Date(),"12:00:00"))+360*(t-1))
        find2<-P1$locid==locations[[j]]
      counter<-counter+1
      data[counter]<-mean(P1$P1[find&find2])
  }
}




113 locations with 8038 observations


Simple spTimer model
```{r}
library(spTimer)
GP<-spT.Gibbs(data~1,coords=cbind(locid$lon[-55],locid$lat[-55]))
newlat<-rep(seq(bbox[1],bbox[3],length=20),each=20)
newlon<-rep(seq(bbox[2],bbox[4],length=20),times=20)
newcoords<-data.frame("lat"=newlat,"lon"=newlon)
p.GP<-predict.spT(GP,newcoords=newcoords)

GPP<-spT.Gibbs(data~1,coords=cbind(locid$lon[-55],locid$lat[-55]), model="GPP")
p.GPP<-predict.spT(GPP,newcoords=newcoords)

plot(p.GP)
plot(p.GPP)
```

