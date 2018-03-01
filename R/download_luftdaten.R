#' Download csv files from archive.liftdaten.info
#'
#' @param folder folder to store csv
#' @param start start date, e.g. "2015-10-01"
#' @param end end data, e.g. Sys.Date()
#'
#' @import curl 
#' @return files are stored in folder/
#' @export 
LD_download<-function(folder, start="2015-10-01",end=Sys.Date())
{
  x=as.Date(start)
  end<-as.Date(end)
  while (x<=end)
  {
    print(x)
    dir.create(paste0(folder,"/",x))
    con<-curl::curl(paste0("http://archive.luftdaten.info/",x))
    test<-try({open(con)})
    if(is.null(test))
    {
      mode=2
      temp=readLines(con, n = 4)
      if (temp[2]=="<html>")
      {
        mode=7
        temp=readLines(con, n = 7)
      }
      while (TRUE){
        out <- readLines(con, n = 1)
        if (out=="</pre><hr></body>")break
        out<-strsplit(out,">")[[1]][mode]
        out<-strsplit(out,"</a")[[1]]
        if (out=="measurements.txt")break
        dest<-paste0(folder,"/",x,"/",out)
        out<-paste0("http://archive.luftdaten.info/",x,"/",out)
        print(out)
        curl::curl_download(out, dest)
        Sys.sleep(time = runif(1,0.07,0.19))
      }
    }
    close(con)
    x<-x+1
  }
}
