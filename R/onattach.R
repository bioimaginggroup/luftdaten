.onAttach<-function(libname, pkgname)
{
  packageStartupMessage(paste0("luftdaten R package ver. ", utils::packageVersion("luftdaten")))
}
