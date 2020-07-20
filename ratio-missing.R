station <-name
data=data.frame()
ratio <- c()
length <- c()
for(i in station){ 
  Y<-get(load(paste0(path_era5,i,".RData")))
  b= subset(Y, is.na(Y$signal) == FALSE)
  DateConvert = as.Date(as.POSIXct(Y$date, 'GMT'))
  a = tail(DateConvert, n=1) - head(DateConvert, n=1)
  a1 = nrow(b)/as.numeric(a)
  ratio <- c(ratio,a1)
  length <- c(length,nrow(b))
}
