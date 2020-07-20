# This function used to plot the time series for all stations,
# name station input by station argument
# number.segment : k = 0 : run the whole segmentation with criteria
#                  k = fix number: run the result for segmentation with given number of segments
# path_raw: input of time series 
# path_homo: path of the result of GNSS fast
# path_resutt: where save file 
# nb_test, criterion: defined in the main program
plot_timeseries = function(station,number.segment, path_raw, path_homo, path_result, nb_test, criterion ){
  for(i in 1:length(station)){
    station_name=station[i]
    timeseries = get(load(paste0(path_raw,station_name,".RData")))
    homo = get(load(paste0(path_homo,"homo_",station_name,nb_test,".RData")))
    t=timeseries$date
    k = number.segment
    if (k ==0){
      segment <-as.data.frame(homo$seg[[criterion]])  
      funct <- homo$funct[[criterion]] 
#      funct = homo$funct$BM_BJ
#      seg = homo$seg$BM_BJ
      number_break_point <- as.data.frame(homo$K[[criterion]])  
#      number_break_point = homo$K$BM_BJ
    } else if (k!=0) {
      funct = homo$funct
      segment <- homo$seg
      number_break_point = homo$K
    }
    std = homo$variances
    # Create mean array
    m <- numeric(0)
    for(i in 1:length(t)) {
      for(j in 1:nrow(segment)) {
        if( (segment$begin[j]<=i)&(i<=segment$end[j]) ) {
          m[i] = segment$mean[j]
        }
      }
    }
    #Create variance array
    va <- numeric(0)
    for(k in 1:length(timeseries$month)) {
      for(h in 1:12) {
        if( as.numeric(timeseries$month[k])==h ) {
          va[k] = std[h]
        }
      }
    }
    # print number of detected, validated point
    path_results = "/Users/khanhninhnguyen/Documents/internipgp/Stage/Results/validation/"
    meta <- read.table(paste0(path_results,nb_test,"-",criterion,".txt"),skip=1)
    colnames(meta) <- c("name", "detected", "known", "flag","dt12","valid","noise")
    meta.sta = meta[which(meta$name == station_name),]
    nb_valid = sum(meta.sta$valid)
    nb_outlier = sum(meta.sta$noise)
   # miny =  min(timeseries$signal,na.rm=TRUE)
   # maxy = max(timeseries$signal,na.rm=TRUE)
    miny = -10
    maxy = 10
    ref = rep(c(miny-1), times =length(timeseries$signal))
    jpeg(paste0(path_result,nb_test,"-", criterion, "-", station_name,".jpeg"),width = 1600, height = 800,res = 300)
#    plot(timeseries$date,timeseries$signal,cex =0.3,type = "l",col ='gray',ylab ="IWV diff ( GPS -ERAI) kg/m2",xlab = " ",ylim=c(miny-2,maxy+2))
    plot(timeseries$date,-timeseries$signal,cex =0.3,type = "l",col ='gray',ylab ="IWV diff(kg/m2)",xlab = "Time")
    points(t,-m,type = "l",col ='red',cex=0.5)  
#    points(t,funct-1+miny,col ='cyan',cex=0.3)
#    text = paste0("detection = ", number_break_point-1,  "  validated = ", nb_valid, "  noise = ",nb_outlier,  " std(f) = ",round(sd(funct,na.rm = TRUE),digits = 3), "  mean(variance) = ", round(mean(std),digits = 3))
#    mtext(text, side=3)
#    points(t,va-1+miny,col ='blue',cex=0.3,type = "l")
#    points(t,ref,col ='black',cex=0.3,type = "l")
    imax = dim(segment)[1]-1
    for( i in 1:imax ) {
      abline(v=t[segment$end[i]],col='red', lty=2)
    }
 
#    title(paste0(criterion, "  ", station_name ))
#    title( station_name )
    dev.off()
   
    
  }
  
  
  
  
}