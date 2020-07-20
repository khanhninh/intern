# This function extract the segmentation results for many stations
# input: station (list name of all stations), k (segment with the whole model, default =0)
# path_data_homo ( path of homo file for all stations), path_raw (path of time series in R file),
# nb_test : to separate results of the test
# criterion : "BM_BJ", "BM_slope", "Lav"
# output: data frame including all breakpoints of all stations
plot_table=function(station,k,path_data_homo,path_raw,path_result,nb_test,criterion){
  number_break = data.frame()
  position_break = data.frame()
  for(i in 1:length(station)){
    station_name=station[i]
    df = mget(load(paste0(path_data_homo,"homo_",station_name,nb_test,".RData")))
    time_series = get(load(paste0(path_raw,station_name,".RData")))
    number<-as.data.frame(df$GNSS$K)
    number_break <- rbind(number_break,number)
  
  if (k ==0){
    criteri <-as.data.frame(df$GNSS$seg[[criterion]])      
    day <- time_series$date[criteri$end]
    criteri$day <- as.Date(day)
    position_break <- rbind(position_break,criteri)
    
  } else  {
    position <-as.data.frame(df$GNSS$seg)
    day <- time_series$date[position$end]
    position$day <- as.Date(day)
    position_break <- rbind(position_break,position)
  }
  }
  position_break$mean <- round(as.numeric(position_break$mean),digits=3)
  #Plot
  row.names(number_break) <- station
  jpeg(paste0(path_result,"figure/",nb_test,"number_break.jpeg"),,width = 1000, height = 1000,res =300)
  grid.table(number_break-1)
  #title("Number of breakpoints of the new data set")
  dev.off()
  # Set the name station for table
  name_station <- c()
  for(l in 1:nrow(number_break)){
    if (k ==0){
    h = number_break[[criterion]][l]
    } else {
      h = number_break[l,]
    }
    name_station <- c(name_station, rep(station[l],h))
  } 
  position_break$station <- name_station 
  filename = paste0(path_result,"figure/",nb_test,criterion,"position_break.pdf")
  pdf(filename)
  grid.table(position_break,rows = NULL)
  dev.off() 
  #png(paste0(path_results,"figure/","compare-new-cutoff.png"),width = 2000, height = 3000,res=300)
  system(paste("pdfcrop", filename, filename))
  
 # jpeg(paste0(path_result,"figure/",nb_test,criterion,"position_break.jpeg"),width = 1100, height = 3000,res=300)
  #grid.table(position_break)
  #title("Positions and amplitudes of breakpoints of the new data set")
  #dev.off()      
  save(number_break,file = paste0(path_result,"meta_compare/",nb_test,"number_break.RData"))
  save(position_break,file = paste0(path_result,"meta_compare/",nb_test,criterion,"position_break.RData"))
  write.table(number_break-1, file = paste0(path_result,"meta_compare/",nb_test,".txt")    #-1 last point
              , sep="\t", col.names = TRUE,row.names = TRUE,quote=FALSE)
  write.table(position_break, file = paste0(path_result,"meta_compare/",nb_test,criterion,".txt")
              , sep="\t", col.names = TRUE,row.names = TRUE,quote=FALSE)
}

