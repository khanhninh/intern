timematching = function(station,path_new_data,path_old_data,path_result){
  for(i in 1:length(station)){
    station_name=station[i]
    new = get(load(paste0(path_new_data,station_name,".RData")))
    old = get(load(paste0(path_old_data,station_name,".RData")))
    
    dif <- merge(new,old, by=c("date"))  # merge 2 data which have the same date
    d <- dif[complete.cases(dif),]  # remove NA value
    d1 <- d[,c("date","signal.x","signal.y")]   # keep only date and signal
    
    # aggregate two data sets
    new1 <- merge(new,d1, by=c("date"))   # merge new data with merged frame to get same date 
    new1 <- new1[,c("name_station","date","signal","ERAI","GPS","month","year")] # create the same frame to homogenize
    old1 <- merge(old,d1, by=c("date"))
    old1 <- old1[,c("name_station","date","signal","ERAI","GPS","month","year")]
    save(new1, file = paste0(path_result,"new/",station_name,".RData"))
    save(old1, file = paste0(path_result,"old/",station_name,".RData"))
    
  }
}
