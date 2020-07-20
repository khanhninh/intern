timecutoff = function(station,path_new_data,path_old_data,path_results,nb_test){
  for(i in 1:length(station)){
    station_name=station[1]
    new = get(load(paste0(path_new_data,station_name,".RData")))
    old = get(load(paste0(path_old_data,station_name,".RData")))
    
    dif <- merge(new,old, by=c("date"))  # merge 2 data which have the same date
    d <- dif[complete.cases(dif),]  # remove NA value
    d1 <- d[,c("date","signal.x","signal.y")]   # keep only date and signal
    
    # aggregate two data sets
    new1 <- new[which(new$date >= min(d1$date) & new$date <= max(d1$date)),]   # merge new data with merged frame to get same date 
    save(new1, file = paste0(path_results,"cutoff/",station_name,".RData"))

  }
}