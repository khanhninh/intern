monthly = function(station,path_new_data,path_old_data,path_results,nb_test){
  for(i in 1:length(station)){
    station_name=station[i]
    new = get(load(paste0(path_new_data,station_name,".RData")))
    old = get(load(paste0(path_old_data,station_name,".RData")))
    
    # extract monthly new data
    new = new[complete.cases(new), ]
    new$agg <- paste0(new$month,new$year)
    monthnew <- data.frame(new)  %>% group_by(agg) %>% summarise_each(funs(mean))
    monthnew <- monthnew[order(as.Date(monthnew$date, format="%d/%m/%Y")),]
    # extract monthly old data
    old = old[complete.cases(old), ]
    old$agg <- paste0(old$month,old$year)
    monthold <- data.frame(old)  %>% group_by(agg) %>% summarise_each(funs(mean))
    monthold  <- monthold [order(as.Date(monthold $date, format="%d/%m/%Y")),]
    # extract monthly difference 
    dif <- merge(monthnew,monthold, by=c("agg"))
    dif <- dif[,c(3,4,11)]
    dif  <- dif [order(as.Date(dif$date, format="%d/%m/%Y")),]
    differ = -dif$signal.x+dif$signal.y
    # Set limit and plot 
    lim1 = min(min(monthnew$signal),min(monthold$signal))
    lim2 = max(max(monthnew$signal),max(monthold$signal))
    ref = rep(c(lim1-1), times =length(differ))
    
    png(filename=paste0(path_results,station_name,nb_test,"compare.png"),width = 800, height = 500)
    plot(monthnew$date,monthnew$signal,cex =0.3,type = "l",col ='red',ylab ="GNSS IWV time series (average monthly)",ylim=c(lim1-1,lim2+1))
    legend("topright", legend = c("new","old","difference"), col=c("red", "blue","green"), lty=1:1, cex=0.8)
    points(monthold$date,monthold$signal,cex =0.3,type = "l",col ='blue')
    points(dif$date.x,differ+(lim1-1),cex =0.3,type = "l",col ='green')
    points(dif$date.x,ref,col ='black',cex=0.3,type = "l")
    title(station_name)
    dev.off()
    
  }
}
