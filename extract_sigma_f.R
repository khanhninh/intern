# this function used to extract the monthly variance (12 months) and the std(f)
# and save it in the monthly_var.txt 
# It also extract the stdf file including the mean of monthly variance, stdf, 
# mean of the mean value of intervals and number of breakpoints for each station.

extract.std = function(station,number.segment, path_homo, path_result, nb_test, criterion ){
list.std =data.frame()
ext.std = data.frame()
ext.noise = data.frame()
new.noise = data.frame()
  for(i in 1:length(station)){
    station_name=station[i]
    homo = get(load(paste0(path_homo,"homo_",station_name,nb_test,".RData")))
    k = number.segment
    if (k ==0){
      segment <-as.data.frame(homo$seg[[criterion]])  
      funct <- homo$funct[[criterion]] 
      number_break_point <- as.data.frame(homo$K[[criterion]])  
    } else if (k!=0) {
      funct = homo$funct
      segment <- homo$seg
      number_break_point = homo$K
    }
    sigma = homo$variances
    ext.noise = rbind(ext.noise, round(sigma, digits = 3))
    average = mean(segment$mean)
    std.func =sd(funct,na.rm = TRUE)
    mean.variance = mean(sigma)
    val = c(std.func, mean.variance, average, number_break_point[1,] )
    list.std = rbind(list.std,val)
  }
  colnames(ext.noise) <-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  colnames(list.std) <- c("stdf","meansigma","mean","nb")
  ext.noise$stdf <- round(list.std$stdf, digits = 3)
  list.std$name <-name
  ext.noise$station<-name
  new.noise <- ext.noise[, c("station","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","stdf" )]
  write.table(list.std, file = paste0(path_results,"sigma-f",criterion,nb_test,".txt")
              , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)
  write.table(new.noise, file = paste0(path_results,"monthly_var",criterion,nb_test,".txt")
              , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)
 # return(list.std)
  print(paste0("stdf", mean(list.std$stdf)))
  print(paste0( "meansigma", mean(list.std$meansigma)))
  
}