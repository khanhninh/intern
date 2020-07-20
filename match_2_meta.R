rm(list = ls())
library(tidyverse)
b <- read.csv("/Users/khanhninhnguyen/Documents/internipgp/Stage/Data/eq_change_list.dates.txt",sep="")
a <- read.csv("/Users/khanhninhnguyen/Documents/internipgp/Stage/Data/eq_change_list_mapfun.dates.txt",sep="")
total <-merge(x = a, y = b, by = c("NAME","YYYY.MM.DD"), all  = TRUE)
for (i in 1:nrow(total)){
   year = c(total$YEAR.x[i],total$YEAR.y[i])
   year = year[!is.na(year)]
   year = unique(year)
   total$YEAR[i] = year
   doy = c(total$DOY.x[i],total$DOY.y[i])
   doy = doy[!is.na(doy)]
   doy = unique(doy)
   total$DOY[i] = doy
  if (is.na(total$TYPE.x[i]) == TRUE){
    typp=as.character(total$TYPE.y[i])
  }else{
    typp=as.character(total$TYPE.x[i])
  }
  total$TYPE[i] = typp
}
total <- total[ , -which(names(total) %in% c("TYPE.y","TYPE.x","YEAR.x","YEAR.y","DOY.x","DOY.y"))]
data <- total[c(1,3,4,2,5)]
new_data = data.frame()
for (j in 1: length(unique(data$NAME))){
  namefile = unique(data$NAME)[j]
  sub = subset(data, data$NAME == namefile)
  sub1 = sub[order(as.Date(sub$YYYY.MM.DD)),]
  new_data <- rbind(new_data,sub1)
  
}
write.table(new_data, file = paste0(path_data,"metaNinh.txt")
            , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)

