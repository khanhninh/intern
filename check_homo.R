# # check validation file
# a<- read.table(paste0(path_results,"validation/2-BM_BJ.txt"),skip=1)
# b<- read.table(paste0(path_data,"tot_out_init_BM1_30_30.validation.txt"),skip=1)
# namecol <- c("name","detected","known","flag","dt","valid","noise")
# colnames(a) <- namecol
# colnames(b) <- namecol
############### Check homogenization file because there is mistake in copying ############
files_old=list.files(paste0(path_old_data,''))
name_old=substr(files_old[],1,nchar(files_old[])-6)
files_new=list.files(paste0(path_new_data,''))
name_new=substr(files_new[],1,nchar(files_new[])-6)
length.new = c()
mindate = c(as.Date("00/00/00", "%y/%m/%d"))
maxdate = c(as.Date("00/00/00", "%y/%m/%d"))
for(i in name){
  station_name=i
  Y<-get(load(paste0(path_data,"matching/new/",station_name,".RData")))
  Ym = subset(Y, is.na(Y$signal) == FALSE)
  length.new = c(length.new,nrow(Ym))
  DateConvert = as.Date(as.POSIXct(Ym$date, 'GMT'))
  mindate = c(mindate, head(DateConvert, n=1))
  maxdate = c(maxdate, tail(DateConvert, n=1))
}
mindate = mindate[-1]
maxdate = maxdate[-1]

list.new.length = data.frame(name,length.new,mindate,maxdate)
list.new.length$delta = list.new.length$maxdate - list.new.length$mindate

write.table(list.new.length, file = paste0(path_results,"list-length-new-matched-data.txt")
                , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)

cutoff = sort(list.new.length$length.new, decreasing=TRUE)
list.new.max = data.frame()
order = list.new.length[order(-list.new.length$length.new),]
cutoff = order[which(order$length.new >=5500),]
cutoff1 <- cutoff[order(cutoff$name_new),]
cutname = cutoff1$name_new

homo.new =list.files(paste0(path_main,"homo/old/",''))
name.new=substr(homo.new[],1,nchar(homo.new[])-7)

for(i in 1:length(name.new)){
  look = subset(cutoff1, cutoff1$name_new == name.new[i])
  if (nrow(look)== 0){
    print(i)
  }
}

# # write.table(order, file = paste0(path_results,"list-length-new-data.txt")
# #             , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)
# 
# 
# filename = paste0(path_results,"figure/","histogram-length-new.jpg")
# jpeg(filename,width = 3000, height = 2000,res=300)
# hist(list.new.length$length.new,breaks = 100,col ='gray', xlab = " Length of time series",main = " Histogram of length of time series of new data set")
# dev.off() 

