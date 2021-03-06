# This function is used for finding the number of similar time point between two segmentation
# result. The idea 
# Input: path of validation file, path of result, path of the time series of ref and compared,
# number of 2 tests: reference and compared, criterion. 
# Output: number and table of comparison
similarity=function(path.validation,path_result,path_ref,path_comp,nb_test.ref,nb_test.comp,detection,criterion){
# Load the validation data, note that this data only include the stations which has breakpoints
ref <-get(load(paste0(path.validation,nb_test.ref,"-",criterion,"metacompa.RData")))# reference 
ref <- ref[,c("name","detected","valid","flag")] # extract only the detected day and name of station
ref$name <- as.character(ref$name)
comp <-get(load(paste0(path.validation,nb_test.comp,"-",criterion,"metacompa.RData")))
comp <- comp[,c("name","detected","valid","flag")]
comp$name <- as.character(comp$name)
a = inner_join(ref,comp,by="name") # find the name of the cross stations in two data sets 
station <- as.character(unique(a$name))
print(paste0("number of common stations ",length(station)))
data=data.frame()
for(i in 1:length(station)){ 
  ref.series = get(load(paste0(path_ref,station[i],".RData")))
  comp.series = get(load(paste0(path_comp,station[i],".RData")))
  DateConvert = as.Date(as.POSIXct(ref.series$date, 'GMT'))
  ref.series$date <- DateConvert
  DateConvert1 = as.Date(as.POSIXct(comp.series$date, 'GMT'))
  comp.series$date <- DateConvert1
  comp.series = comp.series[,c("name_station","date","signal")]
  ref.series = ref.series[,c("name_station","date","signal")]
  if(detection ==1){ 
    comp.indiv = comp[which(comp$name == station[i]),] # extract homogenize for each station
    ref.indiv  = ref[which(ref$name == station[i]),] # extract homogenize for each station
  } else if(detection == 0){
    comp.indiv = comp[which(comp$name == station[i]),] # extract homogenize for each station
    comp.indiv = comp.indiv[which(comp.indiv$valid ==1),]
    ref.indiv  = ref[which(ref$name == station[i]),]    # extract homogenize for each station  
    ref.indiv = ref.indiv[which(ref.indiv$valid ==1 ),]
  }
 #seg_old = seg_old[which(as.Date(seg_old$detected) <= "2010-12-31"),]      if need compare in the sam window, uncomment this line
  
  
  
  
  ############## !!!!!!!!!!!!!! check xem co dung na omit truoc khi tim near point ko? ######
  
  if (nrow(comp.indiv)>=1 & nrow(ref.indiv) >=1){
    similar <-rep(0,length(comp.indiv[,1]))
    z <-c(as.Date("00/00/00", "%y/%m/%d"))
    near_comp <-rep(z,length(comp.indiv[,1]))
    dt12 <-rep(0,length(comp.indiv[,1]))
    for(j in 1:nrow(comp.indiv)){
      ref.td = ref.indiv$detected
      comp.td = comp.indiv$detected[j]
      nearpoint = ref.td[which.min(abs(ref.td - comp.td))]
      if(comp.td == nearpoint){
        dt12[j] = 0
        similar[j] = 1
      } else {
        twopoint = sort(c(comp.td,nearpoint), decreasing = FALSE)
        comp.sub = na.omit(comp.series[which(comp.series$date >= twopoint[1] & comp.series$date <= twopoint[2]),])
        ref.sub = na.omit(ref.series[which(ref.series$date >= twopoint[1] & ref.series$date <= twopoint[2]),])
        dt = inner_join(comp.sub,ref.sub, by = "date")
        con1 = length(which(dt$date==comp.td))
        con2 = length(which(dt$date==nearpoint))
        dt12[j] = nrow(dt)
        if (con1==1 & con2 == 1){
          dt12[j] = dt12[j] -1
        }
        if (dt12[j]<=15){
          similar[j] = 1
        }
      }
      near_comp[j] <- as.Date(as.POSIXct(nearpoint, 'GMT'))
    }
    # Set name columns
    comp.indiv$compared<-comp.indiv$detected     #name of reference
    comp.indiv$similar<-similar
    comp.indiv$ref<-near_comp      # NAME OF COMPARED SERIES
    comp.indiv$dt12<-dt12 
    data<-rbind(data,comp.indiv)
  }
}

# total number of similar points
print(paste0("number of similars :",sum(data$similar), "number of detection ", nrow(data)))
data.new = data[,c("name","compared","ref","similar","dt12")]
#out = data[-1:-4]
write.table(data.new, file = paste0(
                  path.validation,
                  criterion,nb_test.ref,
                  nb_test.comp,detection,
                  "matching.txt"),
            sep="\t",
            col.names = TRUE,
            row.names = FALSE,
            quote=FALSE)
}
# if want to produce a table as a figure, uncomment these lines
# filename = (paste0(path_results,"figure/","compare",criterion,"-new-cutoff-matching.pdf"))
# pdf(filename, height = 20, width = 10)
# grid.table(out,rows = NULL)
# dev.off() 
# system(paste("pdfcrop", filename, filename))

# Make a table of number breakpoints with result of homogenize new data, but the number only count 
# for the same window with truncated data
# criteria <- c("Lav", "BM_BJ", "BM_slope")    # mBIC, Lav, BM_BJ,
# number = data.frame()
# for (j in 1:3){
#   criterion = criteria[j]
#   print(criterion)
#   a = c()
#   b = c()
#   d = c()
#   old <-get(load(paste0(path_results,"validation/",8,"-",criterion,"metacompa.RData")))
#   station <- as.character(unique(old$name))
#   for(i in 1:length(station)){
#     seg_old = old[which(old$name == station[i]),] # extract homogenize for each station
#     seg_old = seg_old[which(as.Date(seg_old$detected) <= "2010-12-31"),]
#     a = c(a,nrow(seg_old))
#     b = c(b,sum(seg_old$valid))
#     d = c(d,sum(seg_old$noise))
#   }
#   print(sum(a))
#   print(sum(b))
#   print(sum(d))
# }





