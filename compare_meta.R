# This function used to check the GNSS breakpoints based on the metadata. 
# Input: station (list name of station), path_raw ( path of time series), path_meta (path of metadata file),
# path_result (path of the result of segmentation and also used for store the result of this function),
# nb_test: number of test used for separate different test
# criterion: "BM_BJ", "BM_sloe", "Lav"
# output: validation file ( including the data frame with colums: name station, detection, closest known change
# from metadata, time difference between two points, validation (1 means it validated), noise (1 means it is outlier))
compare_meta=function(station,path_raw,path_meta,path_result,nb_test,criterion){
  if (nb_test == 2 | nb_test ==4 | nb_test ==7 | nb_test ==9){
    meta <-read.table(paste0(path_meta,"/eq_change_list_mapfun.dates.txt"),skip=1)
  }else{
    meta <-read.table(paste0(path_meta,"/eq_change_list.dates.txt"),skip=1)
  }
  colnames(meta)<-c("name","year","doy","ymd","type")
  DateConvertmeta = as.Date(meta$ymd)
  meta$ymd <-DateConvertmeta
  data=data.frame()
  for(l in 1:length(station)){ 
    stat = meta[which(meta$name ==station[l]),]
    time_series = get(load(paste0(path_raw,station[l],".RData")))
    DateConvert = as.Date(as.POSIXct(time_series$date, 'GMT'))
    time_series$date <- DateConvert
    segment <-get(load(paste0(path_result,"meta_compare/",nb_test,criterion,"position_break",".RData")))
    bkp = segment[which(segment$station == station[l]),] # extract homogenize for each station
    bkp = head(bkp,-1) # Because the segmentation result include the last point of time series
    valid <-rep(0,length(bkp[,1]))
    z <-c(as.Date("00/00/00", "%y/%m/%d"))
    instru <-rep(z,length(bkp[,1]))
    Delta <-rep(0,length(bkp[,1]))
    name_Flag <-rep("a",length(bkp[,1]))
    noise <-rep(0,length(bkp[,1]))
    c = data.frame()
    if (nrow(bkp) == 0){
      noisy = 0
    }else{
      for(i in 1:length(bkp[,1])){
        td = bkp$day[i]
        tknown = stat$ymd
        nearpoint_1 = tknown[which.min(abs(tknown - td))]
        index_near = which.min(abs(tknown - td))
        ind = which((time_series$date) == nearpoint_1)
        meta_flag <- lapply(stat$type, as.character)
        name_flag = meta_flag[index_near]
        if (nearpoint_1 < time_series$date[1]){
          space = time_series$date[1] - nearpoint_1 -1 # consider the space if known point is before the begining of time series
        } else if (nearpoint_1 > time_series$date[length(time_series$date)]){
          space = nearpoint_1 - time_series$date[length(time_series$date)] -1  # consider nearest point is out of time series
        }else{
          space = 0 
        }
        # Checck noise in detection, is there any point near known less than 30 days
        if (nrow(bkp) <= 1){
          noisy = 0
        }else if (nrow(bkp) > 1){
          time_series.nonna = time_series[which(!is.na(time_series$signal)),]
          rownames(time_series.nonna) = 1:nrow(time_series.nonna)
          index_noise = which(time_series.nonna$date == td)
          if (index_noise-30 < 1){
            min_noise = head(time_series.nonna$date,n=1)   # Avoid the maximum out of series
          }else{
            min_noise = time_series.nonna$date[index_noise-30]
          }
          if (index_noise+30 > nrow(time_series.nonna)){
            max_noise = tail(time_series.nonna$date,n=1)   # Avoid the maximum out of series
          }else{
            max_noise =  time_series.nonna$date[index_noise+30]
          }
          noise_meta = min_noise < bkp$day[-i] & bkp$day[-i] < max_noise
          if (mean(noise_meta) != 0){
            noisy = 1
          }else{
            noisy =0
          }
        }
        noise[i] = noisy
        # Check known point is missing in time series and signal is NA at that point
        if (length(ind) == 0){
          flag =0                # because the difference in R count begin and end points, we need to minus 1 when it include all 2 points
          if (td<nearpoint_1){
            keypoint = -1         # Between known point and detection have no space but not the same day, we assume it is 1 day difference
            nearpoint = max(time_series$date[which(time_series$date < nearpoint_1)])
            index = which(time_series$date == nearpoint)
            if (is.na(time_series$signal[index]) == TRUE ){
              while (is.na(time_series$signal[index]) == FALSE) {
                index = index -1
                nearpoint = time_series$date[index]
              }
            }
          }else{
            keypoint= 1
            nearpoint = min(time_series$date[which(time_series$date > nearpoint_1)])
            index = which(time_series$date == nearpoint)
            if (is.na(time_series$signal[index]) == TRUE ){
              while (is.na(time_series$signal[index]) == FALSE) {
                index = index+1
                nearpoint = time_series$date[index]
              }
            }
          }
        } else{
          flag = 1
          nearpoint = nearpoint_1
            if (is.na(time_series$signal[ind]) == TRUE ){
              flag = 0
              if (td< nearpoint){
                while (is.na(time_series$signal[ind]) == FALSE) {
                  index = index -1
                  nearpoint = time_series$date[index]
                }
              } else if(td> nearpoint){
                while (is.na(time_series$signal[ind]) == FALSE) {
                  index = index +1
                  nearpoint = time_series$date[index]
                }
              }
            }
        }
        instru[i] = nearpoint_1
        # Calculate dt12
        if (td< nearpoint){
          time <- subset(time_series, time_series$date >= td & time_series$date <= nearpoint)
          delta = - nrow(na.omit(time)) - space + flag

        }else if (td> nearpoint) {
          time <- subset(time_series, time_series$date >= nearpoint & time_series$date <= td)
          delta = nrow(na.omit(time)) + space -flag
        }
        else{
          delta =0
          if ( nearpoint_1 != td){
            delta = keypoint
          }
            
        }
        if (abs(delta) <= 29){
          valid[i] = 1
        } 
        Delta[i] = delta
        name_Flag[i] = name_flag
      }
    }
    bkp$noise <- noise
    bkp$known <- instru
    bkp$valid<-valid
    bkp$Delta <- Delta
    bkp$flag <- name_Flag
    data<-rbind(data,bkp)
    
  }
  
  name <- data$station
  detected <- data$day
  known <- data$known
  flag <- sapply( data$flag, paste0, collapse="")
  dt12 <- data$Delta
  valid <- data$valid
  noise <- data$noise
  out.pre <- data.frame(name, detected, known, flag,dt12,valid,noise)
  
  ########## Check more than 1 detection near to 1 known change, only keep the valid nearest
  known.pre = nrow(unique(out.pre[c("name", "known")]))
  for ( i in 1:known.pre){
    station.change = unique(out.pre[c("name", "known")])[i,1]
    change.point = unique(out.pre[c("name", "known")])[i,2]
    known.point.pos = subset(out.pre, out.pre$name == station.change & out.pre$known == change.point & out.pre$valid ==1 & out.pre$noise ==1)
    if(nrow(known.point.pos) >=2){
      valid.point.ind.pos = which.min(abs(known.point.pos$dt12))
      valid.point.pos = known.point.pos[valid.point.ind.pos,]
      valid.change.pos = known.point.pos[-valid.point.ind.pos,]
      for (j in 1: nrow(valid.change.pos)){
        valid.change.ind.pos = which(out.pre$name == station.change & out.pre$detected == valid.change.pos$detected[j])
        out.pre$valid[valid.change.ind.pos] = 0
      }
    }
  }
  
  out = out.pre
  ratio <- (sum(out$valid))/(nrow(out))
  save(out,file = paste0(path_result,"validation/",nb_test,"-",criterion,"metacompa.RData"))
  write.table(out, file = paste0(path_result,"validation/",nb_test,"-",criterion,".txt")
              , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)

  filename = paste0(path_result,"figure/",nb_test,criterion,"validation.pdf")
  pdf(filename, height = 20, width = 10)
  grid.table(out,rows = NULL)
  dev.off()
  system(paste("pdfcrop", filename, filename))
  print(paste0("valid", sum(out$valid)))
  print(paste0("detection", nrow(out)))
  print(paste0("noise", sum(out$noise)))
  
  }

