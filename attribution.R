library(rgeos)
library(geosphere)
criterion = "BM_BJ"
nb_test = 14
path_series = path_era5    # path of data GNSS-ERA5
list.era5 = list.files(path_era5)
list.era5 =substr(list.era5 ,1,nchar(list.era5)-6)
coor.file.test = paste0(path_data,"gps_sta_CODE_REPRO_2015_OPER_combi.txt") # file contains coordinates of all stations
a = nearby_search(coor.file.test,name,list.era5)
meta.file = paste0(path_data,"/eq_change_list.dates.txt")
validation.file = paste0(path_results,"validation/",nb_test,"-",criterion,".txt")
a1 = homogeneous.nearby(meta.file,validation.file,a,path_series)
a2 = significance.test(a1,path_series)


# All functions 
########## search for nearby station #######
# input: coor.file: file contains coordinates of all stations
# list.name: list name of 81 stations
# list.gnss: list all files we have in GNSS-ERA5
# output: dataframe, each row contains the name of station (ref) and possible nearby station (limit of distance = 150km)
nearby_search = function(coor.file, list.name, list.gnss){
  coor = read.table(coor.file, header = TRUE)
  colnames(coor) = c("name","lat","lon","height","altitude")
  for (i in 1:nrow(coor)){
    if (coor$lon[i] >180){
      coor$lon[i] <- coor$lon[i]-360
    }
  }
  
  coor$name <- tolower(coor$name)
  lonlat = data.frame(coor$lon,coor$lat)
  lonlat = SpatialPoints(lonlat, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  last.list <- data.frame(matrix(NA, nrow = 81 , ncol = 7))
  colnames(last.list) <- c("ref.sta","nearest","near1","near2","near3","near4","near5")
  count = 0
  length.nearby =c() # to check at last
  for (i in 1:81){
    ind = which(coor$name == list.name[i])
    ref = lonlat[ind]
    coor.m =coor[-ind,]
    lonlat1 = data.frame(coor.m$lon,coor.m$lat)
    spdf <- SpatialPointsDataFrame(coords = lonlat1, data = coor.m,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
    point = SpatialPoints(lonlat1, proj4string=CRS("+proj=longlat +datum=WGS84")) # checked
    distance = distGeo(point, ref)
    spdf$dist <- distance
    sort.fr <- spdf[order(distance),]
    sort.fr$dist = as.numeric(sort.fr$dist)/1000
    posible = sort.fr[which(sort.fr$dist<=150),]
    list.near = c()
    for (p in 1:nrow(posible)){
      era5.ind = length(which(list.gnss == posible$name[p]))
      if (era5.ind!=0){
        list.near = c(list.near, posible$name[p])
      }
    }
    last.list[i,1:(length(list.near)+1)] <- c(name[i],list.near)
    length.nearby = c(length.nearby, length(list.near))
    if(length(list.near)!=0){
      count = count+1
    }
  }
  return(last.list)
}

######### Finding homogenous nearby station ###############
# input: meta.file(name file of metadata), validation.file, last.list ( output of nearby_search function),
# path_series (path of time series GNSS-ERA5)
# output:data frame includes name of 81 stations, all breakpoints, validation of breakpoints, 
# homo = 1: can find the homogeneous nearby station for this breakpoints, homo = 0 otherwise
# nearby.station = name of nearby station, "nothave" meaning that no nearby station, 
# "name station + inhomogeneous": near by station is not homogeneous during 1 year before anf after breakpoints
homogeneous.nearby = function(meta.file,validation.file,last.list,path_series){
  meta <-read.table(meta.file,skip=1)
  colnames(meta)<-c("name","year","doy","ymd","type")
  DateConvertmeta = as.Date(meta$ymd)
  meta$ymd <-DateConvertmeta
  valid = read.table(validation.file, header = TRUE)
  DateConvert = as.Date(as.POSIXct(valid$detected, 'GMT'))
  valid$detected <- DateConvert
  name.station = unique(valid$name)
  sta.near <-c()
  validation = c()
  homo = 0
  homo.code = c()
  near.name = c()
  list.sta = c()
  list.detec <-c(as.Date("00/00/00", "%y/%m/%d"))
  for (j in name.station){
    ref.station = j # name staion consider
    valid.sta = valid[which(valid$name == j),]
    nears.fr = as.character(last.list[which(last.list$ref.sta == j),])
    for (l in 1:nrow(valid.sta)){
      breakpoint = valid.sta$detected[l]
      validation <- c( validation, valid.sta$valid[l])
      ## filter stations have no nearby 
      if (length(na.omit(nears.fr)) == 1){
        homo = 0
        name.nearby = "nothave"
      } else if(length(na.omit(nears.fr)) >= 2){ #consider the station have nearby <150km !!!! tiep tu day
        near.meta = meta[which(meta$name==nears.fr[2]),]
        k=2
        if (nrow(near.meta)==0){ # truong hop ko có breakpoints trong metadata
          homo = 1
          name.nearby = nears.fr[2]
        }else if  (nrow(near.meta)!=0){ # in case have breakpoints in metadata
          while(homo ==0){
            name.nearby = nears.fr[k]
            near.meta = meta[which(meta$name==nears.fr[k]),]
            series.m = mget(load(paste0(path_series,nears.fr[k],".RData")))
            series.m <- series.m$Y
            Date.m = as.Date(as.POSIXct(series.m$date, 'GMT'))
            series.m$date <- Date.m
            series.m = na.omit(series.m)
            ind.brp = which.min(abs(series.m$date - breakpoint ))
            nb.obs = nrow(series.m)-ind.brp
            if (nb.obs>=365 & ind.brp >365){
              min = series.m$date[(ind.brp-365)]
              max = series.m$date[(ind.brp+365)]
            } else if (nb.obs < 365 & ind.brp>365){
              min = series.m$date[ind.brp-365]
              max = series.m$date[nrow(series.m)]
            } else if (breakpoint<365){
              min = series.m$date[1]
              max = series.m$date[breakpoint+365]
            } # ìf no breakpoint in meta data within this interval, then use this station, if not go to next station
            meta.point = near.meta$ymd
            con = mean(min<meta.point & meta.point<max)
            if (con!=0){
              homo = 0 # can not use this time series as nearby
              name.nearby = paste0(name.nearby,"-inhomogeneous")
            }else if (con ==0){
              homo=1
              break
            }
            if (k == length(na.omit(nears.fr))){
              break
            }
            k = k+1
          }
        }
      }
      homo.code = c(homo.code, homo)
      near.name = c(near.name, name.nearby)
      list.sta = c(list.sta, ref.station)
      list.detec = c(list.detec, breakpoint)
    }          
  }
  out= data.frame(list.sta,list.detec[-1],validation,homo.code,near.name)
  colnames(out) <- c("ref.station","detected","validated","homo", "nearby.station")
  return(out)
}
#################### significant test ###########################\
# calculate the statistic of t-test for each breakpoints 
# input: homo.nearby (output of homogeneous.nearby function), path_series: path of time series GNSS_ERA5
# output: p-value, t statistic for each breakpoint that can find homogeneous nearby station
# significant value = 1 : this point is validated GNSS origin
significance.test = function(homo.nearby,path_series){
  signif = c()
  t =c()
  sta = c()
  significance = c()
  brp = as.Date("12-12-2020")
  long =c()
  homo.nearby$t <- NA
  homo.nearby$p <- 1
  homo.nearby$sig <- NA
  long1 = c()
  #colnames(out) = c("station","detection","teststatistic","pvalue")
  for (p in 24:27){
    turn = nchar(as.character(homo.nearby$nearby.station[p]))
    if (turn==4){
      detection = homo.nearby$detected[p]
      station.ref = homo.nearby$ref.station[p]
      station.near = homo.nearby$nearby.station[p]
      # read two time series
      series.ref = mget(load(paste0(path_era5,station.ref,".RData")))
      series.ref <- series.ref$Y
      DateConvert.ref = as.Date(as.POSIXct(series.ref$date, 'GMT'))
      series.ref$date <- DateConvert.ref
      series.m.ref = na.omit(series.ref)
      
      series.near = mget(load(paste0(path_era5,station.near,".RData")))
      series.near <- series.near$Y
      DateConvert.near = as.Date(as.POSIXct(series.near$date, 'GMT'))
      series.near$date <- DateConvert.near
      series.m.near = na.omit(series.near)
      
      both = inner_join(series.m.ref,series.m.near, by = "date")  # checked
      both$delta = both$GPS.x-both$GPS.y
      
      # consider location of breakpoint
      ind.dt = which.min(abs(both$date - detection ))
      nb.obs = nrow(both)-ind.dt
      if (nb.obs>=365 & ind.dt >365){
        min = both$date[ind.dt] -365
        max = both$date[ind.dt] +365
      } else if (nb.obs < 365 & ind.dt>365){
        min = both$date[ind.dt]-365
        max = both$date[nrow(both)]
      } else if (ind.dt<365){
        min = both$date[1]
        max = both$date[ind.dt]+365
      } 
      min = which.min(abs(both$date - min))
      max = which.min(abs(both$date - max))
      before = both[min:ind.dt-1,]
      after = both[ind.dt:max,]
      print(paste0( station.ref,detection, nrow(before), nrow(after)))
      t1 = as.numeric(before$delta)
      t2 = as.numeric(after$delta)
      if (length(t1) >1 & length(t2) >1){
        sig = t.test(x = t1, y = t2, var.equal = FALSE, paired = FALSE)
        homo.nearby$t[p] = round(sig$statistic, digits = 3)
        homo.nearby$p[p] = round(sig$p.value, digits = 3)
        if (abs(sig$statistic)>1.96){
          homo.nearby$sig[p] = 1
        } else{
          homo.nearby$sig[p] = 0
        }
      }
    }
  }
  return(homo.nearby)
}

