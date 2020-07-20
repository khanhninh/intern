# This program used to check the significance differerence in mean shift
# uncomment the pdf function to save figures
# change the file name (in the meta_compare folder) 
# change the saved file name
# Run three function before
nearby_search = function(coor.file, list.name, list.gnss, horizontal, vertical){
  coor = read.table(coor.file, header = TRUE)
  colnames(coor) = c("name","lat","lon","height","altitude")
  last.list <- data.frame(matrix(NA, nrow = 81 , ncol = 10))
  colnames(last.list) <- c("ref.sta","nearest","near1","near2","near3","near4","near5","near8","near9","near10")
  for (i in 1:nrow(coor)){
    if (coor$lon[i] >180){
      coor$lon[i] <- coor$lon[i]-360
    }
  }
  coor$name <- tolower(coor$name)
  lonlat = data.frame(coor$lon,coor$lat)
  lonlat = SpatialPoints(lonlat, proj4string=CRS("+proj=longlat +datum=WGS84"))
  count = 0
  length.nearby =c() # to check at last
  for (i in 1:81){
    ind = which(coor$name == list.name[i])
    ref = lonlat[ind]
    coor.m =coor[-ind,]
    distance.ver = coor[ind,]$altitude - coor[-ind,]$altitude
    lonlat1 = data.frame(coor.m$lon,coor.m$lat)
    spdf <- SpatialPointsDataFrame(coords = lonlat1, data = coor.m,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
    point = SpatialPoints(lonlat1, proj4string=CRS("+proj=longlat +datum=WGS84")) # checked
    distance = distGeo(point, ref)
    spdf$dist <- distance
    spdf$dist.ver <- distance.ver
    sort.fr <- spdf[order(distance),]
    sort.fr$dist.ver = as.numeric(sort.fr$dist.ver)*1000
    sort.fr$dist = as.numeric(sort.fr$dist)/1000
    if (vertical!=0){
      sort.fr = sort.fr[which(abs(sort.fr$dist.ver)<=vertical),]
    }else if (vertical ==0){
      sort.fr = sort.fr
    }
    posible = sort.fr[which(sort.fr$dist<=horizontal),]
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
} # search for nearby station 
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
  homo.code = c()
  homo = 0
  near.name = c()
  list.sta = c()
  possible = c()
  list.detec <-c(as.Date("00/00/00", "%y/%m/%d"))
  for (j in name.station){
    ref.station = j # name staion consider
    valid.sta = valid[which(valid$name == j),]
    nears.fr = as.character(last.list[which(last.list$ref.sta == j),])
    for (l in 1:nrow(valid.sta)){
      breakpoint = valid.sta$detected[l]
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
          for (k in 2:length(na.omit(nears.fr))){
            name.nearby = nears.fr[k]
            near.meta = meta[which(meta$name==nears.fr[k]),]
            series.m = mget(load(paste0(path_series,nears.fr[k],".RData")))
            series.m <- series.m$Y
            Date.m = as.Date(as.POSIXct(series.m$date, 'GMT'))
            series.m$date <- Date.m
            series.m = na.omit(series.m)
            ind.brp = which.min(abs(series.m$date - breakpoint ))
            nb.obs = nrow(series.m)-ind.brp
            if (nb.obs>=182 & ind.brp >182){
              min = breakpoint - 182
              max = breakpoint + 182 
            } else if (nb.obs < 182 & ind.brp>182){
              min = breakpoint - 182
              max = series.m$date[nrow(series.m)]
            } else if (ind.brp<182){
              min = series.m$date[1]
              max = breakpoint + 182
            } # ìf no breakpoint in meta data within this interval, then use this station, if not go to next station
            meta.point = near.meta$ymd
            con = mean(min<meta.point & meta.point<max)
            before = series.m[which(series.m$date >=min & series.m$date < breakpoint),]
            after = series.m[which(series.m$date > breakpoint & series.m$date<max),]
            con1 = nrow(before)>1 & nrow(after)>1
            if (mean(con1)==1){
              possib = 1
              if (con!=0){
                homo = 0 # can not use this time series as nearby
                name.nearby = name.nearby
              } else if (con ==0){
                homo=1
              }
            }else{
              possib = 0
            }
            validation <- c( validation, valid.sta$valid[l])
            possible = c(possible,possib)
            homo.code = c(homo.code, homo)
            near.name = c(near.name, name.nearby)
            list.sta = c(list.sta, ref.station)
            list.detec = c(list.detec, breakpoint)
          }
        }
      }
    }          
  }
  out= data.frame(list.sta,list.detec[-1],validation,homo.code,near.name,possible)
  colnames(out) <- c("ref.station","detected","validated","homo", "nearby.station","possible")
  return(out)
} # search for nearby homogeneous
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
  homo.nearby$nb.before <- NA
  homo.nearby$nb.after <- NA
  long1 = c()
  #colnames(out) = c("station","detection","teststatistic","pvalue")
  for (p in 1:nrow(homo.nearby)){
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
      both$GPS.y <- both$ERAI.y-both$signal.y
      both$GPS.x <- both$ERAI.x-both$signal.x
      both$delta = both$GPS.y-both$GPS.x
      
      # consider location of breakpoint
      ind.dt = which.min(abs(both$date - detection ))
      nb.obs = nrow(both)-ind.dt
      if (nb.obs>=182 & ind.dt >182){
        min = detection -182
        max = detection +182
      } else if (nb.obs < 182 & ind.dt>182){
        min = detection -182
        max = both$date[nrow(both)]
      } else if (ind.dt<182){
        min = both$date[1]
        max = detection +182
      }
      before = both[which(both$date >=min & both$date < detection),]
      after = both[which(both$date > detection & both$date<max),]
      order.free = min(nrow(before),nrow(after))
      differ= nrow(before) - nrow(after)
      ind = nrow(before)
      homo.nearby$nb.before[p] = nrow(before)
      homo.nearby$nb.after[p] = nrow(after)
      if (nrow(before)>1 & nrow(na.omit(after))>1){
        long = c(long,length(before$delta))
        long1 =c(long1, length(after$delta))
        t1 = before$delta
        t2 = after$delta
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
} # make significance test for each detection


########## Read the time series and segmentation result ##########\
library(rgeos)
library(geosphere)
criterion = "Lav"
nb_test = 14
path_series = path_era5
list.era5 = list.files(path_era5)
list.era5 =substr(list.era5 ,1,nchar(list.era5)-6)
coor.file.test = paste0(path_data,"gps_sta_CODE_REPRO_2015_OPER_combi.txt") # height and altitude in km
meta.file = paste0(path_data,"/eq_change_list.dates.txt")
validation.file = paste0(path_results,"validation/",nb_test,"-",criterion,".txt")

list.nearby.station= nearby_search(coor.file.test,name,list.era5,300,0)
# Reoder the name of nearby station 
for (i in 1:nrow(list.nearby.station)){
  b <- rep(as.character(NA), (length(list.nearby.station)-1))
  d <- sort(list.nearby.station[i,2:length(list.nearby.station)])
  if (length(d)>0){
    b[1:(length(d))] <- d
    list.nearby.station[i,2:length(list.nearby.station)] <-b
    
  }
}
list.nearby.station.homo <- homogeneous.nearby(meta.file,validation.file,list.nearby.station,path_series)
list.nearby.station.homo <- list.nearby.station.homo[which(list.nearby.station.homo$possible>0),]
list.t.test <-significance.test(list.nearby.station.homo,path_series)
list.t.test <- na.omit(list.t.test)
last.t.test<-aggregate(sig~detected+ref.station+validated, list.t.test, sum)
print(paste0("NB nonvalid signif : ", table(last.t.test$sig>=1&last.t.test$validated==0)[2], " per ", nrow(last.t.test)))
print(paste0("NB signif : ", table(last.t.test$sig>0)[2], " per ", nrow(last.t.test)))
print(paste0("nb of station inhomogeneous  :",length(unique(last.t.test$ref.station))))
homogeneous <- list.t.test[which(list.t.test$homo==1),]
homo1<-aggregate(sig~detected+ref.station+validated,homogeneous , sum)
print(paste0("nb of station homogeneous : ",length(unique(homo1$ref.station))))
print(paste0("NB nonvalid signif : ", table(homo1$sig>=1&homo1$validated==0)[2], " per ", nrow(homo1)))
print(paste0("NB signif : ", table(homo1$sig>0)[2], " per ", nrow(homo1)))

# check with result of Olivier
# bock <- read.table("/Users/khanhninhnguyen/Downloads/attribution-code/attribution.BM_BJ.txt", header = TRUE)
# bock1 <- na.omit(bock)
# bock2 <- aggregate(signif.~t_break.+valid.+name., bock1, sum)

################# Plot results ###############
# make dataframe including no station, Nb.significance w/wt validation with order: t = 1 year: d=150, 300, limver =500m,
# t = 6 months d =150, 300 km
nb.sta.inhomo = c(37,46,32,37,46)
nb.sta.homo = c(27,39,21,30,42)
nb.signif.inhomo = c(80,120,67,88,145)
tot.nb.signif.inhomo = c(121,182,98,118,179)
nb.signif.homo = c(39,72,32,57,105)
tot.nb.signif.homo = c(64,123,52,79,145)
signif.inhomo <- (nb.signif.inhomo/tot.nb.signif.inhomo)*100
signif.homo <- (nb.signif.homo/tot.nb.signif.homo)*100
nb.sig.inhomo <- c(63,95,50,71,120)
nb.sig.homo <- c(30,58,24,47,89)
sig.homo <- (nb.sig.homo/tot.nb.signif.homo)*100
sig.inhomo <- (nb.sig.inhomo/tot.nb.signif.inhomo)*100

# # change index to test 
# homo = c(signif.homo[1:2])*100
# inhomo = c(signif.inhomo[1:2])*100
homo = c(sig.homo[c(1,2)])
inhomo = c(sig.inhomo[c(1,2)])
percentage = c(inhomo ,homo)
criterion = rep(c("150km","300km"), times =2)
nb = c(rep("inhomo", 2), rep("homo", 2))
mydata <-data.frame(percentage,criterion,nb)
colnames(mydata)<- c("percentage(%)", "Criterion", "nb")

# filename = paste0(path_results,"figure/","valid-noise-lengths.pdf")
# pdf(filename)

# jpeg(paste0(path_results,"figure/",criterion,"test5.jpg"),width = 1500, height = 1000,res = 300) # change name
# plot_lst <- vector("list", length = 2)
# plot_lst[[1]] <- 
jpeg(paste0(path_results,"figure/",criterion,"distance-5.jpg"),width = 1800, height = 1500,res = 300) # change name

ggplot(mydata, aes(criterion, percentage))+
geom_bar(stat = "identity", aes(fill = nb), position = "dodge", width = 0.5)+
scale_fill_manual("var", values = c("homo" = "lightsteelblue4",
                                    "inhomo" = "lightsteelblue1"))+
labs(x = "distance", y = "No. significant invalid detections(%)") +
theme_bw()+
theme(plot.title = element_text(hjust=0, size=12), 
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 12))
dev.off() 

  
plot_lst[[2]] <- ggplot(mydata1, aes(criterion, percentage1)) +
  geom_bar(stat = "identity", aes(fill = nb), position = "dodge", width = 0.5)+
  scale_fill_manual("var", values = c("noise.new" = "lightsalmon4", 
                                      "noise.old" = "lightsalmon"))+
  labs(x = "Criterion", y = "outlier(%)")+
  theme_bw()+
  theme(plot.title = element_text(hjust=0, size=12), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 12))
ml1 <- marrangeGrob(plot_lst, nrow = 2, ncol = 1,top=NULL)
ml1







