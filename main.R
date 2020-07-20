rm(list = ls())
#############################  main ###########################
library("usethis")
library("devtools")
library("gfpop")
library("GNSSfast")
library("rlist")
library("gridExtra")
library("dplyr") 
library("ggplot2")
library("grid")
library("gridExtra")

path_main="/Users/khanhninhnguyen/Documents/internipgp/Stage/"
path_code=paste0(path_main,"CodeR/analyse/")
path_2015_ver1=paste0(path_main,"Data/new_R_data/") #8
path_old_data=paste0(path_main,"Data/old_GNSS/") #7
path_data = paste0(path_main,"Data/")
path_results=paste0(path_main,"Results/")
path_2015_ver2 = paste0(path_main,"Data/ERAI2/") #12
path_era5 = paste0(path_main,"Data/R_ERA5/") #14
path.era5.old = paste0(path_main,"Data/era5/") #13
path.erai.v2b = paste0(path_data,"R_ERAI_v2b/") #16
path.erai.era5 = paste0(path_data,"R_ERAI_ERA5/") #15
path.validation = paste0(path_results, "validation/")
path.v1b = paste0(path_data,"R_ERAI_v1b/") #17
path_dat ="/Users/khanhninhnguyen/Documents/internipgp/Stage/Data/matching/new/"

setwd(path_main)
############### List function ###############
source(paste0(path_code,"homo_list.R"))
source(paste0(path_code,"plot_table.R"))
source(paste0(path_code,"timematching.R"))
source(paste0(path_code,"plot_timeseries.R"))
source(paste0(path_code,"monthly.R"))
source(paste0(path_code,"compare_meta.R"))
source(paste0(path_code,"cutoff.R"))
source(paste0(path_code,"extract_sigma_f.R"))
#source(paste0(path_main,"CodeR/","compare_similarity.R")) #old function
source(paste0(path_code,"compare_similarity1.R")) #new function
#### Parameter for running function ###############
# list  cross stations for two data set
list.station = read.table(paste0(path_results,"list-cross-stations.txt"),skip = 1)
name = as.character(list.station[,1])
# name =list.files(path_old_data)
# name_old=substr(name,1,nchar(name)-6)
# # # 2 : old, 3 : new, 4 : old match ,5 new match, 6 new cutoof, 7 entire old, 8 entire new, 
# 9 old matched, 10 new matched, 11 new limited, 12 version 2 of CODE 2015, 13 era5
# 14: fixed era (GNSS fixed), 15 ERAI-ERA5, 16 GNSS(ERA5) - ERAI
nb_test = 14 # All result are indentified by number test, only raw data is not include
#name <- c('pol2','stjo','dubo','mcm4')
criterion = "BM_BJ"   #  "BM_BJ"    # mBIC, Lav, BM_BJ, 
# Path data : paste0(path_data,"matching/old/")
#similarity(path.validation,path_results,path_2015_ver2,path_2015_ver1,12,8,0,"Lav")

############## Homogenization ################################
#homo_list=function(station,k,path_data,path_results,nb_test)
# k = 0 mean running segmentation with 4 criteria, k!=0: number of fixed segment#
homo_list("wtzl",0,path_era5,path_main,nb_test)

############# Extract a table of number and position ############
# plot_table=function(station,k,path_data_homo,path_raw,path_result,nb_test,criterion)
plot_table("joze",0,paste0(path_main,"homo/11/"), path_dat, path_results,nb_test,criterion)
#compare_meta=function(station,path_raw,path_meta,path_result,nb_test,criterion) print nb of valid, detection,noise
compare_meta(name,path_dat,path_data,path_results,nb_test,criterion) 

extract.std(name,0, paste0(path_main,"homo/11/"), path_results, nb_test, criterion )


############# Create a matching time or cutoff data ####################
#timematching(name,path_new_data,path_old_data,paste0(path_data,"matching/"))
#timecutoff(name,path_new_data,path_old_data,path_data,nb_test)
  
############# Plot time series #################################
# plot_timeseries = function(station,k, path_raw, path_homo, path_result, nb_test )
plot_timeseries(c("nrc1"),0, path_2015_ver1, paste0(path_main,"homo/12/"),paste0(path_results,"/figure/"), nb_test, criterion )
plot_timeseries(name,0, paste0(path_data,"matching/old/"),paste0(path_results,"homo/"),paste0(path_results,"/figure/"), nb_test, criterion )
################## Extract monthly difference ##################
monthly(name,path_new_data,path_old_data,path_results,nb_test)

########## Extract std(f) and mean of  variances ################
#extract.std = function(station,number.segment, path_homo, path_result, nb_test, criterion )
extract.std(name,0, paste0(path_main,"homo/15/"), path_results, nb_test, criterion )
######### test ###########
criterion = "BM_slope"
sigma2 = read.table(paste0(path_results,"sigma-f/sigma-f",criterion,12,".txt"), header = TRUE)
sigma2$detection = sigma2$nb-1
mean(sigma$detection) # Mean of the number of detections
data1 = inner_join(sigma,sigma2,by ="name")

a = length(unique(sigma$detection))+1

filename = paste0(path_results,"figure/",8,criterion,"histogram-detection.pdf")
pdf(filename)
hist(sigma$detection, breaks = a, xlab = "Number of breakpoints", ylab = "frequency", main = "The new data set",ylim = c(0,25),xlim = c(0,15))
dev.off() 
system(paste("pdfcrop", filename, filename))
######################### meta data of 81 stations ######################

meta <-read.table(paste0(path_data,"/eq_change_list_mapfun.dates.txt"),skip=1)
colnames(meta)<-c("name","year","doy","ymd","type")
DateConvertmeta = as.Date(meta$ymd)
meta$ymd <-DateConvertmeta
data=data.frame()
for(l in 1:length(name)){ 
  stat = meta[which(meta$name ==name[l]),]
  data<-rbind(data,stat)
}
  
##################  Similarity ####################
# similarity=function(path.validation,path_result,
# path_ref,path_comp,nb_test.ref,nb_test.comp,detection,criterion)
# detection =1: number of similar detection
# detection =0: number of similar validation
path.validation = paste0(path_results, "validation/")
similarity(path.validation,path_results,,path_2015_ver1,11,8,1,"BM_BJ")

#similarity=function(path.validation,path_result,path_ref,path_comp,nb_test.ref,nb_test.comp,detection,criterion){
ninh = read.table(paste0(path.validation,"similar_breakpoints_BM_slope.txt"), header = TRUE)
bock = read.table("similar_detect_BM_slope.txt", header = TRUE)
name.file = name
l <- data.frame()
l = c()
for (i in 24:24){
  a <- mget(load(paste0(path_2015_ver1,name.file[i],".RData")))
  DateConvert = as.Date(as.POSIXct(a$Y$date, 'GMT'))
  a$Y$date <- DateConvert
  b <- na.omit(a$Y)
  a1 <- mget(load(paste0(path_2015_ver2,name.file[i],".RData")))
  DateConvert = as.Date(as.POSIXct(a1$Y$date, 'GMT'))
  a1$Y$date <- DateConvert
  b1 <- na.omit(a1$Y)
  d = inner_join(b, b1, by = "date")
  me = mean(d$ERAI.x-d$ERAI.y)
  l <- c(l,me)
}
Y$diff = Y$GPS-Y$ERAI
jpeg(paste0(path_results,"figure/",criterion,"test4.jpg"),width = 2400, height = 1000,res = 300) # change name
# plot_lst <- vector("list", length = 2)
#plot_lst[[1]] <- 
  ggplot(data = Y, aes(x = date)) +
  geom_line(aes(y = GPS, colour = "GPS")) +
  geom_line(aes(y = ERAI, colour = "ERAI")) +
  geom_line(aes(y = test, colour = "ERAI-trend"),linetype = 2)+
  geom_line(aes(y = test1, colour = "GNSS-trend"),linetype = 2)+
  scale_colour_manual("",
                      breaks = c("GPS", "ERAI","ERAI-trend","GNSS-trend"),
                      values = c("black", "red","yellow","blue"))+
  ylab("Intergrated water vapour (kg/m2)")+ xlab("Time")+
  theme_bw()+
  theme(legend.position = c(0.9, .9),
        legend.text=element_text(size=6),
        axis.text.x = element_text(size=10))
dev.off() 
  
# plot_lst[[2]] <- 
ggplot(Y, aes(x = date, y = diff)) +
  geom_line( color="gray") + xlab("Time") +
  geom_vline(xintercept = as.numeric(Y$date[595]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[596]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[1444]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[1697]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[1762]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[2034]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[3657]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[3658]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[3820]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[5018]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[5167]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[6181]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[6994]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[8114]), lty=2,
             color = "blue", size=0.8)+
  geom_vline(xintercept = as.numeric(Y$date[69]), lty=1,
             color = "red", size=0.5)+
  geom_vline(xintercept = as.numeric(Y$date[1704]), lty=1,
             color = "red", size=0.5)+
  geom_vline(xintercept = as.numeric(Y$date[2034]), lty=1,
             color = "red", size=0.5)+
  geom_vline(xintercept = as.numeric(Y$date[3822]), lty=1,
             color = "red", size=0.5)+
  geom_vline(xintercept = as.numeric(Y$date[5017]), lty=1,
             color = "red", size=0.5)+
  geom_vline(xintercept = as.numeric(Y$date[6490]), lty=1,
             color = "red", size=0.5)+
  ylab("IWV difference (kg/m2)")+
#  ggtitle(paste0("No. detections = ", 6, "  No. outliers = ", 0, "   No. validation = 0"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=10))
# ml1 <- marrangeGrob(plot_lst, nrow = 2, ncol = 1,top=NULL)
# ml1
dev.off() 
##################### sort files can not segment #######################
list.file = c()
name =list.files(path_era5)
name_old=substr(name,1,nchar(name)-6)
for (i in 1:length(name_old)){
  station <- mget(load(paste0(path_era5,name_old[i],".RData")))
  station1 <- na.omit(station$Y)
  month.var <- table(station1$month)
  nb.obs = mean(month.var>3) # modify this threshold
  if (nrow(station1) <= 31 | nb.obs <1){
    list.file <- c(list.file,name_old[i])
  } 
}








