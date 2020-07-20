#rm(list = ls())
#############################  main ###########################
library("usethis")
library("devtools")
library("gfpop")
library("rlist")
library("gridExtra")
library("dplyr") 
library("ggplot2")
library("grid")
library("gridExtra")
library(lattice)
library(reshape2)
library(MASS)
library(survival)
library(coin)
path_main="/Users/khanhninhnguyen/Documents/internipgp/Stage/"
path_results=paste0(path_main,"Results/")
path.validation = paste0(path_results, "validation/")
setwd(path_main)

################## Statistical ssignificance test ######################
criterion = "BM_BJ"
######## Choose the file and version of data
ver1 = read.table(paste0(path_results,"sigma-f/sigma-f",criterion,15,".txt"), header = TRUE)
ver1.valid = read.table(paste0(path_results,"validation/",15,"-",criterion,".txt"), header = TRUE)

ver2 = read.table(paste0(path_results,"sigma-f/sigma-f",criterion,12,".txt"), header = TRUE)
ver2.valid = read.table(paste0(path_results,"validation/",12,"-",criterion,".txt"), header = TRUE)
# calculate the number of breakpoints for each station
ver1$brp = ver1$nb-1
ver2$brp = ver2$nb-1
ver1$stdf = round(ver1$stdf, digits = 3)
ver2$stdf = round(ver2$stdf, digits = 3)

################## Mean noise or STD(f)#############
data1 = inner_join(ver1,ver2,by ="name")
data = data1[c("name", "stdf.x","stdf.y")]   # Modify the column name to meansigma to investigate the mean noise
#
# # ##################  number of breakpoints ############
data1 = inner_join(ver1,ver2,by ="name")
data = data1[c("name", "brp.x","brp.y")]
data1$brp.y = 0
################## Number of noise or validation ###################

ver1.merge <-left_join(ver1, ver1.valid)
ver1.merge$valid[which(is.na(ver1.merge$valid)==TRUE)] =0
ver1.merge$noise[which(is.na(ver1.merge$noise)==TRUE)] =0
ver1.data = ver1.merge[c("name", "valid","noise")]
ver1.last = aggregate(ver1.data$noise, by=list(ver1.data$name), FUN = "sum")  #change name of aggregate column to noise
colnames(ver1.last) <- c("name", "noise")     # Modify this line to choose investigate the noise or validation
# ver1.last = inner_join(ver1,ver1.last)     # Uncomment to turn the investigation to percentage of validation
# ver1.last$per.val = round(ver1.last$valid/ver1.last$brp, digits = 3)
# ver1.last = ver1.last[,c("name","per.val")]

# ver2.merge <-left_join(ver2, ver2.valid)
# ver2.merge$valid[which(is.na(ver2.merge$valid)==TRUE)] =0
# ver2.merge$noise[which(is.na(ver2.merge$noise)==TRUE)] =0
# ver2.data = ver2.merge[c("name", "valid","noise")]
# ver2.last = aggregate(ver2.data$valid, by=list(ver2.data$name), FUN = "sum")   #change name of aggregate column to noise
# colnames(ver2.last) <- c("name", "valid") # Modify this line to choose investigate the noise or validation
# ver2.last = inner_join(ver2,ver2.last)  # Uncomment to turn the investigation to percentage of validation
# ver2.last$per.val = round(ver2.last$valid/ver2.last$brp, digits = 3)#
# ver2.last = ver2.last[,c("name","per.val")]
# 
# data = inner_join(ver1.last,ver2.last,by ="name")

####################### Calculate the difference ###############
data = na.omit(data)
colnames(data) <- c("name", "ver1","ver2")
dat.m <- melt(data, measure.vars=c("ver1", "ver2"), id = "name")
data$`delta(ver2-ver1)` <- data$ver2 - data$ver1
data <- data[order(data$`delta(ver2-ver1)`, decreasing = TRUE),]  
#dat.m <- melt(data, measure.vars=c("ver1", "ver2","delta(ver2-ver1)"))
#data$ratio = abs(data$`delta(ver2-ver1)`)/abs(data$ver1)
#dat.m <- melt(data, measure.vars=c("ver1", "ver2","delta(ver2-ver1)","ratio"))
shapiro.test(data$ver1)
shapiro.test(data$ver2)

######## Paired significance test #############
# After compare to the result of Bock perform two sided test instead of one side
# & apply the appximation
test <- wilcox.test(data$ver1,data$ver2, 
                    alternative = "two.sided", 
                    paired = TRUE, correct = FALSE, exact = FALSE)
# print test statistic ( T = min W+ and W-, p value, all info)
test
# calculate z score and effective size
library(rcompanion)
z.va <- wilcoxonZ(x =data$ver1, y = data$ver2,paired = TRUE)
es <- abs(z.va)/sqrt(81)
print(paste0("z score    ", z.va, "    effective size     ", es ))
# calculate mannualy
diff <- c(data$ver1 - data$ver2) #calculating the vector containing the differences
diff <- diff[ diff!=0 ] #delete all differences equal to zero
diff.rank <- rank(abs(diff)) #check the ranks of the differences, taken in absolute
diff.rank.sign <- diff.rank * sign(diff) #check the sign to the ranks, recalling the signs of the values of the differences
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0]) #calculating the sum of ranks assigned to the differences as a positive, ie greater than zero
ranks.neg <- -sum(diff.rank.sign[diff.rank.sign < 0]) #calculating the sum of ranks assigned to the differences as a negative, ie less than zero
ranks.neg
ranks.pos


# before compare to the result of Bock
wilcox.test(data$ver2, data$ver1, 
            alternative = "two.sided", 
            paired = TRUE,
            #            conf.int = TRUE, 
            conf.level = 0.95, 
            exact = F)

##################### Plot box plot - use for number of breakpoints ###############################
# 
# d <- list(ver1$brp, ver2$brp)
# names(d) <- c("version 1", "version 2")
# par(mgp=c(3,2,0))
# filename = paste0(path_results,"figure/","boxplot-14-16.pdf")
# pdf(filename)
# boxplot(d , col="#69b3a2" , ylab="value")
# par(cex.lab=1.4) # is for y-axis
# par(cex.axis=1.4)
# dev.off() 

############### Plot comparison ###################

# with sigma and variance: use geom_point instead of geom_col
data = data1[c("brp.x","brp.y","name")]  # change this line when choose different variables 
colnames(data) <- c("ver1","ver2","name")
data$name <- factor(data$name, levels = data$name[order(data$ver2-data$ver1)])
df2 <- melt(data, id.vars='name') # melting data including only ver1 and ver2 to plot the first figure
data$diff <- data$ver2-data$ver1
data$name <- factor(data$name, levels = data$name[order(data$diff)]) # melting again to remain the name order with the first 

jpeg(paste0(path_results,"figure/",criterion,"test.jpg"),width = 2400, height = 1000,res = 300) # change name
plot_lst <- vector("list", length = 2)
plot_lst[[1]] <- ggplot(df2, aes(x=name, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge',width=0.5) +
  theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.5, .95),
        legend.direction = "horizontal",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2, "cm"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size=6))+
  ggtitle(paste0("Number of breakpoints ", criterion)) # change name
plot_lst[[2]] <- ggplot(data, aes(x = name, y = diff)) +
  geom_col(width=0.5) + xlab("station") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
ml1 <- marrangeGrob(plot_lst, nrow = 2, ncol = 1,top=NULL)
ml1
dev.off() 

############### Plot histogram to choose the appropriate significance test ###########
# Note that with the normal distribution, we used t-test
# with free distribution, we use wilcoxon 
# jpeg(paste0(path_results,"figure/",criterion,"per-val-hist-2-ver-REPRO-2015.jpg"),width = 2400, height = 1000,res = 300)
# ggplot(dat.m, aes(value)) + 
#   facet_wrap(~variable,scales = "free_y") + 
#   geom_histogram(binwidth = 0.1) + 
# #  coord_cartesian(xlim = c(-5, 10)) + 
#   labs(x = "Validation percentage", y = "No. Stations") 
# dev.off() 
# library("carData")
# library("car")
# qqPlot(data$ver2)


############# test why is different from the results of Olivier ################
# ver1.m = ver1[which(ver1$brp != 0),]
# ver2.m = ver2[which(ver2$brp != 0),]
# new = inner_join(ver1,ver2, by = "name")
# list.name = inner_join(ver1.m, ver2.m, by = "name" )
# data = list.name[c("name", "stdf.x","stdf.y")]   # Modify the column name to meansigma to investigate the mean noise
# colnames(data) <- c("name", "ver1","ver2")
# 
# 
# path_new ="/Users/khanhninhnguyen/Documents/internipgp/Stage/Results/REPRO_CODE_2015/"
# path.ver1 = paste0(path_new, "ver1/REPRO_CODE_2015_ver1_BM_BJ_monthly_var_stdf.txt")
# path.ver2 = paste0(path_new, "ver2/REPRO_CODE_2015_ver2_BM_BJ_monthly_var_stdf.txt")
# 
# ver1.n = read.table(path.ver1, header = T)
# ver2.n = read.table(path.ver2, header = T)
# ver1.n = ver1.n[,c("station", "stdf")]
# ver2.n = ver2.n[,c("station", "stdf")]
# data.n = inner_join(ver1.n,ver2.n,by ="station")
# colnames(data.n) <- c("name", "ver1","ver2")
# 
# 
# dat = inner_join(data, data.n, by = "name")
# colnames(data) <- c("name", "ver1","ver2")
# test <- wilcox.test(data$ver1,data$ver2, 
#             alternative = "two.sided", 
#             paired = TRUE, correct = FALSE, exact = FALSE)

#################### Plot the std(diff) between two versions #############
#all.fr = data.frame(matrix(NA, nrow = 81 , ncol = 8509)) maximum length of diffrence
sd.diff <- c()
station <- c()
mean.diff<- c()
for (i in 1:81){
  era1 <- mget(load(paste0(path_2015_ver1,name[i],".RData")))
  DateConvert = as.Date(as.POSIXct(era1$Y$date, 'GMT'))
  era1$Y$date <- DateConvert
  era1.m = era1$Y
  era2 <- mget(load(paste0(path_2015_ver2,name[i],".RData")))
  DateConvert = as.Date(as.POSIXct(era2$Y$date, 'GMT'))
  era2$Y$date <- DateConvert
  era2.m = era2$Y
  erai = inner_join(era1.m, era2.m, by = "date") # ??????? the nay se ko thay dc su khac biet trong missing data
  erai = na.omit(erai)
  diff = erai$GPS.x-erai$GPS.y
  sd.diff <- c(sd.diff, sd(diff))
  station <- c(station, name[i])
  mean.diff <- c(mean.diff, mean(diff))
  # all.fr[i,2:length(diff)] <-diff
  # all.fr[i,1] <- name[i]
  
}
############### Try to do with box plot but not so different between stations
# skip this direction
ggplot(dat.m, aes(x=sta, y =value))+
  geom_boxplot() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size=6))
# From this step, begin to calculate the mean and plot it 
out <-data.frame(station,sd.diff,mean.diff)
colnames(out) <- c("station","std.diff","mean.diff")
jpeg(paste0(path_results,"figure/difference-taoversions.jpg"),width = 1800, height = 1500,res = 300) # change name
ggplot(out, aes(x=mean.diff, y=std.diff, label = station))+
  geom_point() + 
  xlab("Mean of differences") + 
  ylab("Standard deviation of differences") + 
  geom_text(hjust = 0, nudge_x = 0.001,size = 3)
dev.off() 
# line plot
out$station <- factor(out$station, levels = out$station[order(out$std.diff)])
jpeg(paste0(path_results,"figure/difference-twoversions.jpg"),width = 2400, height = 1000,res = 300) # change name
plot_lst <- vector("list", length = 2)
plot_lst[[1]] <- ggplot(out, aes(x=station, y=std.diff, group=1))+
  geom_line() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size=6))
plot_lst[[2]] <- ggplot(out, aes(x = station, y= mean.diff)) +
  geom_col(width=0.5)+
  xlab("station") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
ml1 <- marrangeGrob(plot_lst, nrow = 2, ncol = 1,top=NULL)
ml1
dev.off() 

a$name <- factor(a$name, levels =a$name[order(a$brp)])
df2 <- melt(a, id.vars='name') # melting data including only ver1 and ver2 to plot the first figure
jpeg(paste0(path_results,"figure/oulier.jpg"),width = 2400, height = 1000,res = 300) # change name
ggplot(a, aes(x=name, y=noise)) +
  geom_col(width=0.5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.5, .95),
    legend.direction = "horizontal",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.2, "cm"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, size=6))+
  ggtitle(paste0("Number of outliers ", criterion)) # cha
dev.off() 

