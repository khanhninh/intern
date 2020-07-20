path.validation = paste0(path_results, "validation/")
a <- read.table(paste0(path.validation,"7-BM_BJ.txt"),header = TRUE)
b <- read.table(paste0(path.validation,"8-BM_BJ.txt"),header = TRUE)
d <- b[which(as.Date(b$detected)<=as.Date("2011-01-01")),]
b$count = 1

e=aggregate(b$count,by=list(b$name), FUN = "sum")
sum(e$x)
mean(e$x)

noise.long = aggregate(a$noise, list(a$name), sum)
noise.short = aggregate(b$noise, list(b$name), sum)

colnames(noise.long)<-c("name","noise")
colnames(noise.short)<-c("name","noise")
data1 = inner_join(noise.long,noise.short,by ="name")
data1$delta = data1$noise.x-data1$noise.y

sigma.long = read.table(paste0(path_results,"sigma-f/sigma-f","BM_BJ",7,".txt"), header = TRUE)
sigma1.short = read.table(paste0(path_results,"sigma-f/sigma-f","BM_slope",8,".txt"), header = TRUE)
sigma.long$nb = sigma.long$nb -1
sigma1.short$nb = sigma1.short$nb -1

data2= inner_join(sigma.long,sigma1.short,by ="name")
data2$delta.sigma = data2$stdf.x-data2$stdf.y
data2$delta.var = data2$meansigma.x - data2$meansigma.y
data2$nb = data2$nb.x - data2$nb.y
data2 = data2[,c("name", "delta.sigma","delta.var","nb")]

data3 = inner_join(data1,data2,by ="name")
#data4 = data3[,c("delta", "delta.sigma")]

data5 = inner_join(noise.long ,sigma.long,by ="name")
#data3$product = data3$delta.sigma * data3$delta.var
data3$change = 0
data3$change[which(data3$delta!=0)] =1

x = data3[,c("name","noise.x","stdf.x","meansigma.x","nb.x")]
library(rpart)
library(rpart.plot)
spamCART = rpart(nb ~ stdf+meansigma, data=sigma.long, method="class",cp=0.02)
prp(spamCART)
library(ggplot2)
library(RColorBrewer)
sigma.long$rate = sigma.long$mean/sigma.long$meansigma
sp3<- ggplot(sigma.long, aes(x=rate, y = nb))+geom_point()
sp3+scale_color_gradientn(colours = rainbow(2))

criteria = c("BM_BJ","BM_slope","Lav")
detection.old = c(237,302,386)
detection.new = c(298,375,302)
noise.old = c(24,28,116)
noise.old <- noise.old/detection.old
noise.new =c(32,59,47)
noise.new <- noise.new/detection.new
valid.old = c(46,52 ,65)
valid.old= valid.old/detection.old
valid.new = c(59,64,57)
valid.new= valid.new/detection.new

new = c(valid.new)*100
old = c(valid.old)*100
percentage = c(old,new)
criterion = rep(criteria, times =2)
nb = c(rep("valid.old", 3), rep("valid.new", 3))
mydata <-data.frame(percentage,criterion,nb)
colnames(mydata)<- c("percentage(%)", "Criterion", "nb")

new = c(noise.new)*100
old = c(noise.old)*100
percentage1 = c(old,new)
criterion = rep(criteria, times =2)
nb = c(rep("noise.old", 3),rep("noise.new", 3))
mydata1 <-data.frame(percentage1,criterion,nb)
colnames(mydata1)<- c("percentage(%)", "Criterion", "nb")
#p <-ggplot(mydata, aes(criterion, detection))
#p +geom_bar(stat = "identity", aes(fill = nb), position = "dodge", )

filename = paste0(path_results,"figure/","valid-noise-lengths.pdf")
pdf(filename)

jpeg(paste0(path_results,"figure/",criterion,"test5.jpg"),width = 1500, height = 1000,res = 300) # change name
plot_lst <- vector("list", length = 2)
plot_lst[[1]] <- ggplot(mydata, aes(criterion, percentage))+
  geom_bar(stat = "identity", aes(fill = nb), position = "dodge", width = 0.5)+
  scale_fill_manual("var", values = c("valid.new" = "lightsteelblue4",
                                          "valid.old" = "lightsteelblue1"))+
  labs(x = "Criterion", y = "validation (%)") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0, size=12), 
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text = element_text(size = 12))
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
dev.off() 


library(ggplot2)
library(reshape2)
values <-c(detection.old , detection.new)
barplot(t(dat), beside = T)
ggplot(data = dat, 
      aes(x = criterion, fill = Variable)) + 
  geom_bar(stat = 'identity', position = 'dodge')

###### similar detection #######
similar.de = c(59,87,49) 
all.de = c(256,334,252)/100
similar.de = similar.de/all.de
similar.va = c(11,14,6)
similar.va =similar.va*100/c(53,55,60)
criterion = rep(criteria, times =2)
nb = c(rep("detection", 3), rep("validation", 3))
similar = c(similar.de,similar.va)
mydata <-data.frame(similar,criterion,nb)
filename = paste0(path_results,"figure/","similar-detection.pdf")
pdf(filename)

p <-ggplot(mydata, aes(criterion, similar))
p <- p +geom_bar(stat = "identity", aes(fill = nb), position = "dodge", width = 0.5)
p <- p + labs(x = "Criterion", y = "Percentage of similars (%)")                                      
p+ theme(plot.title = element_text(hjust=0, size=12), 
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text = element_text(size = 12))


dev.off()









