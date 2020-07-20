#name =list.files(path_old_data)
#name_old=substr(name,1,nchar(name)-6)
station = name
criterion = "BM_BJ"
data <-data.frame()
for(i in 1:length(station)){
  station_name=station[i]
  df = mget(load(paste0(path_main,"homo/8/","homo_",station_name,8,".RData")))
  variance <- sqrt(df$GNSS$variances)
#  std.var = sd(variance)
  mean.var = mean(variance)
  delta = max(variance) - min(variance)
  sta = c(mean.var, delta)
  data <- rbind(data, sta)
}
data1 <-data
colnames(data1) <- c("mean", "delta(max-min)")
data1$name = station
mean.noise = round(mean(data1$mean), digits = 3)
mean.delta = round(mean(data1$`delta(max-min)`), digits = 3)
std.mean = round(sd(data1$mean), digits = 3)
std.delta = round(sd(data1$`delta(max-min)`), digits = 3)
text = paste0("mean =", mean.noise, " ", mean.delta, "   & std = ", std.mean, " ", std.delta)
####### Plot the histogram ##############
library(ggplot2)
library(reshape2)
filename = paste0(path_results,"figure/","hist-noise-new-81.pdf")
pdf(filename)
p <- ggplot(melt(data1), aes(value, fill = variable)) 
p <- p +geom_histogram(position = "dodge", bins = 10) 
p <- p + labs(x = "Std of noise", y = "No. stations") 
p+ ggtitle(text)+ theme(plot.title = element_text(hjust=0, size=12), 
                        axis.title.x = element_text(size = 12),
                        axis.title.y = element_text(size = 12),
                        axis.text = element_text(size = 12))
dev.off()

# library(plotrix)
# l <- list(data$mean.var,data$delta)
# multhist(l,legend = (c("mean","delta")))

# xlim = ceiling(max(max(data$delta),max(data$mean.var)))
# breaks = 0.2
# n = xlim/breaks
# bound1= 0
# plot.data <- data.frame()
# for (j in 1:n){
#   bound2 = bound1 + breaks
#   sub.delta <- data[which(data$delta>=bound1 & data$delta <=bound2),]
#   no.delta = nrow(sub.delta)
#   sub.mean <- data[which(data$mean.var>=bound1 & data$mean.var <=bound2),]
#   no.mean = nrow(sub.mean)
#   bound1 = bound2
#   sub.value <- c(no.delta,no.mean)
#   plot.data <-rbind(plot.data, sub.value)
# }
# colnames(plot.data) <- c("no.delta","no.mean")
# x.val <-seq(0,xlim,breaks)
# x.val <- x.val[-1]
# plot.data$x.val <- x.val
# rownames(plot.data) <-x.val


