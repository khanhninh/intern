# This program used to plot the histogram of offset ( mean of interval n+1 minus mean of interval n)
# uncomment the pdf function to save figures
# change the file name (in the meta_compare folder) 
# change the saved file name
########## Plot the histogram of offset ##########\

a = read.table(paste0(path_results, "meta_compare/14BM_BJ.txt"),header = TRUE)
a$offset =0 
offset = c()
name.a = unique(a$station)
for (i in name.a){
  test = a[which(a$station == i),]
  offset.test = 0
  if (nrow(test) >=2){
    for (j in 2:nrow(test)){
      offset.a = test$mean[j]-test$mean[j-1]
      offset.test = c(offset.test, offset.a)
    }
  }
  offset = c(offset, offset.test)
}
a$offset = offset
b = a[which(a$offset!=0),]
mean.abs = round(mean(abs(b$offset)), digits = 3)
print(paste0("mean(abs) = ", mean.abs))
std = round(sd(b$offset),digits = 3)
print(paste0("std = ", std))
# filename = paste0(path_results,"figure/","hist-offset-new.pdf")
# pdf(filename)
hist(b$offset,breaks = 100, xlab = " offset (kg/m2)", 
     main = "histogram of the offset of the new data set",
     xlim = c(-5,5),
     lty="blank", col="gray")
# text = paste0("mean abs =", mean.abs, " std = ", std)
# mtext(text, side=3)
# dev.off() 
