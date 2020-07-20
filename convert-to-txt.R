############### convert to the txt ###############
path_dat ="/Users/khanhninhnguyen/Documents/internipgp/Stage/homo/7/"
for (i in 1:81){
  a <- mget(load(paste0(path_dat,"homo_",name[1],"7.RData")))
  b <- a$Y
  write.table(b, paste0(path_main,"old-time-matched/",name[1],".txt"), sep = " ", quote = FALSE)
}
