######### Objective function: check the result of homogenization for all stations ##########
# extract the number of validation, noise and detection for all stations and each criterion
# from the validation file
colname = c("name","length","begin","end","delta")
list.new = read.table(paste0(path_results,"list-length-new-data.txt"),skip = 1, col.names = colname)
list.old = read.table(paste0(path_results,"list-length-old-data.txt"),skip = 1, col.names = colname)
# find the cross stations in two data sets
joint = inner_join(list.new,list.old,by="name")
write.table(joint, file = paste0(path_results,"list-cross-stations.txt")
            , sep="\t", col.names = TRUE,row.names = FALSE,quote=FALSE)
########## compare two old validation result ##############
colname.valid = c("name", "detected", "known", "flag", "dt12", "valid", "noise")
prof = read.table(paste0(path_main,"validationref/tot_out_init_BM1_30_30.validation.txt"),skip = 1, col.names = colname.valid)
ninh = read.table(paste0(path_results,"validation/7-BM_BJ.txt"),skip = 1, col.names = colname.valid)
prof$name = as.character(prof$name)
ninh$name = as.character(ninh$name)
aall = data.frame()
ball = data.frame()
for (i in name){
  a = subset(prof, name ==i)
  b = subset(ninh, name == i)
  print(a$dt12 -b$dt12)
  print(a$valid- b$valid)
  print(a$noise -b$noise)
  aall = rbind(aall,a)
  ball = rbind(ball,b)
}
d = outer(prof,ball, by = "name")
##############################----------------####################################




