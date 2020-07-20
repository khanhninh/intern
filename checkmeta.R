# check 2 meta data
old <- read.table("eq_change_list_mapfun.dates.txt", skip=1)
new <- read.table("eq_change_list.dates.txt", skip=1)
namecol = c("NAME","YEAR","DOY","DATE","TYPE")
colnames(new) <-namecol
colnames(old)<-namecol

new.m = new[which(new$YEAR <= 2010),]
old.m = old[which(old$TYPE != "P"),]
for (i in name){
  new.sta = new.m[which(new.m$NAME == i),]
  old.sta = old.m[which(old.m$NAME == i),]
  print(as.Date(new.sta$DATE) - as.Date(old.sta$DATE) )
  print(as.Date(new.sta$DATE))
  print(as.Date(old.sta$DATE) )
  
}
new.sta = new.m[which(new.m$NAME == "pol2"),]
old.sta = old.m[which(old.m$NAME == "pol2"),]
new.sta1 = new.sta[which(new.sta$YEAR > 1994),]
as.Date(new.sta1$DATE) - as.Date(old.sta$DATE)
print(as.Date(new.sta1$DATE))
print(as.Date(old.sta$DATE))
