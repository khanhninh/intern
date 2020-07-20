homo_list=function(station,k,path_data,path_results,nb_test){
  # k=0: mean running segmentation with 4 criteria, k!=0: no criteria
  for(i in 1:length(station)){
    station_name=station[i]
    Y<-get(load(paste0(path_data,station_name,".RData")))
    if (k ==0){
      GNSS <- GNSSfast(Y,lyear=365.25,lmin=1,Kmax=30,selection.K="All",S=0.75,f=TRUE,selection.f=FALSE,threshold=0.001,tol=1e-4)
    } else  {
      GNSS <- GNSSfast(Y,lyear=365.25,lmin=1,Kmax=k,selection.K="none",S=0.75,f=TRUE,selection.f=FALSE,threshold=0.001,tol=1e-4)
    }
    save(GNSS, file = paste0(path_results,"homo_",station_name,nb_test,".RData"))
  }
}

