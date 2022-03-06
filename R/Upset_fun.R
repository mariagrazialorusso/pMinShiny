#'@title Upset plot
#'@description this function creates an upset plot in order to analyze the co-occurrence of the events
#'by showing the frequencies of the combination sets.
#'
#'@import pMineR
#'@import dplyr
#'@import UpSetR
#'





# library(pMineR)
# library(UpSetR)
# library(dplyr)

upset_fun<-function(data,chosen,objDL.new){
  objDL.new$applyFilter(array.events.to.keep = chosen)
  # id<-select(data,1)[,1]
  # date_var<-as.Date(select(data,2)[,1])
  # events<-as.factor(select(data,4)[,1])
  # new_data<-data.frame(id,date_var,events)
  # objDL.new <- dataLoader(verbose.mode = FALSE)
  # objDL.new$load.data.frame(mydata = new_data,IDName = "id",EVENTName = "events",dateColumnName = "date_var",
  #                           format.column.date = "%Y-%m-%d")
  objDL.new.export <- objDL.new$getData()
  df<-as.data.frame(objDL.new.export$original.CSV)
  names<-unique(df[,5])
  upsetMat<-matrix(nrow = length(unique(df[,1])), ncol = length(names))
  # names<-unique(new_data$events)
  # upsetMat<-matrix(nrow = length(unique(new_data$id)), ncol = length(names))
  colnames(upsetMat)<-names

  for(i in c(1:dim(upsetMat)[1])){
    arr.evt.to.chech <-objDL.new.export$pat.process[[as.character(df$ID[i])]]$EVENT
    # arr.evt.to.chech <-objDL.new.export$pat.process[[as.character(i)]]$events
    for(j in c(1:dim(upsetMat)[2])){
      if(colnames(upsetMat)[j] %in% arr.evt.to.chech){
        upsetMat[i,j]<-1
      }else{
        upsetMat[i,j]<-0
      }
    }
  }

  upsetData<-as.data.frame(upsetMat)
  # tot<-colSums(upsetData)
  # tot<-tot[!tot==0]
  #
  # upsetData<-subset(upsetData, select = names(tot))
  upset(upsetData,nsets = length(names), point.size = 3.5, line.size = 1.8,
        mainbar.y.label = "Events Intersections",
        sets.x.label = "Event Type",
        keep.order = TRUE,
        mb.ratio = c(0.55, 0.55),
        order.by = "freq",
        text.scale = c(1.3, 1.3, 1, 1, 1.7, 1.2))

}
