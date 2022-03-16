
#'@title trace id
#'
#'@import pMineR
#'




# #creo oggetto data loader
# objDL <- dataLoader(verbose.mode = FALSE)
#
# #carico EL
# objDL$load.csv(nomeFile = "data/EventLogDiag.csv",sep = ",",IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",format.column.date = "%Y-%m-%d")
#
# objDL.out <- objDL$getData()
#
# #EL
# EventLog<-objDL.out$original.CSV
#
# #creo obj QOD
# objQOD <- QOD()
# objQOD$loadDataset(dataList = objDL.out)
#
# #sull' oggetto QOD sono presenti 3 metodi fondamentali
# ################################################### 1. QUERY   ###################################################################
#
# #seleziono quali pazienti sono andati da Accesstohospital a ICU
# arr.ID <- objQOD$query(from = "AccessToHospital",to = "ICU",returnCompleteMatrix =TRUE)
#
# # from , #stringa evento ini
# # to ,  #stringa evento end
# # complement = FALSE, #paz che NON soddisfano query
# # time.range=c(0,Inf), #tracce entro il time range indicato
# # step.range = c(1,Inf) , #num
# # UM = NA,
# # arr.passingThrough = c(),
# # arr.NOTpassingThrough = c(),
# # returnCompleteMatrix = FALSE
#
# arr.ID
#
# #seleziono quali pazienti sono andati da Accesstohospital a ICU passando per "Tested_positive"
# arr.ID1 <- objQOD$query(from = "AccessToHospital",to = "ICU",arr.passingThrough=c("Tested_Positive"),returnCompleteMatrix =TRUE)
# arr.ID1
#
#
# ################################################ 2. PLOT TIME LINE  ##############################################################
# arr.IPP.to.plot <- objQOD$query(from = C,to = "ICU",arr.passingThrough=c("Tested_Positive"),returnCompleteMatrix =FALSE)
#
#
# objQOD$plotTimeline(objDL.obj = objDL.out,arr.ID = arr.IPP.to.plot,UM = "weeks",ID.on.y.label = FALSE,arr.evt.pch = c("AccessToHospital"=19,"ICU"=17))


trace.id<-function(Obj.QOD, event.start,event.end, time.b, time.a, inf.flag,um.time,event.between, comp.mat=TRUE){
  if(inf.flag){
    time.r<-c(time.b,Inf)
  }else{
    time.r<-c(time.b,time.a)
  }

  arr.ID <- Obj.QOD$query(from = event.start,
                          to = event.end,
                          UM= um.time,
                          arr.passingThrough =event.between,
                          time.range= time.r,
                          returnCompleteMatrix =comp.mat)

  return(arr.ID)
}






