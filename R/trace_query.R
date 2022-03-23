
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



trace.id<-function(Obj.QOD, event.start,event.end, time.b, time.a, inf.flag,um.time,event.between,event.NOT.between, comp.mat=TRUE){
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
                          arr.NOTpassingThrough = event.NOT.between,
                          returnCompleteMatrix =comp.mat)

  return(arr.ID)
}






