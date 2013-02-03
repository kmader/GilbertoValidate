
goldToName<-function(gFile) {
  cList<-unlist(strsplit(gFile,"/"))
  cList<-cList[-length(cList)]
  #cList
  Sys.glob(paste(c(cList,"*","clpor_2.csv"),sep="/",collapse="/"))
}
flatnessFromSample<-function(cStr) {
  cRes<-re.extract("FLAT_-?\\d+[.]?\\d*-",cStr)
  as.double(re.extract("-?\\d+[.]?\\d*",cRes))
}
growFromSample<-function(cStr) {
  cRes<-re.extract("GROW_-?\\d+[.]?\\d*/",cStr)
  as.double(re.extract("-?\\d+[.]?\\d*",cRes))
}
readfcn<-function(lacFile) {compare.foam.clean(read.csv(lacFile,skip=1))}
setClass("GroupMatch",representation(goldFile="character",names="character",goldData="data.frame",raw="list",all="data.frame",growSteps="numeric",flatnessSteps="numeric",match="list",cor="data.frame"))
genLabComp<-function(goldFile) {
  labComp<-new("GroupMatch",goldFile=goldFile)
  labComp@goldData<-readfcn(goldFile)
  labComp@names<-goldToName(goldFile)
  labComp@raw<-lapply(labComp@names,readfcn)
  labComp@growSteps<-unlist(lapply(labComp@names,growFromSample))
  labComp@flatnessSteps<-unlist(lapply(labComp@names,flatnessFromSample))
  labComp@all<-do.call(rbind,addColToDFlist(addColToDFlist(labComp@raw,"FLATNESS",labComp@flatnessSteps),"GROWTH",labComp@growSteps))
  # Match all the foams
  labComp@match<-lapply(labComp@raw,function(cFoam) {compare.foam.frames(labComp@goldData,cFoam)})
  labComp@cor<-do.call(rbind,lapply(labComp@match,corTable))
  labComp@cor$FLATNESS<-labComp.flatnessSteps
  labComp@cor$GROWTH<-labComp.growSteps
  labComp
}  