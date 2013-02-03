source('~/Dropbox/3DFoam/Gilberto/compGilKevin.R')
# Read in all files
rootDir<-"/Users/maderk/Dropbox/3DFoam/LabelValidation/PottLabelComparison/"
goldFiles<-Sys.glob(paste(rootDir,"*/*/glpor_1.csv",sep="/"))

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
genLabComp<-function(goldFile) {
  labComp.goldData<-readfcn(goldFile)
  labComp.names<-goldToName(goldFile)
  labComp.raw<-lapply(labComp.names,readfcn)
  labComp.growSteps<-unlist(lapply(labComp.names,growFromSample))
  labComp.flatnessSteps<-unlist(lapply(labComp.names,flatnessFromSample))
  labComp.all<-do.call(rbind,addColToDFlist(addColToDFlist(labComp.raw,"FLATNESS",labComp.flatnessSteps),"GROWTH",labComp.growSteps))
  # Match all the foams
  labComp.match<-lapply(labComp.raw,function(cFoam) {compare.foam.frames(labComp.goldData,cFoam)})
  labComp.cor<-do.call(rbind,lapply(labComp.match,corTable))
  labComp.cor$FLATNESS<-labComp.flatnessSteps
  labComp.cor$GROWTH<-labComp.growSteps
  labComp
}  
# Plot a simple histogram
# lapply(tA,function(cObj) {plotHist(cObj,breaks=seq(0,3500,500),xlim=c(0,3500),ylim=c(0,400))})



# Plot code for showing the effect of measurement on various parameters
ggplot(labComp.all,aes(x=FLATNESS,y=VOLUME))+geom_boxplot(aes(groups=as.factor(FLATNESS) ))+scale_y_log10()
ggplot(labComp.all,aes(x=FLATNESS,y=VOLUME))+geom_jitter(aes(color=as.factor(GROWTH) ))+scale_y_log10()

#






showMatchResLine(ggplot(labComp.cor,aes(x=GROWTH,group=as.factor(FLATNESS))))
showMatchResLine(ggplot(labComp.cor,aes(x=FLATNESS,group=as.factor(GROWTH))))

labComp.match.all<-merge.lists(labComp.match)
labComp.match.all$FLATNESS<-labComp.flatnessSteps[labComp.match.all$MEASURE]
labComp.match.all$GROWTH<-labComp.growSteps[labComp.match.all$MEASURE]



ggplot(labComp.match.all,aes(x=(PCA1_S-PCA3_S)/PCA1_S,y=(M_PCA1_S-M_PCA3_S)/M_PCA1_S,colour=as.factor(FLATNESS)))+scale_fill_brewer()+geom_jitter(aes(alpha=0.8,size=as.factor(GROWTH) ))+coord_fixed()
ggplot(labComp.match.all,aes(x=VOLUME,y=M_VOLUME,colour=as.factor(FLATNESS)))+scale_fill_brewer()+geom_jitter(aes(alpha=0.2,size=as.factor(GROWTH)))+coord_fixed()+scale_y_log10()+scale_x_log10()
ggplot(labComp.match.all,aes(x=NEIGHBORS,y=M_NEIGHBORS,colour=as.factor(FLATNESS)))+scale_fill_brewer()+geom_jitter(aes(alpha=0.2,shape=GROWTH))+coord_fixed()

# density plots in 2D , 2d histogram
ggplot(labComp.match.all,aes(x=VOLUME,y=M_VOLUME,colour=as.factor(interaction(labComp.cor$GROWTH,labComp.cor$FLATNESS))))+scale_fill_brewer()+geom_density2d(aes(alpha=0.2))+coord_fixed()+scale_y_log10()+scale_x_log10()
> 
# Volume-face relation
curplot<-ggplot(labComp.match.all)+scale_fill_brewer()+geom_jitter(aes(x=M_VOLUME,y=M_NEIGHBORS,colour=as.factor(FLATNESS),alpha=0.2,shape=as.factor(GROWTH)))+scale_x_log10()
curplot+geom_line(data=goldData,aes(x=VOLUME,y=NEIGHBORS,fill="Truth"))
showMatchRes(ggplot(nwMcor))
stepNames<-mapply(function(a,b) { paste("G:",a," F:",b,sep="")},a=labComp.growSteps,b=labComp.flatnessSteps)
showAllHistograms(ggplot(goldData,aes(x=NEIGHBORS)),labComp.raw,stepNames)

# from center position gplot(subset(allNWmatch,(MEASURE-1)%%3==0),aes(x=sqrt((POS_X-150)^2+(POS_Y-150)^2+(POS_Z-150)^2),y=M_NEIGHBORS-NEIGHBORS,colour=as.factor(STEPS)))+scale_fill_brewer()+geom_jitter(aes(alpha=0.2,size=40-MEASURE))

#ggplot(subset(allNWmatch,MEASURE%%3==0),aes(x=MASK_DISTANCE_MEAN,y=M_MATCH_DIST/OBJ_RADIUS+(abs(VOLUME-M_VOLUME)/VOLUME)^0.33+abs(AISO-M_AISO),colour=as.factor(STEPS)))+scale_fill_brewer()+geom_jitter(aes(alpha=0.2,size=40-MEASURE))+ylim(0,2)+geom_smooth()+ylab("Match Quality (au)")