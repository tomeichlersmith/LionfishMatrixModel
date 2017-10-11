# This code is meant to determine the minimum CRNumber required to eradicate
# the population within 100 years of CRBegin depending on CRLength

#Include file that defines simulation function
source("MatrixModelCSim.R")
# Reserved Names (Used in MatrixModelCSim.R):
#   dpv0,identity5,zero5,Acf,Aif,Awf,Awf0,Awm0,dCRBegin,dCRLength,dCRNumber,
#   dDE,dDL,df,dFracCatchable,dHBegin,dHLength,dHNumber,dMA,dME,dMinPop,dMJ0,dML,dMortSlope,
#   dRf,dshortout,JAindices,Jcf,Jcm,Jwf0,Jwm0,Lwf0,Lwm0,BirthDeath,CRISPRintro,cr,H,harv,Harvest
#   MatrixModelCSim,maxcatch,P,rwf,totalpop

CRLengthOpts <- c(1,12,24,36,48,60,72,84,96,108,120) #11
CorrespondingMinCRNum <- NULL

for (i in 1:length(CRLengthOpts)) {
  
  crnum <- 5
  eradtime <- 100
  while (eradtime >= 100 && crnum < 1000) {
    finalnums <- MatrixModelCSim(
      CRNumber = crnum, CRLength = CRLengthOpts[i], nhe = 0,
      shortout = TRUE)
    eradtime <- finalnums["EradTime"]
    crnum <- crnum+5
  }
  
  CorrespondingMinCRNum[i] <- crnum
}

library("ggplot2")
tmpdf <- data.frame(CRLength = CRLengthOpts, MinCRNum = CorrespondingMinCRNum)
ggplot() +
  geom_point(data = tmpdf, aes(x = CRLength, y = CorrespondingMinCRNum)) +
  xlab('CRLength') +
  ylab('MinCRNumber')
