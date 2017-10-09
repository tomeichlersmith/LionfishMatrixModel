
#Include file that defines simulation function
source("MatrixModelCSim.R")
# Reserved Names (Used in MatrixModelCSim.R):
#   Acf, Acm, Aif, Awf, BirthDeath, cr, CRISPRintro, crisprpop, dCRBegin, dCRLength, dCRNumber,
#   dDE, dDL, df, dFracCatchable, dHBegin, dHLength, dHNumber, dInitialPop, dMA, dME, dMinPop, dMJ0,
#   dML, dMortSlope, dnhe, dRf, dshortout, HMatrix, harv, Harvest, identity5, JAindices, Jcf, Jcm, MatrixModelCSim,
#   P, pv0, rwf, totalpop, zero5

# # Test
pR <- MatrixModelCSim(nhe=0.01)
plot(pR)

### Option Ranges for Various Constants ####
CRNumberOpts <- c(1000)#seq(1,5001,5000/100)
CRLengthOpts <- seq(1,601,600/100)
HNumberOpts <- c(dHNumber)#seq(0,5000,5000/50)
HLengthOpts <- c(dHLength)#c(12,60,120,600,1200)
MortSlopeOpts <- seq(0,5*10^(-6),(5*10^(-6))/100)

### Data Frame Initialization ####
numtrials = length(CRNumberOpts)*length(CRLengthOpts)*length(HNumberOpts)
datarecord <- data.frame(#CRNumber = 1:numtrials,
                 CRLength = 1:numtrials,
                 # HNumber = 1:numtrials,
                 # HLength = 1:numtrials,
                 MortSlope = 1:numtrials,
                 # MaxCPopTim = 1:numtrials,
                 EradTime = 1:numtrials)

### Model Running ####
trinum <- 1
for (crnum in CRNumberOpts)
{
  for (crlen in CRLengthOpts)
  {
    for (hnum in HNumberOpts)
    {
      for (hlen in HLengthOpts)
      {
        for (mslope in MortSlopeOpts)
        {
          finalnums <- MatrixModelCSim(
            CRNumber = crnum, CRLength = crlen,
            HNumber = hnum, HLength = hlen,
            MortSlope = mslope,
            shortout = TRUE)
          
          # datarecord[trinum,"CRNumber"] <- crnum
          datarecord[trinum,"CRLength"] <- crlen
          # datarecord[trinum,"HNumber"] <- hnum
          # datarecord[trinum,"HLength"] <- hlen
          datarecord[trinum,"MortSlope"] <- mslope
          # datarecord[trinum,"MaxCpopTime"] <- finalnums["MaxCpopTime"]/12
          datarecord[trinum,"EradTime"] <- finalnums["EradTime"]/12
          
          trinum <- trinum + 1
        }
      }
    }
  }
}

# for( i in 1:numtrials)
# {
#   yr <- datarecord[i,"CRLength"]
#   if (yr == 1)
#     datarecord[i,"CRLength"] <- "1 Month"
#   else if (yr == 12)
#     datarecord[i,"CRLength"] <- "1 Year"
#   else if (yr == 60)
#     datarecord[i,"CRLength"] <- "5 Years"
#   else if (yr == 120)
#     datarecord[i,"CRLength"] <- "10 Years"
#   else
#     datarecord[i,"CRLength"] <- "Blah"
# }

library("ggplot2")
foundation <- ggplot(datarecord, aes(CRLength, MortSlope))
foundation + geom_raster(aes(fill = EradTime)) +
  geom_contour(aes(z = EradTime), show.legend = FALSE)
