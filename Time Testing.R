
times <- NULL
trials <- seq(1,2000,100)

k <- length(times)+1
for (ntri in trials)
{
  stopwatch <- proc.time() #Start stop watch
  
  source("MatrixModelCSim.R")
  
  ## Option Ranges
  CRNumberOpts <- seq(0,5000,5000/100) #101
  CRLengthOpts <- c(1,12,24,36,48,60,72,84,96,108,120) #11
  HNumberOpts <- seq(0,5000,5000/100) #101
  HLengthOpts <- c(12,60,120,600,1200) #5
  MortSlopeOpts <- seq(0,5*10^(-6),(5*10^(-6))/100) #101
  
  datarecord <- data.frame(CRNumber = 1:ntri,
                           CRLength = 1:ntri,
                           HNumber = 1:ntri,
                           HLength = 1:ntri,
                           MortSlope = 1:ntri,
                           MaxPop = 1:ntri,
                           EradTime = 1:ntri)
  
  for (i in 1:ntri)
  {
    fnums <- MatrixModelCSim()
    
    datarecord[i,"CRNumber"] <- dCRNumber
    datarecord[i,"CRLength"] <- dCRLength
    datarecord[i,"HNumber"] <- dHNumber
    datarecord[i,"HLength"] <- dHLength
    datarecord[i, "MortSlope"] <- dMortSlope
    datarecord[i, "MaxPop"] <- fnums[2]
    datarecord[i, "EradTime"] <- fnums[1]/12
  }
  
  times[k] <- (proc.time()-stopwatch)[3]
  k <- k+1
}

beep()

fit <- lm(formula = times ~ trials)
m <- fit$coefficients[2]
b <- fit$coefficients[1]

curve(m*x+b, from = 0, to = 4000)
points(trials,times)
