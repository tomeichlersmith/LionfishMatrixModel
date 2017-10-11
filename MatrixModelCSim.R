
### Arg -> Comments on Argument (default value) ##################################################################################
#Human Controlled Constants
#   CRNumber -> Number of CRISPR lionfish to release each month (assumed all Juveniles) (100)
#   CRBegin -> Time step to begin CRISPR Release procedure (12)
#   CRLength -> Number of time steps to perform CRISPR Release procedure (12)
#   HNumber -> Number of lionfish to harvest each month (0)
#   HBegin -> Time step to begin Harvesting procedure (12)
#   HLength -> Number to time steps to perform Harvesting procedure (1200)
#Nature Controlled Constants
#   nhe -> probability that a lionfish unsuccessfully passes on the CRISPR gene (0.01)
#   MortSlope -> slope of linear relationship between juvenile mortality and total population (1.72499*10^(-6))
#   FracCatchable -> Fraction of the total population that is catchable in one month (0.05)
#   MinPop -> Minimum size of total population before population is considered eradicated (2)
#   MaxSteps -> Maximum number to time steps to run simulation if population isn't eradicated (1200+CRBegin)
#   InitialPop -> Initial population of wild adults at time step 0 (2500)
#   Rf -> Proportion of population that is female (0.46)
#   MJ0 -> juvenile mortality without dependence of total population (0.165)
#   ML -> larvae mortality (0.35)
#   MA -> adult mortality (0.052)
#   DL -> larval duration (30)
#   ME -> egg mortality (0.31)
#   DE -> egg duration (3)
#   f -> fecundity (194577)
#Other
#   cRecordOut -> boolean value to decide which record to return - ignored if shortout = TRUE (FALSE)
#     FALSE ==> returns total pop record
#     TRUE ==> returns crispr pop record and total pop record
#         {Tpop = total pop record, Cpop = crispr pop record}
#   shortout -> boolean value to decide what to return (FALSE)
#     FALSE ==> returns entire population record from the simulation
#     TRUE ==> returns list of the form
#         {EradTime = Time Steps from CRBegin to Eradication, MaxCpop = Time Steps from CRBegin to Max CRISPR Population}

#Default Argument Values
dCRNumber <- 100
dCRBegin <- 12
dCRLength <- 12
dHNumber <- 0
dHBegin <- 12
dHLength <- 1200
dnhe <- 0.01
dMortSlope <- 1.72499*10^(-6)
dFracCatchable <- 0.05
dMinPop <- 2
dRf <- 0.46
dInitialPop <- 2500 #Per Hectare -  estimate from coastalscience.noaa.gov
dMJ0 <- 0.165
dML <- 0.35
dMA <- 0.052
dDL <- 30
dME <- 0.31
dDE <- 3
df <- 194577
dshortout <- FALSE
dcRecordOut <- FALSE

#Helpful Indices of population vector
Jcf <- 6
Jcm <- 7
Acm <- 12
Acf <- 11
Awf <- 13
Aif <- 15
JAindices <- list(Jcf,Jcm,8,9,10,Acf,Acm,Awf,14,Aif)

### Utility Functions #########################################################################

#Initial Population Vector build from Initial Population Estimate
pv0 <- function(InitialPop, Rf)
{
  #assume that the initial population only consists of wild types
  #Values for pv0 retrieved from eigenvector with positive eigenvalue of Matrix from origin paper
  Lwf0 <- Rf*31391.6*InitialPop
  Lwm0 <- (1-Rf)*31391.6*InitialPop
  Jwf0 <- Rf*2.48583*InitialPop
  Jwm0 <- (1-Rf)*2.48583*InitialPop
  Awf0 <- Rf*InitialPop
  Awm0 <- (1-Rf)*InitialPop
  return (matrix(
    c(0,0,Lwf0,Lwm0,0,0,0,Jwf0,Jwm0,0,0,0,Awf0,Awm0,0),
    nrow = 15,
    ncol = 1,
    byrow = TRUE))
}

#CRISPR intro number depending on current time step
cr <- function(timestep, CRNumber, CRBegin, CRLength) {
  if (timestep < CRBegin || timestep > CRBegin+CRLength)
  {
    return (0)
  }
  else
  {
    return (CRNumber)
  }
}

#Harvesting number depending on current time step
harv <- function(timestep, tpop, FracCatchable, HNumber, HBegin, HLength) {
  if (timestep < HBegin || timestep > HBegin+HLength)
  {
    return (0)
  }
  else
  {
    return (min(c(FracCatchable*tpop,HNumber)))
  }
}

#Does not count Larval stage in population, assumes popvector is correctly formated
totalpop <- function(popvector) {
  sum <- 0
  for (i in JAindices)
  {
    sum <- sum + popvector[i]
  }
  
  return (sum)
}

#Does not count Larval stage in population, assumes popvector is correctly formated
crisprpop <- function(popvector) {
  return (
    popvector[Jcf]+popvector[Jcm]+popvector[Acf]+popvector[Acm]
  )
}

#Ratio of wild type adult females to total adult females, assumes popvector is correctly formated
rwf <- function(popvector) {
  totaladultfemales <- popvector[Acf]+popvector[Awf]+popvector[Aif]
  
  if (totaladultfemales == 0)
    return (0) #Force the production matrix (P) to produce the zero vector when multiplied by adult population vector
  else
    return (popvector[Awf]/totaladultfemales)
}

### Matrix Constructions #########################################################################

identity5 <- diag(5) #Five-dimensional identity matrix
zero5 <- (diag(5)-diag(5)) #Five-dimensional zero matrix

#Produces the matrix that "switches" a crispr/infertile type to a wild type given a probability that 
#the gene is unsucessfully passed
#If nhe=0, then this matrix is the identity matrix (assumes the gene is passed perfectly, so no switching)
SwitchMat <- function(nhe) {
  return (
    matrix(
      c(1-nhe,0,0,0,0,
        0,1-nhe,0,0,0,
        nhe,0,1,0,nhe,
        0,nhe,0,1,0,
        0,0,0,0,1-nhe),
      nrow = 5,
      ncol = 5,
      byrow = TRUE)
  )
}

#Produces the production matrix given the current popvector, uses function rwf
P <- function(popvector, Rf) {
  rwft <- rwf(popvector)
  return (
    matrix(
      c(0,rwft*Rf,0,0,0,
        (1-Rf),rwft*(1-Rf),0,0,0,
        0,0,0,rwft*Rf,0,
        0,0,0,rwft*(1-Rf),0,
        Rf,0,0,0,0),
      nrow = 5,
      ncol = 5,
      byrow = TRUE)
  )
}

#Produces the harvesting submatrix given current popvector and timestep, uses functions cr and totalpop
HMatrix <- function(popvector, timestep, HNumber, HBegin, HLength, FracCatchable) {
  tp <- totalpop(popvector)
  hnum <- harv(timestep, tp, FracCatchable, HNumber, HBegin, HLength)
  
  if (tp == 0)
    return (identity5)
  else
    return (identity5 - (hnum/tp)*identity5)
}

#Produces the birth/death matrix given current popvector, uses function P
BirthDeath <- function(popvector, mslope, Rf, nhe, ML, DL, MA, ME, DE, MJ0, f) {
  tp <- totalpop(popvector)
  r1 <- cbind(zero5, zero5, f*exp(-ME*DE)*(SwitchMat(nhe) %*% P(popvector, Rf)) )
  r2 <- cbind(exp(-ML*DL)*identity5, ((11/12)*exp(-(mslope*tp+MJ0)))*identity5, zero5)
  r3 <- cbind(zero5, ((1/12)*exp(-(mslope*tp+MJ0)))*identity5, exp(-MA)*identity5)
  return (rbind(r1,r2,r3))
}

#Produces the catch/release matrix given current popvector and time step, uses function C
Harvest <- function(popvector, timestep, HNumber, HBegin, HLength, FracCatchable) {
  HH <- HMatrix(popvector, timestep, HNumber, HBegin, HLength, FracCatchable)
  r1 <- cbind(identity5, zero5, zero5)
  r2 <- cbind(zero5, HH, zero5)
  r3 <- cbind(zero5, zero5, HH)
  return (rbind(r1,r2,r3))
}

#Produces CRISPR introduction vector
CRISPRintro <- function(timestep, Rf, CRNumber, CRBegin, CRLength) {
  cnum <- cr(timestep, CRNumber, CRBegin, CRLength)
  pvIn <- matrix(
    c(0,0,0,0,0,Rf*cnum,(1-Rf)*cnum,0,0,0,0,0,0,0,0),
    nrow = 15,
    ncol = 1,
    byrow = TRUE)
  return (pvIn)
}

### Model Definition #########################################################################

#Runs one simulation, returning a vector that has a record of the total population
#Uses BirthDeath, CatchRelease, totalpop
MatrixModelCSim <- function(
    CRNumber = dCRNumber, CRBegin = dCRBegin, CRLength = dCRLength,
    HNumber = dHNumber, HBegin = dHBegin, HLength = dHLength,
    nhe = dnhe, FracCatchable = dFracCatchable, MortSlope = dMortSlope,
    InitialPop = dInitialPop, MinPop = dMinPop,
    Rf = dRf, MJ0 = dMJ0, ML = dML, MA = dMA, DL = dDL, ME = dME, DE = dDE, f = df,
    shortout = dshortout, cRecordOut = dcRecordOut
  )
{
  tpopRecord <- NULL #sets tpopRecord
  cpopRecord <- NULL #sets cpopRecord
  pv <- pv0(InitialPop, Rf) #Population Vector initialized
  
  tpop <- totalpop(pv)
  
  t <- 1 #Starting at time step 1
  while (tpop > MinPop && t < 1200+CRBegin)
  {
    BD <- BirthDeath(pv, MortSlope, Rf, nhe, ML, DL, MA, ME, DE, MJ0, f) #Constructs BirthDeath Matrix from current pv
    HV <- Harvest(pv, t, HNumber, HBegin, HLength, FracCatchable) #Constructs Harvest Matrix from pv,
    # time step t as well as the Harvest constants
    cin <- CRISPRintro(t, Rf, CRNumber, CRBegin, CRLength)
    
    pv <- BD %*% HV %*% (pv+cin) #Calculates next population vector from constructed matrices
    
    tpop <- totalpop(pv)
    tpopRecord[t] <- tpop #Stores total population into a Record list
    cpopRecord[t] <- crisprpop(pv)
    
    t <- t+1 #Iterates to the next time step
  }
  
  if (shortout)
  {
    maxcpop <- which.max( cpopRecord ) - CRBegin
    eradtime <- t-CRBegin #Will be the time step the loop is exited minus CRBegin
    return ( c(EradTime = eradtime, MaxCpopTime = maxcpop) )
  }
  else
  {
    if (cRecordOut)
      return ( list(Tpop = tpopRecord, Cpop = cpopRecord) )
    else
      return (tpopRecord)
  }
}

# # Test
# library("ggplot2")
# tmp <- MatrixModelCSim(
#   cRecordOut = TRUE)
# pR <- tmp$Tpop
# cR <- tmp$Cpop
# tmpdf <- data.frame(Years = (1:length(pR)-dCRBegin)/12, TPop = pR, CPop = cR)
# ggplot() +
#   geom_point(data = tmpdf, aes(x = Years, y = TPop, color = "Total")) +
#   geom_point(data = tmpdf, aes(x = Years, y = CPop, color = "CRISPR")) +
#   xlab('Years') +
#   ylab('Population')
