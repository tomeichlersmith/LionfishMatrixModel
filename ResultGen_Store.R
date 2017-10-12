#This file is intended to iterate over the list of possible inputs (at least the interesting ones),
# generate the results from the model, and store them in a .csv file to easily obtain in the future.
#The largest difficulty in this case is the huge size of the input space. This vastness requires the storage
# to occur after each run of the model, which is much slower but safer in the event the machine running this
# code freezes due to running out of RAM.

#Include file that defines simulation function
source("MatrixModelCSim.R")

#Statements defining the lists of possible values for the inputs
# Any inputs not defined here will just be defined as their default values
crnumberOpts <- c(10,100,1000)
crlengthOpts <- c(1,6,12,18,24,36,48,60,120)
hnumberOpts <- c(10,100,1000)
hlengthOpts <- c(12,18,24,36,48,60,120,1200)
nheOpts <- c(0,0.001,0.005,0.01,0.05,0.1,0.15,0.2)
mortslopeOpts <- seq(0,2*dMortSlope,(2*dMortSlope)/10)

numbertrials <- length(crnumberOpts)*length(crlengthOpts)*length(hnumberOpts)*length(hlengthOpts)*length(nheOpts)*length(mortslopeOpts)

for (crnum in crnumberOpts) {
  for (crlen in crlengthOpts) {
    for (hnum in hnumberOpts) {
      for (hlen in hlengthOpts) {
        for (nhe in nheOpts) {
          for (mslope in mortslopeOpts) {
            
            #Run Simulation with current values for inputs
            results <- MatrixModelCSim(
              CRNumber = crnum,
              CRLength = crlen,
              HNumber = hnum,
              HLength = hlen,
              nhe = nhe,
              MortSlope = mslope,
              shortout = TRUE)
            
            #Output results to file
            
          }
        }
      }
    }
  }
}

