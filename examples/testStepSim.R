library(gadgetr)

# Change working directory to example haddock data
currDir <- getwd()
loadExample()

# Load parameters
gadget(c("-s","-i","refinputfile"))

setwd(currDir)

# Initialize simulation
initSim()

# Count steps
lengthSteps <- 0

# Loop for all steps (year + step)
while (TRUE){
        status <- stepSim()
        # Stop at the end of time
        if(status["finished"] == 1) break
        lengthSteps <- lengthSteps + 1
}

print(lengthSteps)

# Sim cleanup
finalizeSim()

# Gadget cleanup
out <- finalize()

