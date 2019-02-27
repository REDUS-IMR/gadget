library(gadgetr)

# Change working directory to example haddock data
currDir <- getwd()
loadExample()

# Load parameters
gadget(c("-s","-i","refinputfile"))

setwd(currDir)

# Initialize simulation
initSim()

# Count year
lengthYear <- 0

# Loop for all years
while (TRUE){
	status<- yearSim()
	# Stop at the end of time
        if(status["finished"] == 1) break
	lengthYear <- lengthYear + 1
}

print(lengthYear)

# Sim cleanup
finalizeSim()

# Gadget cleanup
out <- finalize()

