library(gadgetr)

# Change working directory to example haddock data
currDir <- getwd()
gadgetr:::loadExample()

# Load parameters
gadgetr:::gadget(c("-s","-i","refinputfile"))

setwd(currDir)

# Initialize simulation
gadgetr:::initSim()

# Get initial information
initial <- gadgetr:::getEcosystemInfo()

# Print start year
print(initial$time["currentYear"])
# Print initial step number
print(initial$time["currentTime"])
# Print final step number
print(initial$time["totalSteps"])

# Count step
lengthStep <- 1

# Loop for all steps
stats <- list()
while (TRUE){
	# Append stats
	stats[[lengthStep]] <- gadgetr:::runStep()
	# Stop at the end of time
	status <- gadgetr:::getEcosystemInfo()
        if(status[["time"]]["finished"] == 1) break
	lengthStep <- lengthStep + 1
}

print(lengthStep)

# Get termination information
term <- gadgetr:::getEcosystemInfo()

# Print end year
print(term$time["currentYear"])
print(initial$time["currentYear"] + (lengthStep / 4) - 1)

# Print last step number
print(term$time["currentTime"])
print(lengthStep)

# Print out all status
names(stats[[1]])
names(stats[[1]]$stock)
names(stats[[1]]$fleet)

# Print haddock stock in step 1
names(stats[[1]]$stock$had)
stats[[1]]$stock$had$stk

# Print haddock recruitment in step 1
stats[[1]]$stock$had$rec

# Print haddock catch by commercial fleet in step 1
names(stats[[1]]$fleet)
names(stats[[1]]$fleet$comm$catch)
stats[[1]]$fleet$comm$catch$had

# Sim cleanup
gadgetr:::finalizeSim()

# Gadget cleanup
out <- gadgetr:::finalize()
