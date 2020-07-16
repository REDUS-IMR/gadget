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

# Count year
lengthYear <- 1

# Loop for all years
stats <- list()
while (TRUE){
	# Append stats
	stats[[lengthYear]] <- gadgetr:::runYear()
	# Stop at the end of time
	status <- gadgetr:::getEcosystemInfo()
        if(status[["time"]]["finished"] == 1) break
	lengthYear <- lengthYear + 1
}

print(lengthYear)

# Get termination information
term <- gadgetr:::getEcosystemInfo()

# Print end year
print(term$time["currentYear"])
print(initial$time["currentYear"] + lengthYear - 1)

# Print last step number
print(term$time["currentTime"])
print(lengthYear/4)

# Print out all status
names(stats[[1]])
names(stats[[1]]$stock)
names(stats[[1]]$fleet)

# Print haddock stock in year 1
names(stats[[1]]$stock$had)
stats[[1]]$stock$had$stk

# Print haddock recruitment in year 1
stats[[1]]$stock$had$rec

# Print haddock catch by commercial fleet in year 1
names(stats[[1]]$fleet)
names(stats[[1]]$fleet$comm$catch)
stats[[1]]$fleet$comm$catch$had

# Sim cleanup
gadgetr:::finalizeSim()

# Gadget cleanup
out <- gadgetr:::finalize()
