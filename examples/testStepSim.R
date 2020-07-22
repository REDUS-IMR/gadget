library(gadgetr)

# Init the example haddock data
exPath <- gadgetr::loadExample()
gadgetr::initGadget(exPath, "refinputfile")

# Get initial information
initial <- gadgetr::getEcosystemInfo()

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
	stats[[lengthStep]] <- gadgetr::runStep()
	# Stop at the end of time
	status <- gadgetr::getEcosystemInfo()
        if(status[["time"]]["finished"] == 1) break
	lengthStep <- lengthStep + 1
}

print(lengthStep)

# Get termination information
term <- gadgetr::getEcosystemInfo()

# Print end year
print(term$time["currentYear"])
print(initial$time["currentYear"] + (lengthStep / 4) - 1)

# Print last step number
print(term$time["currentTime"])
print(lengthStep)

# Print out all status
print(names(stats[[1]]))
print(names(stats[[1]]$stock))
print(names(stats[[1]]$fleet))

# Print haddock stock in step 1
print(names(stats[[1]]$stock$had))
print(stats[[1]]$stock$had$stk)

# Print haddock recruitment in step 1
print(stats[[1]]$stock$had$rec)

# Print haddock catch by commercial fleet in step 1
print(names(stats[[1]]$fleet))
print(names(stats[[1]]$fleet$comm$catch))
print(stats[[1]]$fleet$comm$catch$had)

# Sim cleanup
gadgetr::endGadget()
