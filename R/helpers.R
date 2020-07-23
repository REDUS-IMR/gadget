#' @title Initialize a gadget simulation
#' @description This function initialize a gadget simulation given a valid model directory and a input file.
#' The simulation will not be run yet, please see the \code{\link{runStep}} and \code{\link{runYear}} functions.  
#' @param modelDir Path to the gadget model
#' @param inputfile The name of the simulation input (or main) file
#' @rdname initGadget
#' @export
initGadget <- function(modelDir, inputfile) {

	if(isGadgetInitialized()) {
		print("A previous instance of gadgetr run is active in memory. Please stop it first.")
		return(NULL)
	}

	if(!dir.exists(modelDir)) {
		print("Model directory not found!")
		return(NULL)
	}

	inputFull <- paste0(modelDir, "/", inputfile)

	if(!file.exists(inputFull)) {
		print("Input file not found!")
		return(NULL)
	}

	currWD <- getwd()

	setwd(modelDir)
	gadget(c("-s", "-i", inputfile))
	initSim()

	setwd(currWD)
	print(paste0("Gadget is started with the given model in ", modelDir, "."))
}

#' @title Stop the active gadget simulation run
#' @description This function stops a currently active gagdet simulation and release all the allocated memory.
#' @rdname endGadget
#' @export
endGadget <- function() {
	if(!isGadgetInitialized()) {
		print("No instance of gadget simulation is active in memory. Exiting.")
		return(NULL)
	}
	finalizeSim()
	finalize()
}

#' @title Get the predatory (eating/catch) statistics of a stock/fleet subject 
#' @description This function produces similar output as the standalone Gadget's printPredatorPrey type output
#' @param x Stock or Fleet name
#' @param type Must be either "stock" or "fleet"
#' @return A matrix of predator-prey statistics of an subject inside the list(catch = ...) object
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  exPath <- loadExample()
#'  initGadget(exPath, "refinputfile")
#'  stat <- getPredation("comm", "fleet")
#'  print(stat$catch$had)
#'  endGadget()
#'  }
#' }
#' @rdname getPredation
#' @export 
getPredation <- function(x, type) {
        ecoSystem <- getEcosystemInfo()
        stocks <- ecoSystem[["stock"]]

        getConsumption <- function(prey, fleetNo) {
                stockNo <- getStockNo(prey)
                tryCatch({
                        printPredatorPrey(fleetNo, stockNo, type)
                }, error=function(e){})
        }

	if(type == "fleet") {
                fleetNo <- getFleetNo(x)
	}else if(type == "stock") {
		fleetNo <- getStockNo(x)
	}else{
		print("Type is wrong")
		return(NULL)
	}

        ret <- list()
        catches <- lapply(stocks, getConsumption, fleetNo)
        names(catches) <- stocks
        ret[["catch"]] <- catches
        return(ret)
}

# Helper function for runYear/runStep functions. Will get the catch stats, categorized by stock names.
processFleetStats <- function(fleets) {
	stats <- lapply(fleets, getPredation, "fleet")
	names(stats) <- fleets
	return(stats)
}

# Helper function for runYear/runStep functions.
# If pre == TRUE will get the stock number only. Otherwise will get the ssb, recruitment and predatory stats.
processStockStats <- function(stocks, pre = TRUE) {
	# For pre (before stepping)
	getStatsPre <- function(x) {
		stockNo <- getStockNo(x)
		ret <- list()
		ret[["stk"]] <- printStock(stockNo)
		return(ret)
	}

	# For post (after stepping)
	getStatsPost <- function(x) {
		stockNo <- getStockNo(x)
		ret <- list()
		ret[["ssb"]] <- printSSB(stockNo)
		ret[["rec"]] <- printRecruitment(stockNo)
		ret[["eat"]] <- getPredation(x, "stock")[["catch"]]
		return(ret)
	}

	if(pre == TRUE)
		stats <- lapply(stocks, getStatsPre)
	else
		stats <- lapply(stocks, getStatsPost)

	names(stats) <- stocks
	return(stats)
}


#' @title Forward simulation for a year and collect the stats for all the stocks and fleets for that year
#' @description Forward simulation for a year and collect the stats. For the stocks, the output consists of
#' stock number (stk), spawning stock biomass (ssb), recruitment number (rec), and predatory information
#' which are categorized by preys (stock names). Please note that both the stock number before and after the steps are
#' available. For the fleets, the output consists of only the predatory information which are categorized by
#' preys (stock names)
#' @return list of fleets and stocks statistics (in matrix format) for the year run
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  exPath <- loadExample()
#'  initGadget(exPath, "refinputfile")
#'  stat <- runYear()
#'  print(stat)
#'  endGadget()
#'  }
#' }
#' @rdname runYear
#' @export 
runYear <- function() {

	# Placeholders
	stats <- list()
	tmp <- list()

	# Recursive list combining
	combineStats <- function(x, path) {
		path <- c(path, names(x))
		y <- x[[1]]
		if (is(y, "list")) {
			for(i in names(y))
				combineStats(y[i], path)
		} else {
			# Embed the matrix
			# Must use eval and parse
			eval(parse(text=paste0("xtemp <- tmp", paste0("[[\"", path, "\"]]", collapse=""))))
			eval(parse(text=paste0("ztemp <- stats", paste0("[[\"", path, "\"]]", collapse=""))))

			#print(paste(paste0("xtemp <- tmp", paste0("[[\"", path, "\"]]", collapse="")), "X rows", nrow(xtemp)))
			#print(paste(paste0("ztemp <- stats", paste0("[[\"", path, "\"]]", collapse="")), "Z rows", nrow(ztemp)))

			ztemp <- rbind(ztemp, xtemp)
			#print(paste("comb rows ", nrow(ztemp)))
			eval(parse(text=paste0("stats", paste0("[[\"", path, "\"]]", collapse="")," <<- ztemp")))
			#print(paste("aggregated stats ", nrow(eval(parse(text=paste0("stats", paste0("[[\"", path, "\"]]", collapse="")))))))
		}
	}

	ecoSystem <- getEcosystemInfo()
	currentYear <- ecoSystem[["time"]][["currentYear"]]

	while(TRUE) {
		# Get stats from all stocks (before stepping)
		stockStatPre <- processStockStats(ecoSystem[["stock"]], pre=TRUE)

		# Run forward a single step
		status <- stepSim()

		# Get stats from all stocks (after stepping)
		stockStatPost <- processStockStats(ecoSystem[["stock"]], pre=FALSE)

		# Combine both pre and post
		stockStat <- lapply(ecoSystem[["stock"]], function(x) return(c(stockStatPre[[x]], stockStatPost[[x]])))
		names(stockStat) <- ecoSystem[["stock"]]

		# Get stats from all fleets
		fleetStat <- processFleetStats(ecoSystem[["fleet"]])

		# Make temp list
		tmp <- list(fleets=fleetStat, stocks=stockStat)

		if(length(stats)==0) stats <- tmp
		else {
			combineStats(tmp["fleets"], c())
			combineStats(tmp["stocks"], c())
		}
		# stop condition
		if(status[["currentYear"]] > currentYear || status[["finished"]] == 1) break
	}
	return(stats)
}


#' @title Forward simulation for a step and collect stats for all the stocks and fleets
#' @description Forward simulation for a year and collect the stats. For the stocks, the output consists of
#' stock number (stk), spawning stock biomass (ssb), recruitment number (rec), and predatory information
#' which are categorized by preys (stock names). Please note that the stock number can be collected in either
#' before or after the step by using the stockAfterStep parameter. For the fleets, the output consists of
#' only the predatory information which are categorized by preys (stock names).
#' @param stockAfterStep Tells the function to collect stock information after the step if TRUE, Default: FALSE
#' @return list of fleets and stocks statistics (in matrix format) for the current step run
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  exPath <- loadExample()
#'  initGadget(exPath, "refinputfile")
#'  stat <- runStep(stockAfterStep = TRUE)
#'  print(stat)
#'  stat <- runStep(stockAfterStep = FALSE)
#'  print(stat)
#'  endGadget()
#'  }
#' }
#' @rdname runStep
#' @export 
runStep <- function(stockAfterStep = FALSE) {

	# Placeholders
	stats <- list()
	tmp <- list()

	# Recursive list combining
	combineStats <- function(x, path) {
		path <- c(path, names(x))
		y <- x[[1]]
		if (is(y, "list")) {
			for(i in names(y))
				combineStats(y[i], path)
		} else {
			# Embed the matrix
			# Must use eval and parse
			eval(parse(text=paste0("xtemp <- tmp", paste0("[[\"", path, "\"]]", collapse=""))))
			eval(parse(text=paste0("ztemp <- stats", paste0("[[\"", path, "\"]]", collapse=""))))

			#print(paste(paste0("xtemp <- tmp", paste0("[[\"", path, "\"]]", collapse="")), "X rows", nrow(xtemp)))
			#print(paste(paste0("ztemp <- stats", paste0("[[\"", path, "\"]]", collapse="")), "Z rows", nrow(ztemp)))

			ztemp <- rbind(ztemp, xtemp)
			#print(paste("comb rows ", nrow(ztemp)))
			eval(parse(text=paste0("stats", paste0("[[\"", path, "\"]]", collapse="")," <<- ztemp")))
			#print(paste("aggregated stats ", nrow(eval(parse(text=paste0("stats", paste0("[[\"", path, "\"]]", collapse="")))))))
		}
	}

	ecoSystem <- getEcosystemInfo()
	currentYear <- ecoSystem[["time"]][["currentYear"]]
	currentYear <- ecoSystem[["time"]][["currentStep"]]

	if(stockAfterStep) {
		# Run forward a single step
		status <- stepSim()

		# Get stats from all stocks (after stepping)
		stockStatPre <- processStockStats(ecoSystem[["stock"]], pre=TRUE)
	} else {
		# Get stats from all stocks (before stepping)
		stockStatPre <- processStockStats(ecoSystem[["stock"]], pre=TRUE)

		# Run forward a single step
		status <- stepSim()
	}

	# Get stats from all stocks (after stepping)
	stockStatPost <- processStockStats(ecoSystem[["stock"]], pre=FALSE)

	# Combine both pre and post
	stockStat <- lapply(ecoSystem[["stock"]], function(x) return(c(stockStatPre[[x]], stockStatPost[[x]])))
	names(stockStat) <- ecoSystem[["stock"]]

	# Get stats from all fleets
	fleetStat <- processFleetStats(ecoSystem[["fleet"]])

	# Make temp list
	tmp <- list(fleets=fleetStat, stocks=stockStat)

	if(length(stats)==0) stats <- tmp
	else {
		combineStats(tmp["fleets"], c())
		combineStats(tmp["stocks"], c())
	}

	return(stats)
}

#' @title Get the internal fleet identifier
#' @description Converts a fleet name into the interanl fleet identifier in a running simulation
#' @param fleetName String name of the fleet
#' @return an internal fleet number
#' @rdname getFleetNo
#' @export
getFleetNo <- function(fleetName){
	match(fleetName, getEcosystemInfo()$fleet)
}

#' @title Get the internal stock identifier
#' @description Converts a stock name into an internal stock identifier in a running simulation
#' @param stockName String name of the stock
#' @return an internal stock identifier
#' @rdname getStockNo
#' @export
getStockNo <- function(stockName){
        match(stockName, getEcosystemInfo()$stock)
}

#' @title Get a stock's length group index
#' @description This function converts a length measurement into a stock's internal length group index
#' @param stockName a string of the stock name
#' @param len the source length
#' @return a length group index identifier
#' @rdname getLenGrpIdx
#' @export 
getLenGrpIdx <- function(stockName, len){
	# Convert name into number
	stockNo <- getStockNo(stockName)

	if(is.na(stockNo)){
		print("Can't find stock")
		return(1)
	}

	stockInfoLen <- getStockInfoC(stockNo)$lengthGroup
	sSize <- nrow(stockInfoLen)

	if(len < stockInfoLen[1, "minLength"])
		return(-1)

	if(len > stockInfoLen[sSize, "maxLength"])
                return(sSize)

	idx <- which(stockInfoLen$minLength <= len & stockInfoLen$maxLength > len)
	return(idx)
}

#' @title Update a stock's renewal parameters
#' @description This function updates a stock renewal ("recruitment") parameters, but only if
#' the stock is set to renew (i.e., doesrenew is set to 1 in the model). The function accepts a similar
#' input format as the original gadget renewal file. Note that only 2 modes of renewal (Normal 
#' Parametric and Numerical Distribution) that are supported for now.
#' @param stockName a string of the stock name
#' @param year the target year of renewal
#' @param step the target step of renewal
#' @param area the target area of renewal
#' @param age the age target of the stock
#' @param number the number parameter value
#' @param mean the mean parameter value (Normal Parametric Distribution only), Default: NA
#' @param sdev the sdev parameter value (Normal Parametric Distribution only), Default: NA
#' @param alpha the alpha parameter value (Normal Parametric Distribution only), Default: NA
#' @param beta the beta parameter value (Normal Parametric Distribution only), Default: NA
#' @param length the mean parameter value (Numerical Distribution only), Default: NA
#' @param meanWeight the weight parameter value (Numerical Distribution only), Default: NA
#' @return a vector of numeric error codes (0 is success, otherwise is error)
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  exPath <- loadExample()
#'  initGadget(exPath, "refinputfile")
#'  updateRenewal("had", 1978, 1, 1, 1, 123456, mean=16.41, sdev=2.25, alpha=8.85e-6, beta=3.0257)
#'  stat <- runStep(stockAfterStep = TRUE)
#'  print(sum(stat$stocks$had$rec$renew[, "renewalNumber"]))
#'  endGadget()
#'  }
#' }
#' @rdname updateRenewal
#' @export 
updateRenewal <- function(stockName, year, step, area, age, number, mean=NA,
                sdev=NA, alpha=NA, beta=NA,
                length=NA, meanWeight=NA){

	# Convert name into number
	stockNo <- getStockNo(stockName)

	if(is.na(stockNo)){
		print("Can't find stock")
		return(1)
	}

	return (updateRenewalC(stockNo, year, step, area, age, number, mean,
                sdev, alpha, beta,
                length, meanWeight))

}

#' @title Update a stock's spawning parameters
#' @description This function updates a stock spawning recruitment parameters, but only if
#' the stock is set to spawn (i.e., doesspawn is set to 1 in the model). All spawning recruitment
#' functions are supported. However, it is the user responsibility to give the correct
#' parameters that correspond to the stock's spawning recruitment functions. See the
#' gadget user guide for the detailed information about the different functions and their parameters 
#' @param stockName a string of the stock name
#' @param ... the collection of parameters (e.g., if the recruitment function is "fecundity",
#' a vector of 5 parameters are required)
#' @return a vector of numeric error codes (0 is success, otherwise is error)
#' @rdname updateSpawning
#' @export 
updateSpawningRec <- function(stockName, ...){

        # Convert name into number
        stockNo <- getStockNo(stockName)

        # Process params
        params <- c(...)

        if(is.na(stockNo)){
                print("Can't find stock")
                return(1)
        }

        return (updateSpawningRecC(stockNo, params))
}

#' @title Update a fleet suitability parameters for a certain stock 
#' @description This function updates a fleet suitability parameters for a given stock.
#' Please note that this function will only update the gadget internal pre-computed
#' suitability values and not the user defined suitability function and its parameters
#' found in the model file
#' @param fleetName a string of the fleet name
#' @param stockName a string of the stock name
#' @param len the stock length target 
#' @param value the target suitability value
#' @return a vector of numeric error codes (0 is success, otherwise is error)
#' @rdname updateSuitability
#' @export 
updateSuitability <- function(fleetName, stockName, len, value){

	# Convert stock name into number
        stockNo <- getStockNo(stockName)

        if(is.na(stockNo)){
                print("Can't find stock!")
                return(1)
        }

	# Convert fleet name into number
        fleetNo <- getFleetNo(fleetName)

        if(is.na(fleetNo)){
                print("Can't find fleet!")
                return(1)
        }

	return(updateSuitabilityC(fleetNo, stockNo, len, value))
}

#' @title Update a total fleet catch amount 
#' @description This function updates the catch amount of a totalfleet object.
#' @param fleetName a string of the fleet name
#' @param year a target year
#' @param step a target step
#' @param area a target area
#' @param value the target amount
#' @return a vector of numeric error codes (0 is success, otherwise is error)
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  exPath <- loadExample()
#'  initGadget(exPath, "refinputfile")
#'  updateAmount("future", 1978, 1, 1, 123456)
#'  updateAmount("future", 1978, 3, 1, 567890)
#'  stat <- runStep()
#'  print(sum(stat$fleets$future$catch$had[, "biomassConsumed"]))
#'  print(aggregate(biomassConsumed ~ year + step, data = stat$fleets$future$catch$had, FUN=sum))
#'  endGadget()
#'  }
#' }
#' @rdname updateAmount
#' @export 
updateAmount <- function(fleetName, year, step, area, value){

	# Convert fleet name into number
        fleetNo <- getFleetNo(fleetName)

        if(is.na(fleetNo)){
                print("Can't find fleet")
                return(1)
        }

        return(updateAmountYear(fleetNo, year, step, area, value))


}

#' @title Instruct a stock to stop all of its predatory activities
#' @description This function instructs a stock to stop all of its predatory activities.
#' This is equivalent with setting a stock's "doeseat" parameter to 0
#' @param stockName  a string of the stock name
#' @return a vector of numeric error codes (0 is success, otherwise is error)
#' @rdname stopEating
#' @export 
stopEating <- function (stockName){
	# Convert stock name into number
        stockNo <- getStockNo(stockName)

        if(is.na(stockNo)){
                print("Can't find stock!")
                return(1)
        }
	return(setEating(stockNo, 0))
}
