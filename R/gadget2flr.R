#' @title Update FLStock and FLIndex objects using a gadget simulation run output
#' @description This function translates a gadget output object for a given \code{gadgetYear} 
#' year and \code{stockTitle} stock and inserts it to the FLR objects (\code{fl_stock} and \code{fl_index}).
#' @param stockTitle a string of gadget stock name
#' @param out gadget simulation output object. Normally is the output of [runYear()]
#' @param gadgetYear gadget year target
#' @param fl_stock an existing FLStock object. Normally produced by [runUntil()]
#' @param fl_index an existing FLIndex object. Normally produced by [runUntil()]
#' @param globalparams a list object of parameters to aid gadget to FLR objects generation (See this page
#' \code{vignette("gadget-flr", package = "gadgetr")} for an example)
#' @importFrom dplyr anti_join
#' @importFrom FLCore window catch.n<- stock.n<- index<- catch<- catch.wt<- 
#' discards discards<- discards.n discards.n<- discards.wt discards.wt<-
#' landings landings<- landings.n landings.n<- landings.wt landings.wt<-
#' stock<- stock.wt<- m m<- harvest harvest<- mat mat<- range<-
#' @importFrom stats aggregate
#' @export
updateFLStock <- function(stockTitle, out, gadgetYear, fl_stock, fl_index, globalparams) {
	getStocks <- function(stockTitle, out, suffix="stocks") {
		xtemp <- list()
		xtemp$stk <- NULL
		xtemp$ssb <- NULL
		xtemp$rec <- NULL

		for(stockName in names(out[["stocks"]])){
			if(stockName %in% globalparams[[stockTitle]][[suffix]]){
				if(ncol(out[["stocks"]][[stockName]][["stk"]]) > 0)
					xtemp$stk <- rbind(xtemp$stk, out[["stocks"]][[stockName]][["stk"]])
				if(ncol(out[["stocks"]][[stockName]][["ssb"]]) > 0)				
					xtemp$ssb <- rbind(xtemp$ssb, out[["stocks"]][[stockName]][["ssb"]])
				if(ncol(out[["stocks"]][[stockName]][["rec"]][["spawn"]]) > 0)		
					xtemp$rec <- rbind(xtemp$rec, out[["stocks"]][[stockName]][["rec"]][["spawn"]])			
			}
		}
		return(xtemp)
	}

	getCatches <- function(stockTitle, out, type) {
		xtemp <- NULL

		for(fleetName in names(out[["fleets"]])) {
			catches <- out[["fleets"]][[fleetName]][["catch"]]
			#print(paste(fleetName, "->"))
			#print(fleetName %in% eval(parse(text=paste0(stockTitle, ".", type))))

			for(stockName in names(catches)){
				#print(stockName)
				#print(stockName %in% eval(parse(text=paste0(stockTitle, ".stocks"))))
				if(fleetName %in% globalparams[[stockTitle]][[type]] && stockName %in% globalparams[[stockTitle]][["stocks"]]){
					#print(paste(paste0(stockTitle, ".", type), "-", paste0(stockTitle, ".stocks")))
					if(ncol(catches[[stockName]]) > 0)					
						xtemp <- rbind(xtemp, catches[[stockName]])
				}
			}
		}
		return(xtemp)
	}

	getEaten <- function(stockTitle, out) {
		xtemp <- NULL

		for(stockNm in names(out[["stocks"]])) {
			catches <- out[["stocks"]][[stockNm]][["eat"]]
			for(stockName in names(catches)){
				if(stockName %in% globalparams[[stockTitle]][["stocks"]]){
					if(ncol(catches[[stockName]]) > 0)
						xtemp <- rbind(xtemp, catches[[stockName]])
				}
			}
		}
		return(xtemp)
	}

	print(gadgetYear)

	## Get params
	stockParams <- globalparams[[stockTitle]][["params"]]

	# Step of which stock number will be collected
	stockStep <- stockParams$stockStep

	# Get iteration (or ID)
	#iter <-	match(stockTitle, stockList)
	iter <- 1	
	print(paste("Iter:", iter))
	
	# Getting survey and catch
	survey.data <- getCatches(stockTitle, out, "surveys")
	catch.data <- getCatches(stockTitle, out, "fleets")
	eaten.data <- getEaten(stockTitle, out)

	# Delete Age zero
	survey.data <- survey.data[!survey.data[,"age"]==0,]
	catch.data <- catch.data[!catch.data[,"age"]==0,]
	eaten.data <- eaten.data[!eaten.data[,"age"]==0,]

	# Process catch
	catch <- aggregate(biomassConsumed ~ year + area, data=catch.data, FUN=sum)
	catch.n <- aggregate(numberConsumed ~ year + area + age, data=catch.data, FUN=sum)
	catch.biomass <- aggregate(biomassConsumed ~ year + area + age, data=catch.data, FUN=sum) 
	catch.wt <- catch.biomass
	catch.wt[,"biomassConsumed"] <- catch.wt[,"biomassConsumed"]/catch.n[,"numberConsumed"]
	#catch.wt[is.na(catch.wt[,"biomassConsumed"]),"biomassConsumed"]  <- max(catch.wt[,"biomassConsumed"], na.rm = T)

	# Process survey
	index <- aggregate(biomassConsumed ~ year + area, data=survey.data, FUN=sum)
	index.n <- aggregate(numberConsumed ~ year + area + age, data=survey.data, FUN=sum)
	index.biomass <- aggregate(biomassConsumed ~ year + area + age, data=survey.data, FUN=sum)
	index.wt <- index.biomass
	index.wt[,"biomassConsumed"] <- index.wt[,"biomassConsumed"]/index.n[,"numberConsumed"]
	#index.wt[is.na(index.wt[,"biomassConsumed"]),"biomassConsumed"] <- max(index.wt[,"biomassConsumed"], na.rm = T)

	# Process Eaten
	if(length(eaten.data) > 0) {
		eaten <- aggregate(biomassConsumed ~ year + area, data=eaten.data, FUN=sum)
		eaten.n <- aggregate(numberConsumed ~ year + area + age, data=eaten.data, FUN=sum)
		eaten.biomass <- aggregate(biomassConsumed ~ year + area + age, data=eaten.data, FUN=sum)
		eaten.wt <- eaten.biomass
		eaten.wt[,"biomassConsumed"] <- eaten.wt[,"biomassConsumed"]/eaten.n[,"numberConsumed"]
	} else {
		eaten.n <- NA
	}

	# Getting stock information
	stock.data <- getStocks(stockTitle, out)

	# Delete Age zero
	stock.data$stk <- stock.data$stk[!stock.data$stk[,"age"]==0,]

	# Only take the specified timestep
	stock.data$stk <- stock.data$stk[stock.data$stk[,"step"]==stockStep,]

	# Process stock information
	stock <- aggregate(number * meanWeights ~ year + area, data=stock.data$stk, FUN=sum)
	stock.n <- aggregate(number ~ year + area + age, data=stock.data$stk, FUN=sum)
	stock.biomass <- aggregate(number * meanWeights ~ year + area + age, data=stock.data$stk, FUN=sum)
	stock.wt <- stock.biomass
	stock.wt[, ncol(stock.wt)] <- stock.wt[, ncol(stock.wt)]/stock.n[, "number"]
	#stock.wt[is.na(stock.wt[, ncol(stock.wt)]), ncol(stock.wt)]  <- max(stock.wt[, ncol(stock.wt)], na.rm = T)

	# Process SSB information
	if(!is.null(stock.data$ssb))
		ssb <- aggregate(SSB ~ year + area, data=stock.data$ssb, FUN=sum)

	# Process Recruitment information
	if(!is.null(stock.data$rec))
		rec <- aggregate(Rec ~ year + area, data=stock.data$rec, FUN=sum)

	# Process maturity matrix
	## Getting mature stock information
	stock.mature.data <- getStocks(stockTitle, out, suffix = "stocks.mature")
	## Delete Age zero
	stock.mature.data$stk <- stock.mature.data$stk[!stock.mature.data$stk[,"age"]==0,]
	## Only take the specified timestep
	stock.mature.data$stk <- stock.mature.data$stk[stock.mature.data$stk[,"step"]==stockStep,]
	## Calculate mature stocks number
	stock.mature.n <- aggregate(number ~ year + area + age, data=stock.mature.data$stk, FUN=sum)
	mature <- stock.n

	# Check whether we have the same object, if not do some manipulation first
	if(nrow(stock.mature.n) != nrow(stock.n)) {
		tmp_mat <- anti_join(stock.n, stock.mature.n)
		tmp_mat$number <- 0
		tmp_mat <- rbind(tmp_mat, stock.mature.n)
		stock.mature.n <- tmp_mat[order(tmp_mat$year, tmp_mat$area, tmp_mat$age),]
	}

	mature[,ncol(mature)] <- stock.mature.n[,ncol(stock.mature.n)] / stock.n[,ncol(stock.n)]

	# Process the fishing mortality matrix (F)
	mortF <- stock.n
	mortF[,ncol(mortF)] <- -log((stock.n[,ncol(stock.n)] - catch.n[,ncol(catch.n)])/stock.n[,ncol(stock.n)])

	# Process the predation mortality matrix (M2)
	mortPred <- stock.n
	if(!is.data.frame(eaten.n))
		mortPred[,ncol(mortPred)] <- 0
	else
		mortPred[,ncol(mortPred)] <- -log((stock.n[,ncol(stock.n)] - eaten.n[,ncol(eaten.n)])/stock.n[,ncol(stock.n)])

	print("Putting into FLStock")

	areas <- seq_len(length(dimnames(fl_stock)$area))

	for( area in areas ) {

		print(paste("Area: ", area))

		# Put everything into FLStock	
		# Catch

		print("Catches")

		fl_stock@catch[,gadgetYear,,,area,iter] <- catch[catch[["area"]]==area, ncol(catch)]
		fl_stock@catch.n[catch.n[,"age"],gadgetYear,,,area,iter] <- catch.n[catch.n[["area"]]==area, ncol(catch.n)]
		fl_stock@catch.wt[catch.wt[,"age"],gadgetYear,,,area,iter] <- catch.wt[catch.wt[["area"]]==area, ncol(catch.wt)]

		print("Landings")

		# Landings
		fl_stock@landings[,gadgetYear,,,area,iter] <- catch[catch[["area"]]==area, ncol(catch)]
		fl_stock@landings.n[catch.n[,"age"],gadgetYear,,,area,iter] <- catch.n[catch.n[["area"]]==area, ncol(catch.n)]
		fl_stock@landings.wt[catch.wt[,"age"],gadgetYear,,,area,iter] <- catch.wt[catch.wt[["area"]]==area, ncol(catch.wt)]

		print("Discards")

		# Discards (use catch weight as discard weights)
		fl_stock@discards[,gadgetYear,,,area,iter] <- 0
		fl_stock@discards.n[,gadgetYear,,,area,iter] <- 0
		fl_stock@discards.wt[,gadgetYear,,,area,iter] <- catch.wt[catch.wt[["area"]]==area, ncol(catch.wt)]

		print("Stocks")

		# Stocks
		fl_stock@stock[,gadgetYear,,,area,iter] <- stock[stock[["area"]]==area, ncol(stock)]
		fl_stock@stock.n[stock.n[,"age"], gadgetYear,,,area,iter] <- stock.n[stock.n[["area"]]==area, ncol(stock.n)]
		fl_stock@stock.wt[stock.n[,"age"], gadgetYear,,,area,iter] <- stock.wt[stock.wt[["area"]]==area, ncol(stock.wt)]
		
		# SSB
		#fl_stock@stock <- FLQuant(stock[,ncol(stock)], dimnames=list(age="all", year=gadgetYear))

		# Recruitment
		#fl_stock@stock <- FLQuant(stock[,ncol(stock)], dimnames=list(age="all", year=gadgetYear))

		# Maturity
		fl_stock@mat[mature[,"age"], gadgetYear,,,area,iter] <- mature[mature[["area"]]==area, ncol(mature)]

		# Mortality (in harvest with unit f)
		mortF[is.nan(mortF[,ncol(mortF)]), ncol(mortF)] <- 0
		fl_stock@harvest[mortF[,"age"], gadgetYear,,,area,iter] <- mortF[mortF[["area"]]==area, ncol(mortF)]

		#print(harvest(fl_stock))

		# Natural mortality (m)
		mortPred[is.nan(mortPred[,ncol(mortPred)]), ncol(mortPred)] <- 0

		# Always store m in the auxiliary m2 slot
		m2tmp <- fl_stock@m2
		m2tmp[stock.n[,"age"], gadgetYear,,,area,iter] <- mortPred[mortPred[["area"]]==area, ncol(mortPred)]
		attr(fl_stock, "m2") <- m2tmp

		if(is.null(stockParams[["m2"]]))
			fl_stock@m[stock.n[,"age"], gadgetYear,,,area,iter] <- stockParams[["m1"]] + mortPred[mortPred[["area"]]==area, ncol(mortPred)]
		else
			fl_stock@m[stock.n[,"age"], gadgetYear,,,area,iter] <- stockParams[["m1"]] + stockParams[["m2"]]

		#print(m(fl_stock))

		# Set spwns as 0
		fl_stock@m.spwn[stock.n[,"age"], gadgetYear,,,area,iter] <- rep(0, length(stock.n[stock.n[["area"]]==area, "age"]))
		fl_stock@harvest.spwn[stock.n[,"age"], gadgetYear,,,area,iter] <- rep(0, length(stock.n[stock.n[["area"]]==area, "age"]))

		print("Surveys")

		# Survey
		if(!is.null(fl_index)) {
			# Ensure we have free slot
			fl_index <- window(fl_index, end = as.numeric(gadgetYear))
			fl_index@catch.n[index.n[,"age"],gadgetYear,,,area,iter] <- index.n[index.n[["area"]]==area, ncol(index.n)]
			fl_index@catch.wt[index.wt[,"age"],gadgetYear,,,area,iter] <- index.wt[index.wt[["area"]]==area, ncol(index.wt)]
			fl_index@index[index.n[,"age"],gadgetYear,,,area,iter] <- index.n[index.n[["area"]]==area, ncol(index.n)]
			fl_index@effort[,gadgetYear,,,area,iter] <- 1
		}
	}

	# Cleaning ups
	## Make sure we don't have zeros
	catch.n(fl_stock)[catch.n(fl_stock)==0] <- 1
	stock.n(fl_stock)[stock.n(fl_stock)==0] <- 1

	catch.n(fl_index)[catch.n(fl_index)==0] <- 1
	index(fl_index)[index(fl_index)==0] <- 1

	## Replace NAs with 3-years average value
	### Catch
	catch.wt(fl_stock) <- na.Ave(catch.wt(fl_stock), gadgetYear)
	discards.wt(fl_stock) <- na.Ave(discards.wt(fl_stock), gadgetYear)
	landings.wt(fl_stock) <- na.Ave(landings.wt(fl_stock), gadgetYear)
	stock.wt(fl_stock) <- na.Ave(stock.wt(fl_stock), gadgetYear)
	mat(fl_stock) <- na.Ave(mat(fl_stock), gadgetYear)
	harvest(fl_stock) <- na.Ave(harvest(fl_stock), gadgetYear)
	### Survey
	catch.wt(fl_index) <- na.Ave(catch.wt(fl_index), gadgetYear)

	return(list(stk=fl_stock, idx=fl_index))
}


#' @title Instruct gadget to run until a specific year and produces FLR stock and index objects
#' @description This function runs gadget until a specified year and automatically generate two 
#' FLR objects (FLStock and FLIndex) for each available stocks from the gadget run.
#' @param until end year of the simulation
#' @param globalparams a list object of parameters to aid gadget to FLR objects generation (See this page
#' \code{vignette("gadget-flr", package = "gadgetr")} for an example)
#' @importFrom FLCore FLStock FLQuant FLIndex expand units<-
#' catch catch.n catch.wt stock stock.n stock.wt
#' @export
runUntil <- function(until, globalparams) {

	# Time information
	firstYear <- globalparams[["firstYear"]]
	projYear <- globalparams[["projYear"]]
	finalYear <- globalparams[["finalYear"]]

	# Stock Names
	stockList <- globalparams[["stockList"]]

	combinedOut <- list()

	for (sname in stockList) {
		# Get params (for getting the ages)
		stockParams <- globalparams[[sname]][["params"]]

		print(paste(stockParams[["minage"]], stockParams[["maxage"]]))

		# Prepare FLstocks
		if(length(stockParams[["areas"]]) > 1) {
			area <- stockParams[["areas"]]
		} else {
			area <- "unique"
		}

		fl_stock <- FLStock(FLQuant(NA, dimnames=list(age=stockParams[["minage"]]:stockParams[["maxage"]], year=firstYear:(projYear-1), area=area)))
		fl_index <- FLIndex(FLQuant(NA, dimnames=list(age=stockParams[["minage"]]:stockParams[["maxage"]], year=firstYear:(projYear-1), area=area)))

		fl_stock@name <- sname
		fl_index@name <- sname

		# Set the type of FLIndex (FLSAM need this)
		fl_index@type <- "number"

		# Add auxilary (m2, predation mortality) information
		attr(fl_stock, "m2") <- expand(fl_stock@m, year=firstYear:finalYear)

		## Setting units and ranges
		FLCore::units(catch(fl_stock)) <- "kg"
		FLCore::units(catch.n(fl_stock)) <- "1"
		FLCore::units(catch.wt(fl_stock)) <- "kg"

		FLCore::units(discards(fl_stock)) <- "kg"
		FLCore::units(discards.n(fl_stock)) <- "1"
		FLCore::units(discards.wt(fl_stock)) <- "kg"

		FLCore::units(landings(fl_stock)) <- "kg"
		FLCore::units(landings.n(fl_stock)) <- "1"
		FLCore::units(landings.wt(fl_stock)) <- "kg"

		FLCore::units(stock(fl_stock)) <- "kg"
		FLCore::units(stock.n(fl_stock)) <- "1"
		FLCore::units(stock.wt(fl_stock)) <- "kg"

		FLCore::units(m(fl_stock)) <- "m"
		FLCore::units(harvest(fl_stock)) <- "f"

		## Setting fbar range stk
		range(fl_stock)["minfbar"] <- stockParams[["minfbar"]]
		range(fl_stock)["maxfbar"] <- stockParams[["maxfbar"]]

		## Setting f range for index
		range(fl_index)["startf"] <- stockParams[["startf"]]
		range(fl_index)["endf"] <- stockParams[["endf"]]

		combinedOut[[sname]] <- list(stk = fl_stock, idx = fl_index)
	}

	while (TRUE)
	{
		stats <- getEcosystemInfo()

		# Run and collect the stats for a year
		out <- runYear()
		
		for (sname in stockList) {
			print(sname)
			
			# Generate FLs
			updated <- updateFLStock(sname, out, as.character(stats$time[["currentYear"]]), combinedOut[[sname]]$stk, combinedOut[[sname]]$idx, globalparams)

			combinedOut[[sname]]$stk = updated$stk
			combinedOut[[sname]]$idx = updated$idx
		}

		if(stats$time[["currentYear"]] == until)
			break
	}
	return(combinedOut)
}

# Function to fill NA values in an FLR objects with average last three years
na.Ave <- function(inp, yr, yr.ave = 3) {
  ages <- dimnames(inp)$age
  years <- dimnames(inp)$year
  na.yrs <- years[colSums(is.na(inp)) > 0]
  na.yr <- intersect(as.character(yr), na.yrs)

  if( length(na.yr) > 0 ) {
    yr.ave.range <- as.character((as.numeric(na.yr)-yr.ave):(as.numeric(na.yr) - 1))
    na.target <- is.na(inp[, na.yr])
    tmp <- inp[, na.yr]
    tmp[na.target, na.yr] <- rowSums(inp[na.target, (yr.ave.range)])/yr.ave
    inp[, na.yr] <-  tmp
  }
  return(inp)
}

