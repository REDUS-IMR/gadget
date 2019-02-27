context("gadget")

test_that("step sim works", {
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

  # Sim cleanup
  finalizeSim()

  # Gadget cleanup
  out <- finalize()
  
  expect_equal(lengthSteps, 115)
})

test_that("year sim works", {
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

  expect_equal(lengthYear, 28)
})
