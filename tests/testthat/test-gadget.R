context("gadget")

test_that("step sim works", {
  # Init the example haddock data
  exPath <- gadgetr::loadExample()
  gadgetr::initGadget(exPath, "refinputfile")

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
  gadgetr::endGadget()  

  expect_equal(lengthSteps, 171)
})

test_that("year sim works", {
  # Init the example haddock data
  exPath <- gadgetr::loadExample()
  gadgetr::initGadget(exPath, "refinputfile")

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
  gadgetr::endGadget()

  expect_equal(lengthYear, 42)
})
