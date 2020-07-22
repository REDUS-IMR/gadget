#include "ecosystem.h"
#include "maininfo.h"
#include "runid.h"
#include "gadget.h"
#include "errorhandler.h"
#include "stochasticdata.h"
#include "interrupthandler.h"
#include "global.h"
#include <Rcpp.h>

Ecosystem* EcoSystem;

MainInfo mainGlobal;
StochasticData* data;
int check;
char* workingdir;

// [[Rcpp::export]]
Rcpp::IntegerVector wholeSim(){
      EcoSystem->Simulate(mainGlobal.runPrint());
      if ((mainGlobal.getPI()).getPrint())
        EcoSystem->writeValues();

      while (data->isDataLeft()) {
        data->readNextLine();
        EcoSystem->Update(data);
        //EcoSystem->checkBounds();
        EcoSystem->Simulate(mainGlobal.runPrint());
        if ((mainGlobal.getPI()).getPrint())
          EcoSystem->writeValues();
      }
      delete data;
      return Rcpp::IntegerVector(1,0);
}

// [[Rcpp::export]]
Rcpp::IntegerVector updateAmountStep(Rcpp::IntegerVector fleetNo, Rcpp::IntegerVector step, Rcpp::IntegerVector area, Rcpp::NumericVector value){

   int fN = fleetNo[0] - 1;
   int st = step[0];

   double val = value[0];

   int maxFleet = 0;
   int maxSteps = 0;
   int maxArea = 0;

   AreaClass* Area = EcoSystem->getArea();
   int ar = Area->getInnerArea(area[0]);

   FleetPtrVector& fleetvec = EcoSystem->getModelFleetVector();

   maxFleet = fleetvec.Size();

   if(fN < 0 ||fN > maxFleet-1)
	   return Rcpp::IntegerVector(1, 55);

   Fleet *fleet = fleetvec[fN];
	
   FormulaMatrix& amount = fleet->getAmount();

   Rcpp::Rcout << "Change fleet \"" << fleet->getName() << "\" - Step: " << st << " - Area: " << ar + 1 << " with " << val << std::endl;

   //Rcpp::Rcout << "Row Size " << amount.Nrow() << std::endl; 

   maxSteps = amount.Nrow();

   if(st < 1 || st > maxSteps - 1)
           return Rcpp::IntegerVector(1, 55);

   //Rcpp::Rcout << "Column size for " << st << " " << amount.Ncol(st) << std::endl;

   maxArea = amount.Ncol(st);

   if(ar < 0 || ar > maxArea-1)
           return Rcpp::IntegerVector(1, 55);

   Formula& vec = amount[st][ar];

   Rcpp::Rcout << "Value before " << (double) vec << std::endl;

   vec.setValue(val);

   Rcpp::Rcout << "Value after " << (double) vec << std::endl;

   return  Rcpp::IntegerVector(1, 0);
}

// [[Rcpp::export]]
Rcpp::IntegerVector updateAmountYear(Rcpp::IntegerVector fleetNo, Rcpp::IntegerVector year, Rcpp::IntegerVector step, Rcpp::IntegerVector area, Rcpp::NumericVector value){

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();

   int timeid;

   int yy = year[0];
   int ss = step[0];

   if (TimeInfo->isWithinPeriod(yy, ss))
      timeid = TimeInfo->calcSteps(yy, ss);
   else
      return Rcpp::IntegerVector(1, 55);

   Rcpp::Rcout << "Step is " << timeid << std::endl;

   Rcpp::IntegerVector timeidvec(1,timeid);

   return updateAmountStep(fleetNo, timeidvec, area, value);
}

Rcpp::IntegerVector getEcosystemTime(Ecosystem* e){
   int res = 0;

   if(e->getCurrentYear() == e->getTimeInfo()->getPrevYear() &&
      e->getCurrentStep() == e->getTimeInfo()->getPrevStep()) res = 1;

   return
   Rcpp::IntegerVector::create(Rcpp::_["currentTime"] = e->getCurrentTime(),
                               Rcpp::_["currentYear"] = e->getCurrentYear(),
                               Rcpp::_["currentStep"] = e->getCurrentStep(),
                               Rcpp::_["totalSteps"] = e->numTotalSteps(),
			       Rcpp::_["finished"] = res);
}

//' Get the simulation ecosystem objects
//' 
//' This function returns a vector of information about the running gadget
//' simulation's stocks, fleet, state, and time information.
//'
//' @export
// [[Rcpp::export]]
Rcpp::List getEcosystemInfo() {

   FleetPtrVector& fleetvec = EcoSystem->getModelFleetVector();

   int maxFleet = fleetvec.Size();

   Rcpp::CharacterVector infoFleet(maxFleet);

   for(int fN=0;fN<maxFleet;fN++){
      Fleet *fleet = fleetvec[fN];
      infoFleet[fN] = fleet->getName();
   }

   StockPtrVector& stockvec = EcoSystem->getModelStockVector();
   int maxStock = stockvec.Size();

   Rcpp::CharacterVector infoStock(maxStock);

   for(int sN=0;sN<maxStock;sN++){
      Stock *stock = stockvec[sN];
      infoStock[sN] = stock->getName();
   }

   return Rcpp::List::create(Rcpp::Named("fleet") = infoFleet,
                Rcpp::Named("stock") = infoStock,
		Rcpp::Named("time") = getEcosystemTime(EcoSystem));

}

// [[Rcpp::export]]
Rcpp::IntegerVector initSim(){
   EcoSystem->initSimulation();
   return getEcosystemTime(EcoSystem);
}

// [[Rcpp::export]]
Rcpp::IntegerVector stepSim(){
   int res;
   res = EcoSystem->stepSimulation(mainGlobal.runPrint());
   return getEcosystemTime(EcoSystem);
}

// [[Rcpp::export]]
Rcpp::IntegerVector yearSim(){
   int res;
   res = EcoSystem->yearSimulation(mainGlobal.runPrint());
   return getEcosystemTime(EcoSystem);
}


// [[Rcpp::export]]
Rcpp::IntegerVector finalizeSim(){
   EcoSystem->finalizeSimulation();
   if ((mainGlobal.getPI()).getPrint())
        EcoSystem->writeValues();
   while (data->isDataLeft()) {
        data->readNextLine();
        EcoSystem->Update(data);
        //EcoSystem->checkBounds();
        EcoSystem->Simulate(mainGlobal.runPrint());
        if ((mainGlobal.getPI()).getPrint())
          EcoSystem->writeValues();
   }
   return Rcpp::IntegerVector(1, 0);
}

// [[Rcpp::export]]
Rcpp::List finalize(){

  if (workingdir) {
     delete workingdir;
     workingdir = 0;
  }

  if(EcoSystem) {
     delete EcoSystem;
     EcoSystem = 0;
  }

  if(data) {
     delete data;
     data = 0;
  }

  check = 0;

  return Rcpp::List::create(R_NilValue);
}

// [[Rcpp::export]]
Rcpp::List gadget(Rcpp::StringVector args) {

  //Ensure to empty everything
  finalize();

  //Get parameter number 
  int aNumber = args.size() + 1;

  char** aVector = new char* [255];

  //Convert args to aVector
  for(int i=0; i<args.size(); i++){
	char *tmp = new char [args(i).size() + 1]; 
	strcpy(tmp, Rcpp::as< std::string >(args(i)).c_str());
	aVector[i+1] = tmp;
  }

  data = 0;
  check = 0;

  //Initialise random number generator with system time [MNAA 02.02.26]
  srand((int)time(NULL));

  //Test to see if the function double lgamma(double) is returning an integer.
  //lgamma is a non-ansi function and on some platforms when compiled with the
  //-ansi flag lgamma returns an integer value. [MNAA&AJ 05.2001]
  assert(lgamma(1.2) != floor(lgamma(1.2)));

  workingdir = getenv("GADGET_WORKING_DIR");
  if (workingdir == 0) {
    if ((workingdir = (char*)malloc(LongString)) == NULL)
      handle.logMessage(LOGFAIL, "Error - failed to malloc space for current working directory");
    check = 1;
    if (getcwd(workingdir, LongString) == NULL)
      handle.logMessage(LOGFAIL, "Error - failed to get current working directory");
  }
  if (chdir(workingdir) != 0)
    handle.logMessage(LOGFAIL, "Error - failed to change working directory to", workingdir);

  char* inputdir = getenv("GADGET_DATA_DIR");
  if (inputdir == 0)
    inputdir = workingdir;
  if (chdir(inputdir) != 0)
    handle.logMessage(LOGFAIL, "Error - failed to change input directory to", inputdir);
  if (chdir(workingdir) != 0) //JMB change back to where we were ...
    handle.logMessage(LOGFAIL, "Error - failed to change working directory to", workingdir);

  mainGlobal.read(aNumber, aVector);
  mainGlobal.checkUsage(inputdir, workingdir);

  if (chdir(inputdir) != 0)
    handle.logMessage(LOGFAIL, "Error - failed to change input directory to", inputdir);
  EcoSystem = new Ecosystem(mainGlobal);

#ifdef INTERRUPT_HANDLER
  //JMB dont register interrupt if doing a network run
  if (!(mainGlobal.runNetwork()))
    registerInterrupts(&EcoSystem->interrupted);
#endif

  if (chdir(workingdir) != 0)
    handle.logMessage(LOGFAIL, "Error - failed to change working directory to", workingdir);
  if ((mainGlobal.getPI()).getPrint())
    EcoSystem->writeInitialInformation((mainGlobal.getPI()).getOutputFile());

  if (mainGlobal.runStochastic()) {
    if (mainGlobal.runNetwork()) {
#ifdef GADGET_NETWORK //to help compiling when pvm libraries are unavailable
      EcoSystem->Initialise();
      data = new StochasticData();
      while (data->getDataFromNetwork()) {
        EcoSystem->Update(data);
        EcoSystem->Simulate(mainGlobal.runPrint());
        data->sendDataToNetwork(EcoSystem->getLikelihood());
        data->readNextLineFromNetwork();
      }
      delete data;
#endif

    } else if (mainGlobal.getInitialParamGiven()) {
      if (chdir(inputdir) != 0) //JMB need to change back to inputdir to read the file
        handle.logMessage(LOGFAIL, "Error - failed to change input directory to", inputdir);
      data = new StochasticData(mainGlobal.getInitialParamFile());
      if (chdir(workingdir) != 0)
        handle.logMessage(LOGFAIL, "Error - failed to change working directory to", workingdir);

      EcoSystem->Update(data);
      EcoSystem->checkBounds();

      EcoSystem->Initialise();
      if (mainGlobal.printInitial()) {
        EcoSystem->Reset();  //JMB only need to call reset() before the print commands
        EcoSystem->writeStatus(mainGlobal.getPrintInitialFile());
      }

      // IU: Try to exit here
      return Rcpp::List::create(R_NilValue);

      EcoSystem->Simulate(mainGlobal.runPrint());
      if ((mainGlobal.getPI()).getPrint())
        EcoSystem->writeValues();

      while (data->isDataLeft()) {
        data->readNextLine();
        EcoSystem->Update(data);
        //EcoSystem->checkBounds();
        EcoSystem->Simulate(mainGlobal.runPrint());
        if ((mainGlobal.getPI()).getPrint())
          EcoSystem->writeValues();
      }
      delete data;

    } else {
      if (EcoSystem->numVariables() != 0)
        handle.logMessage(LOGWARN, "Warning - no parameter input file given, using default values");

      EcoSystem->Initialise();
      if (mainGlobal.printInitial()) {
        EcoSystem->Reset();  //JMB only need to call reset() before the print commands
        EcoSystem->writeStatus(mainGlobal.getPrintInitialFile());
      }

      EcoSystem->Simulate(mainGlobal.runPrint());
      if ((mainGlobal.getPI()).getPrint())
        EcoSystem->writeValues();
    }

  } else if (mainGlobal.runOptimise()) {
    if (EcoSystem->numVariables() == 0)
      handle.logMessage(LOGFAIL, "Error - no parameters can be optimised");

    if (mainGlobal.getInitialParamGiven()) {
      if (chdir(inputdir) != 0) //JMB need to change back to inputdir to read the file
        handle.logMessage(LOGFAIL, "Error - failed to change input directory to", inputdir);
      data = new StochasticData(mainGlobal.getInitialParamFile());
      if (chdir(workingdir) != 0)
        handle.logMessage(LOGFAIL, "Error - failed to change working directory to", workingdir);

      EcoSystem->Update(data);
      EcoSystem->checkBounds();
      delete data;
    } else
      handle.logMessage(LOGFAIL, "Error - no parameter input file specified");

    EcoSystem->Initialise();
    if (mainGlobal.printInitial()) {
      EcoSystem->Reset();  //JMB only need to call reset() before the print commands
      EcoSystem->writeStatus(mainGlobal.getPrintInitialFile());
    }

    EcoSystem->Optimise();
    if (mainGlobal.getForcePrint())
      EcoSystem->Simulate(mainGlobal.getForcePrint());
  }

  handle.logMessage(LOGMESSAGE, "");  //write blank line to log file
  if (mainGlobal.printFinal() && !(mainGlobal.runNetwork()))
    EcoSystem->writeStatus(mainGlobal.getPrintFinalFile());

  //JMB print final values of parameters
  if (!(mainGlobal.runNetwork()))
    EcoSystem->writeParams((mainGlobal.getPI()).getParamOutFile(), (mainGlobal.getPI()).getPrecision());

  if (check)
    free(workingdir);

  //Rcpp::List z = Rcpp::clone(EcoSystem->rdata);
  delete EcoSystem;
  handle.logFinish();
  //return EXIT_SUCCESS;
  //
  return Rcpp::List::create(R_NilValue);
}

// [[Rcpp::export]]
Rcpp::LogicalVector isGadgetInitialized(){
  if(!EcoSystem)
    return Rcpp::LogicalVector(1, FALSE);
  else
    return Rcpp::LogicalVector(1, TRUE);
}

