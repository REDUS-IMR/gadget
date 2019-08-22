#include "ecosystem.h"
#include "renewal.h"
#include "stockprey.h"
#include "spawner.h"

#include <Rcpp.h>

extern Ecosystem* EcoSystem;

// [[Rcpp::export]]
Rcpp::IntegerVector setEating(Rcpp::IntegerVector stockNo, Rcpp::IntegerVector val){

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   stock->setEat(val[0]);

   if(stock->doesEat() == val[0])
     return Rcpp::IntegerVector(1,0);
   else
     return Rcpp::IntegerVector(1,1);
}

// [[Rcpp::export]]
Rcpp::List getStockInfoC(Rcpp::IntegerVector stockNo){

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   // Get length group labels
   const LengthGroupDivision* lengthGroup = stock->getLengthGroupDiv();

   int lSize = lengthGroup->numLengthGroups();

   Rcpp::NumericVector maxLength(lSize);
   Rcpp::NumericVector minLength(lSize);

   for(int i = 0; i < lSize; i++){
      maxLength[i] = lengthGroup->maxLength(i);
      minLength[i] = lengthGroup->minLength(i);
   }

   // Get renewal and recruitment information
   bool hasRenew = false;
   if(stock->getRenewalData()){
     hasRenew = true;
   }

   bool hasSpawn = false;
   if(stock->getSpawnData()){
     hasSpawn = true;
   }

   return Rcpp::List::create(Rcpp::_("lengthGroup") =
				  Rcpp::DataFrame::create(Rcpp::_("minLength") = minLength,
                                  Rcpp::_("maxLength") = maxLength),
          Rcpp::_("hasRenew") = hasRenew,
          Rcpp::_("hasSpawn") = hasSpawn);
}

// [[Rcpp::export]]
Rcpp::IntegerVector updateRenewalC(Rcpp::IntegerVector stockNo, Rcpp::IntegerVector year, Rcpp::IntegerVector step,
                Rcpp::IntegerVector area,
		Rcpp::IntegerVector age, Rcpp::IntegerVector number, Rcpp::NumericVector mean,
		Rcpp::NumericVector sdev, Rcpp::NumericVector alpha, Rcpp::NumericVector beta,
		Rcpp::NumericVector length,Rcpp::NumericVector meanWeight){

   int maxage, minage;

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();
   AreaClass* Area = EcoSystem->getArea();
   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Keeper* keeper = EcoSystem->getKeeper();

   Stock *stock = stockvec[stockNo[0]-1];

   maxage = stock->maxAge();
   minage = maxage = stock->minAge();

   RenewalData* renewal = stock->getRenewalData();

   if(!renewal){
     Rcpp::Rcout << "This stock doesn't renew" << std::endl;
     return Rcpp::IntegerVector(1,55);
   }


   int readoption = renewal->getReadOption();

#ifdef DEBUG
   Rcpp::Rcout << "Renewal type: " << readoption << std::endl;

   Rcpp::Rcout << "print data before" << std::endl;
   std::ofstream ofs;
   ofs.open ("/tmp/testofs-before.txt", std::ofstream::out);

   renewal->Print(ofs);
   ofs.close();
#endif

   if(readoption == 0){
     //Normal condition -- not implemented yet
   }else if(readoption == 1){
     //Normal parameter
     //Rcpp::Rcout << year[0] << step[0] << area[0] << age[0] << number[0] << mean[0] << sdev[0] << alpha[0] << beta[0] << std::endl;
     renewal->updateNormalParameterData(year[0], step[0], area[0], age[0], number[0], mean[0], sdev[0], alpha[0], beta[0],
       keeper, TimeInfo, Area, minage, maxage);
   }else{
     //Number
     renewal->updateNumberData(year[0], step[0], area[0], age[0], length[0], number[0], meanWeight[0],
       keeper, TimeInfo, Area, minage, maxage);
   }

#ifdef DEBUG
   Rcpp::Rcout << "print data after" << std::endl;
   ofs.open ("/tmp/testofs-after.txt", std::ofstream::out);

   renewal->Print(ofs);
   ofs.close();
#endif

   return Rcpp::IntegerVector(1, 0);
}

// [[Rcpp::export]]
Rcpp::IntegerVector updateSuitabilityC(Rcpp::IntegerVector fleetNo, Rcpp::IntegerVector stockNo, Rcpp::NumericVector len, Rcpp::NumericVector value){

   int lenIdx;

   FleetPtrVector& fleetvec = EcoSystem->getModelFleetVector();

   Fleet *fleet = fleetvec[fleetNo[0]-1];

   LengthPredator *predator = fleet->getPredator();

   DoubleMatrix *suit = predator->getSuits(stockNo[0]-1);

#ifdef DEBUG
   for(int i=0; i<suit->Nrow(); i++){
      for(int j=0; j<suit->Ncol(i); j++){
         Rcpp::Rcout << (*suit)[i][j] << " ";
      }
      Rcpp::Rcout << std::endl;
   }
#endif

   if(suit->Nrow() == 0){
     Rcpp::Rcout << "No suitability vector found" << std::endl;
     return Rcpp::IntegerVector(1,55);
   }

   const LengthGroupDivision* lengthGroup = predator->getLengthGroupDiv();

   lenIdx = lengthGroup->numLengthGroup(len[0]);

   if(lenIdx == -1){
     Rcpp::Rcout << "Invalid length" << std::endl;
   }

#ifdef DEBUG
   DoubleVector suitVal = (*suit)[0];

   Rcpp::Rcout << "Bef: " << suitVal[lenIdx] << " " << (*suit)[0][lenIdx] << std::endl;
#endif

   //Since predator length for fleet will always be one, pick only the first element
   (*suit)[0].set(lenIdx, value[0]);

#ifdef DEBUG
   Rcpp::Rcout << "Aft: " << suitVal[lenIdx] << " " << (*suit)[0][lenIdx] << std::endl;
#endif

   return Rcpp::IntegerVector(1,0);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix printPredatorPrey(Rcpp::IntegerVector predatorNo, Rcpp::IntegerVector stockNo, Rcpp::StringVector predatorType){

   int i, j, k, h, l;
   double ratio;

   const DoubleVector* suitptr;
   const AgeBandMatrix* alptr;

   const ConversionIndex *CI;

   AgeBandMatrixPtrVector total;
   AgeBandMatrixPtrVector consume;
   DoubleMatrixPtrVector mortality;

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();

   AreaClass* Area = EcoSystem->getArea();

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   IntVector areas = stock->getAreas();

   PopPredator *predator = NULL;

   //check type of predator (can be fleet or another stock)
   if(strcmp(predatorType[0], "fleet") == 0){
      Rcpp::IntegerVector fleetNo = predatorNo;
      FleetPtrVector& fleetvec = EcoSystem->getModelFleetVector();
      Fleet *fleet = fleetvec[fleetNo[0]-1];
      predator = fleet->getPredator();
   }else if(strcmp(predatorType[0], "stock") == 0){
      Stock *stockPredator = stockvec[predatorNo[0]-1];
      if(stockPredator->doesEat())
         predator = stockPredator->getPredator();
   }else{
      Rcpp::Rcout << "Error in predatorpreyaggregator - error in predator type, should be either fleet or stock" << std::endl;
      Rcpp::NumericMatrix m(0,0);
      return m;
   }

   StockPrey* prey;

   // Make sure the stock is eaten and predator is defined before continuing
   if(stock->isEaten() && predator != NULL)
     prey = stock->getPrey();
   else{
     Rcpp::NumericMatrix m(0,0);
     return m;
   }

   CI = new ConversionIndex(prey->getLengthGroupDiv(), prey->getLengthGroupDiv());

   if (CI->Error())
     Rcpp::Rcout << "Error in predatorpreyaggregator - error when checking length structure" << std::endl;

   //check that the prey is a stock
   if (prey->getType() == LENGTHPREY)
     Rcpp::Rcout << "Error in predatorpreyaggregator - cannot aggregate prey" << prey->getName() << std::endl;

   for (j = 0; j < areas.Size(); j++){
      alptr = &prey->getConsumptionALK(areas[j]);
      mortality.resize(new DoubleMatrix(alptr->Nrow(), prey->getLengthGroupDiv()->numLengthGroups(), 0.0));
   }

   PopInfo tmppop;
   tmppop.N = 1.0;
   PopInfoMatrix popmatrix(alptr->Nrow(), prey->getLengthGroupDiv()->numLengthGroups(), tmppop);
   total.resize(areas.Size(), 0, 0, popmatrix);
   consume.resize(areas.Size(), 0, 0, popmatrix);

   // Set to zero
   for (i = 0; i < mortality.Size(); i++) {
     total[i].setToZero();
     consume[i].setToZero();
     (*mortality[i]).setToZero();
   }

   //Sum over the appropriate predators, preys, areas, ages and length groups
   //First calculate the prey population that is consumed by the predation
   if (predator->doesEat(prey->getName())) {
          for (j = 0; j < areas.Size(); j++) {
            if ((prey->isPreyArea(areas[j])) && (predator->isInArea(areas[j]))) {
              for (k = 0; k < predator->numPreys(); k++) {
                if (strcasecmp(prey->getName(), predator->getPrey(k)->getName()) == 0) {
                  alptr = &prey->getConsumptionALK(areas[j]);
                  for (h = 0; h < predator->getLengthGroupDiv()->numLengthGroups(); h++) {
                    //suitptr = &predator->getSuitability(k)[h];
                    suitptr = &predator->getUseSuitability(areas[j], k)[h];
                    ratio = predator->getConsumptionRatio(areas[j], k, h);
                    for (l = 0; l < alptr->Nrow(); l++){
                          int age = alptr->minAge() + l;
                          consume[j][l].Add((*alptr)[age], *CI, *suitptr, ratio);
                    }
                  }
                }
              }
            }
          }
   }

  //Then calculate the prey population before predation
  for (j = 0; j < areas.Size(); j++) {
        if (prey->isPreyArea(areas[j])) {
          alptr = &prey->getConsumptionALK(areas[j]);
          for (l = 0; l < alptr->Nrow(); l++){
                int age = alptr->minAge() + l;
                total[j][l].Add((*alptr)[age], *CI);
          }

        }
  }

  // Finally calculate the mortality caused by the predation
  ratio = 1.0 / TimeInfo->getTimeStepSize();
  for (i = 0; i < mortality.Size(); i++)
    for (j = 0; j < (*mortality[i]).Nrow(); j++)
      for (k = 0; k < (*mortality[i]).Ncol(j); k++)
        (*mortality[i])[j][k] = calcMortality(consume[i][j][k].N, total[i][j][k].N, ratio);

  //Print it

#ifdef DEBUG
  int width = 0;
  int lowwidth = 0;
  int printwidth = 0;
  int precision = 4;
#endif

  int age, len;

  Rcpp::DataFrame df;

  for (j = 0; j < areas.Size(); j++) {
    for (age = consume[areas[j]].minAge(); age <= consume[areas[j]].maxAge(); age++) {
      for (len = consume[areas[j]].minLength(age); len < consume[areas[j]].maxLength(age); len++) {
#ifdef DEBUG
        Rcpp::Rcout << setw(lowwidth) << TimeInfo->getPrevYear() << sep
          << setw(lowwidth) << TimeInfo->getPrevStep() << sep
          << setw(printwidth) << Area->getModelArea(areas[j]) << sep
          << setw(printwidth) << stock->minAge() + age << sep
          << setw(printwidth) << prey->getLengthGroupDiv()->minLength(len) << sep;
#endif
        Rcpp::NumericVector v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
            TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), stock->minAge() + age,
            prey->getLengthGroupDiv()->minLength(len) );

        Rcpp::NumericVector w;
        // JMB crude filter to remove the 'silly' values from the output
        if ((consume[areas[j]][age][len].N < rathersmall) || (consume[areas[j]][age][len].W < 0.0)){
#ifdef DEBUG
          Rcpp::Rcout << setw(width) << 0 << sep << setw(width) << 0 << sep << setw(width) << 0 << endl;
#endif
          w = Rcpp::NumericVector::create(0, 0, 0);
        }else{
#ifdef DEBUG
          Rcpp::Rcout << setprecision(precision) << setw(width) << consume[areas[j]][age][len].N
            << sep << setprecision(precision) << setw(width)
            << consume[areas[j]][age][len].N * consume[areas[j]][age][len].W
            << sep << setprecision(precision) << setw(width)
            << (*mortality[areas[j]])[age][len] << endl;
#endif
          w = Rcpp::NumericVector::create(consume[areas[j]][age][len].N,
            consume[areas[j]][age][len].N * consume[areas[j]][age][len].W,
            (*mortality[areas[j]])[age][len]);
        }

        // Do: z <- c(v,w)
        Rcpp::NumericVector z(v.size() + w.size());

        std::copy(v.begin(), v.end(), z.begin());
        std::copy(w.begin(), w.end(), z.begin() + v.size());

        // Append at the end of the DataFrame
        df.insert(df.end(), z);
      }
    }
  }

  // Give names to the columns
  Rcpp::CharacterVector namevec = Rcpp::CharacterVector::create("year", "step",
     "area", "age", "length", "numberConsumed", "biomassConsumed", "mortality");

  // Convert to matrix so that we can transpose it (nrows = 8)
  int dfsize = df.size();
  Rcpp::NumericMatrix mattemp(8, dfsize);
  for ( i = 0; i < dfsize; i++ ) {
      mattemp(Rcpp::_, i) = Rcpp::NumericVector(df[i]);
  }

  Rcpp::rownames(mattemp) = namevec;

  Rcpp::NumericMatrix mout = Rcpp::transpose(mattemp);

  return mout;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix printStock(Rcpp::IntegerVector stockNo){

   int j, age, len, minage;

   const AgeBandMatrix* alptr;

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();

   AreaClass* Area = EcoSystem->getArea();

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   IntVector areas = stock->getAreas();

#ifdef DEBUG
   int width = 0;
   int lowwidth = 0;
   int precision = 4;
#endif

   Rcpp::DataFrame df;

   for (j = 0; j < areas.Size(); j++) {
     alptr = &stock->getCurrentALK(areas[j]);
     for (age = alptr->minAge(); age <= alptr->maxAge(); age++) {
       for (len = alptr->minLength(age); len < alptr->maxLength(age); len++) {
#ifdef DEBUG
         Rcpp::Rcout << setw(lowwidth) << TimeInfo->getYear() << sep
           << setw(lowwidth) << TimeInfo->getStep() << sep
           << setw(lowwidth) << Area->getModelArea(areas[j]) << sep << setw(lowwidth)
           << age  << sep << setw(lowwidth)
           << stock->getLengthGroupDiv()->minLength(len) << sep;
#endif
         Rcpp::NumericVector v = Rcpp::NumericVector::create(TimeInfo->getYear(),
            TimeInfo->getStep(), Area->getModelArea(areas[j]), age,
            stock->getLengthGroupDiv()->minLength(len));
         Rcpp::NumericVector w;

         //JMB crude filter to remove the 'silly' values from the output
         if (((*alptr)[age][len].N < rathersmall) || ((*alptr)[age][len].W < 0.0)){
#ifdef DEBUG
           Rcpp::Rcout << setw(width) << 0 << sep << setw(width) << 0 << endl;
#endif
           w = Rcpp::NumericVector::create(0, 0);
         }else{
#ifdef DEBUG
           Rcpp::Rcout << setprecision(precision) << setw(width) << (*alptr)[age][len].N << sep
             << setprecision(precision) << setw(width) << (*alptr)[age][len].W << endl;
#endif
           w = Rcpp::NumericVector::create((*alptr)[age][len].N, (*alptr)[age][len].W);
         }

         // Do: z <- c(v,w)
         Rcpp::NumericVector z(v.size() + w.size());

         std::copy(v.begin(), v.end(), z.begin());
         std::copy(w.begin(), w.end(), z.begin() + v.size());

         // Append at the end of the DataFrame
         df.insert(df.end(), z);
       }
     }
  }

  // Give names to the columns
  Rcpp::CharacterVector namevec = Rcpp::CharacterVector::create("year", "step",
     "area", "age", "length", "number", "meanWeights");

  // Convert to matrix so that we can transpose it (nrows = 7)
  int dfsize = df.size();
  Rcpp::NumericMatrix mattemp(7, dfsize);
  for ( j = 0; j < dfsize; j++ ) {
      mattemp(Rcpp::_, j) = Rcpp::NumericVector(df[j]);
  }

  Rcpp::rownames(mattemp) = namevec;

  Rcpp::NumericMatrix mout = Rcpp::transpose(mattemp);

  return mout;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix printDetailedSSB(Rcpp::IntegerVector stockNo){

   int j, age, len;

   const AgeBandMatrix* alptr;

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();

   AreaClass* Area = EcoSystem->getArea();

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   IntVector areas = stock->getAreas();

   if(!stock->getSpawnData()){
	   Rcpp::NumericMatrix m(0,0);
	   return m;
   }

   const DoubleMatrixPtrVector* spawnNumbers = stock->getSpawnData()->getSpawnNumbers();

#ifdef DEBUG
   int width = 0;
   int lowwidth = 0;
   int precision = 4;
#endif

   Rcpp::DataFrame df;

   for (j = 0; j < areas.Size(); j++) {
     alptr = &stock->getCurrentALK(areas[j]);
     for (age = alptr->minAge(); age <= alptr->maxAge(); age++) {
       for (len = alptr->minLength(age); len < alptr->maxLength(age); len++) {
#ifdef DEBUG
         Rcpp::Rcout << setw(lowwidth) << TimeInfo->getPrevYear() << sep
           << setw(lowwidth) << TimeInfo->getPrevStep() << sep
           << setw(lowwidth) << Area->getModelArea(areas[j]) << sep << setw(lowwidth)
           << age << sep << setw(lowwidth)
           << stock->getLengthGroupDiv()->minLength(len) << sep;
#endif
         Rcpp::NumericVector v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
            TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), age,
            stock->getLengthGroupDiv()->minLength(len));
         Rcpp::NumericVector w;

         if (((*(*spawnNumbers)[areas[j]])[age][len]  < rathersmall) || ((*(*spawnNumbers)[areas[j]])[age][len]< 0.0)){
#ifdef DEBUG
           Rcpp::Rcout << setw(width) << 0 << endl;
#endif
           w = Rcpp::NumericVector::create(0);
         }else{
#ifdef DEBUG
           Rcpp::Rcout << setprecision(precision) << setw(width) <<  (*(*spawnNumbers)[areas[j]])[age][len] << endl;
#endif
           w = Rcpp::NumericVector::create((*(*spawnNumbers)[areas[j]])[age][len]);
         }

         // Do: z <- c(v,w)
         Rcpp::NumericVector z(v.size() + w.size());

         std::copy(v.begin(), v.end(), z.begin());
         std::copy(w.begin(), w.end(), z.begin() + v.size());

         // Append at the end of the DataFrame
         df.insert(df.end(), z);
       }
     }
  }

  // Give names to the columns
  Rcpp::CharacterVector namevec = Rcpp::CharacterVector::create("year", "step",
     "area", "age", "length", "SSB");

  // Convert to matrix so that we can transpose it (nrows = 6)
  int dfsize = df.size();
  Rcpp::NumericMatrix mattemp(6, dfsize);
  for ( j = 0; j < dfsize; j++ ) {
      mattemp(Rcpp::_, j) = Rcpp::NumericVector(df[j]);
  }

  Rcpp::rownames(mattemp) = namevec;

  Rcpp::NumericMatrix mout = Rcpp::transpose(mattemp);

  return mout;
}

// [[Rcpp::export]]
Rcpp::IntegerVector updateRecruitmentC(Rcpp::IntegerVector stockNo, Rcpp::NumericVector recruitParams){

   int j;
   int functionnumber, paramsize;

   AreaClass* Area = EcoSystem->getArea();

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   IntVector areas = stock->getAreas();

   if(!stock->getSpawnData()){
	   return Rcpp::IntegerVector(1,55);
   }

   ModelVariableVector* spawnparameters = stock->getSpawnData()->getSpawnParameters();
   const char* functionname = stock->getSpawnData()->getFunctionName();

   // Determine the function type and its number of parameters
   if (strcasecmp(functionname, "simplessb") == 0) {
     functionnumber = 1;
     paramsize = 1;
   } else if (strcasecmp(functionname, "ricker") == 0) {
     functionnumber = 2;
     paramsize = 2;
   } else if (strcasecmp(functionname, "bevertonholt") == 0) {
     functionnumber = 3;
     paramsize = 2;
   } else if (strcasecmp(functionname, "fecundity") == 0) {
     functionnumber = 4;
     paramsize = 5;
   } else if (strcasecmp(functionname, "baleen") == 0) {
     functionnumber = 5;
     paramsize = 4;
   } else if (strcasecmp(functionname, "hockeystick") == 0) {
     functionnumber = 6;
     paramsize = 2;
   } else {
     Rcpp::Rcout << "unrecognised recruitment function " << functionname << std::endl;
     return Rcpp::IntegerVector(1,55);
   }

   // Check the length of user-defined parameters
   if(recruitParams.length() != paramsize){
      Rcpp::Rcout << "Recruitment function " << functionname << " requires "
         << paramsize << " parameters!" << std::endl;
      return Rcpp::IntegerVector(1,55);
   }

   Rcpp::Rcout << ">> Change: " << functionname << " with params: ";
   for (j = 0 ; j < paramsize ; j++)
      Rcpp::Rcout << (*spawnparameters)[j] << " ";

   // Don't update value if it's negative
   for (j = 0 ; j < paramsize ; j++){
      if(recruitParams[j] > -1)
         (*spawnparameters)[j].setValue(recruitParams[j]);
   }

   Rcpp::Rcout << "... into: " << functionname << " with params: ";
   for (j = 0 ; j < paramsize ; j++)
      Rcpp::Rcout << (*spawnparameters)[j] << " ";
   Rcpp::Rcout << endl;

   return Rcpp::IntegerVector(1,0);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix printSSB(Rcpp::IntegerVector stockNo){

   int j;

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();

   AreaClass* Area = EcoSystem->getArea();

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   IntVector areas = stock->getAreas();

   SpawnData* spawner = stock->getSpawnData();
   if(!spawner){
	   Rcpp::NumericMatrix m(0,0);
	   return m;
   }

   Rcpp::DataFrame df;

   for (j = 0; j < areas.Size(); j++) {
      Rcpp::NumericVector v;
      if(spawner->isSpawnStepAreaPrev(areas[j], TimeInfo)){
          v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
            TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), spawner->getSSB()[areas[j]]);
      }else{
          v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
            TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), 0.0);
      }
      // Append at the end of the DataFrame
      df.insert(df.end(), v);
   }

   // Give names to the columns
   Rcpp::CharacterVector namevec = Rcpp::CharacterVector::create("year", "step",
     "area", "SSB");

   // Convert to matrix so that we can transpose it (nrows = 4)
   int dfsize = df.size();
   Rcpp::NumericMatrix mattemp(4, dfsize);
   for ( j = 0; j < dfsize; j++ ) {
      mattemp(Rcpp::_, j) = Rcpp::NumericVector(df[j]);
   }

   Rcpp::rownames(mattemp) = namevec;

   Rcpp::NumericMatrix mout = Rcpp::transpose(mattemp);

   return mout;
}

// [[Rcpp::export]]
Rcpp::List printRecruitment(Rcpp::IntegerVector stockNo){

   int j;

   TimeClass* TimeInfo = EcoSystem->getTimeInfo();

   AreaClass* Area = EcoSystem->getArea();

   StockPtrVector stockvec = EcoSystem->getModelStockVector();
   Stock *stock = stockvec[stockNo[0]-1];

   IntVector areas = stock->getAreas();

   Rcpp::DataFrame dfRec, dfRen;
   Rcpp::NumericMatrix matRec, matRen;

   // Get from Spawner and Renewal
   SpawnData* spawner = stock->getSpawnData();
   RenewalData* renewal = stock->getRenewalData();

   if(!spawner && !renewal){
        return Rcpp::List::create(
          Rcpp::_["renew"]  = matRen,
          Rcpp::_["spawn"]  = matRec);
   }

   if(spawner){
      for (j = 0; j < areas.Size(); j++) {
         Rcpp::NumericVector v;
         if(spawner->isSpawnStepAreaPrev(areas[j], TimeInfo)){
            v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
               TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), spawner->getRec()[areas[j]]);
         }else{
            v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
               TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), 0.0);
         }
         // Append at the end of the DataFrame
         dfRec.insert(dfRec.end(), v);
      }

      // Give names to the columns
      Rcpp::CharacterVector namevec = Rcpp::CharacterVector::create("year", "step",
        "area", "Rec");

      // Convert to matrix so that we can transpose it (nrows = 4)
      int dfsize = dfRec.size();
      Rcpp::NumericMatrix mattemp(4, dfsize);
      for ( j = 0; j < dfsize; j++ ) {
         mattemp(Rcpp::_, j) = Rcpp::NumericVector(dfRec[j]);
      }

      Rcpp::rownames(mattemp) = namevec;

      matRec = Rcpp::transpose(mattemp);
   }

   if(renewal){
      for (j = 0; j < areas.Size(); j++) {
         Rcpp::NumericVector v;
	 if(renewal->isRenewalPrevStepArea(areas[j], TimeInfo)){
	    Rcpp::List renewalInfo = stock->getCurrentALK(areas[j]).getRenewalInfo();
            for(int ll = 0; ll < renewalInfo.length(); ll++){
               Rcpp::NumericVector tmp = renewalInfo[ll];
               v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
                  TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), tmp[0],
                  stock->getLengthGroupDiv()->minLength(tmp[1]), tmp[2], tmp[3]);
               dfRen.insert(dfRen.end(), v);
            }
         }else{
               v = Rcpp::NumericVector::create(TimeInfo->getPrevYear(),
                  TimeInfo->getPrevStep(), Area->getModelArea(areas[j]), 0, 0, 0, 0);
               dfRen.insert(dfRen.end(), v);
         }
      }
      // Give names to the columns
      Rcpp::CharacterVector namevec = Rcpp::CharacterVector::create("year", "step",
        "area", "age", "length", "renewalNumber", "renewalWeight");

      // Convert to matrix so that we can transpose it (nrows = 7)
      int dfsize = dfRen.size();
      Rcpp::NumericMatrix mattemp(7, dfsize);
      for ( j = 0; j < dfsize; j++ ) {
         mattemp(Rcpp::_, j) = Rcpp::NumericVector(dfRen[j]);
      }

      Rcpp::rownames(mattemp) = namevec;

      matRen = Rcpp::transpose(mattemp);
   }

   return Rcpp::List::create( 
      Rcpp::_["renew"]  = matRen,
      Rcpp::_["spawn"]  = matRec);
}
