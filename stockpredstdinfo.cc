#include "areatime.h"
#include "predstdinfobylength.h"
#include "preystdinfo.h"
#include "prey.h"
#include "stockpredator.h"
#include "stockpredstdinfo.h"
#include "stockpreystdinfo.h"
#include "stockprey.h"


StockPredStdInfo::StockPredStdInfo(const StockPredator* pred, const StockPrey* pRey, const intvector& Areas)
  : AbstrPredStdInfo(Areas, pred->Alproportion(Areas[0]).Minage(), pred->Alproportion(Areas[0]).Maxage(),
    pRey->AlkeysPriorToEating(Areas[0]).Minage(), pRey->AlkeysPriorToEating(Areas[0]).Maxage()),
  preyinfo(new StockPreyStdInfo(pRey, Areas)),
  predinfo(new PredStdInfoByLength(pred, pRey, Areas)),
  predator(pred), prey(pRey) {
}

StockPredStdInfo::StockPredStdInfo(const StockPredator* pred, const Prey* pRey, const intvector& Areas)
  : AbstrPredStdInfo(Areas, pred->Alproportion(Areas[0]).Minage(), pred->Alproportion(Areas[0]).Maxage(), 0, 0),
  preyinfo(new PreyStdInfo(pRey, Areas)),
  predinfo(new PredStdInfoByLength(pred, pRey, Areas)),
  predator(pred), prey(pRey) {
}

StockPredStdInfo::~StockPredStdInfo() {
  delete preyinfo;
  delete predinfo;
}

void StockPredStdInfo::Sum(const TimeClass* const TimeInfo, int area) {
  const int inarea = AreaNr[area];
  preyinfo->Sum(TimeInfo, area);
  predinfo->Sum(TimeInfo, area);
  int predage, preyage;
  for (predage = NconbyAge[inarea].Minage(); predage <= NconbyAge[inarea].Maxage(); predage++)
    for (preyage = NconbyAge[inarea].Mincol(predage); preyage < NconbyAge[inarea].Maxcol(predage); preyage++) {
      NconbyAge[inarea][predage][preyage] = 0;
      BconbyAge[inarea][predage][preyage] = 0;
    }
  const bandmatrix& Alprop = predator->Alproportion(area);
  doublevector predBconsbyAge(Alprop.Maxage() - Alprop.Minage() + 1, 0);
  const bandmatrix& preyNcons = preyinfo->NconsumptionByAgeAndLength(area);
  const bandmatrix& preyBcons = preyinfo->BconsumptionByAgeAndLength(area);
  const bandmatrix& predBcons = predator->Consumption(area, prey->Name());

  int preyl, predl;
  double timeratio;

  timeratio = TimeInfo->LengthOfYear() / TimeInfo->LengthOfCurrent();
  for (predage = Alprop.Minage(); predage <= Alprop.Maxage(); predage++)
    for (preyage = NconbyAge[inarea].Mincol(predage);
         preyage < NconbyAge[inarea].Maxcol(predage); preyage++) {
      for (preyl = 0; preyl < prey->NoLengthGroups(); preyl++) {
        for (predl = 0; predl < predator->NoLengthGroups(); predl++) {
          if (preyBcons[preyage][preyl] != 0 && predBcons[predl][preyl] != 0) {
            //B equals the biomass (predage, predl) eats of (preyage, preyl)
            //N equals the number (predage, predl) eats of (preyage, preyl)
            //prop equals the proportion of predation on (preyl) that
            //(predage, predl) accounts for.
            const double prop = predBcons[predl][preyl]* Alprop[predage][predl]
              / preyinfo->BconsumptionByLength(area)[preyl];
            const double B = prop * preyBcons[preyage][preyl];
            const double N = prop * preyNcons[preyage][preyl];
            BconbyAge[inarea][predage][preyage] += B;
            NconbyAge[inarea][predage][preyage] += N;
          }
        }
      }

      MortbyAge[inarea][predage][preyage]
        = (preyinfo->BconsumptionByAge(area)[preyage] == 0 ? 0
         : (preyinfo->MortalityByAge(area)[preyage] * BconbyAge[inarea][predage][preyage] /
         preyinfo->BconsumptionByAge(area)[preyage]) * timeratio);
    }
}

const bandmatrix& StockPredStdInfo::NconsumptionByLength(int area) const {
  return predinfo->NconsumptionByLength(area);
}

const bandmatrix& StockPredStdInfo::BconsumptionByLength(int area) const {
  return predinfo->BconsumptionByLength(area);
}

const bandmatrix& StockPredStdInfo::MortalityByLength(int area) const {
  return predinfo->MortalityByLength(area);
}