#include "areatime.h"
#include "errorhandler.h"
#include "stockstdprinter.h"
#include "stockaggregator.h"
#include "charptrvector.h"
#include "stockptrvector.h"
#include "stock.h"
#include "popstatistics.h"
#include "conversion.h"
#include "stockpreystdinfo.h"
#include "readword.h"
#include "readaggregation.h"
#include "gadget.h"

#include "runid.h"
extern RunID RUNID;

StockStdPrinter::StockStdPrinter(CommentStream& infile,
  const AreaClass* const Area, const  TimeClass* const TimeInfo)
  : Printer(STOCKSTDPRINTER), stockname(0), LgrpDiv(0),
    minage(0), maxage(0), aggregator(0), preyinfo(0), Scale(1) {

  ErrorHandler handle;
  char text[MaxStrLength];
  strncpy(text, "", MaxStrLength);
  int i;

  stockname = new char[MaxStrLength];
  strncpy(stockname, "", MaxStrLength);
  readWordAndValue(infile, "stockname", stockname);
  infile >> text >> ws;
  if (strcasecmp(text, "scale") == 0)
    infile >> Scale >> ws >> text >> ws;
  else
    Scale = 1.0;

  if (Scale <= 0)
    handle.Message("Illegal value of scale - must be strictly positive");

  //Read in area aggregation from file
  char filename[MaxStrLength];
  strncpy(filename, "", MaxStrLength);
  ifstream datafile;
  CommentStream subdata(datafile);

  CharPtrVector areaindex;
  IntMatrix tmpareas;

  if (strcasecmp(text, "areaaggfile") == 0)
    infile >> filename >> ws;
  else
    handle.Unexpected("areaaggfile", text);

  datafile.open(filename);
  checkIfFailure(datafile, filename);
  handle.Open(filename);
  i = readAggregation(subdata, tmpareas, areaindex);
  handle.Close();
  datafile.close();
  datafile.clear();

  //Check if we read correct input
  if (tmpareas.Nrow() != 1)
    handle.Message("Error - there should be only one aggregated area for stockstdprinter");

  for (i = 0; i < tmpareas.Ncol(0); i++)
    outerareas.resize(1, tmpareas[0][i]);

  //Must change from outer areas to inner areas.
  areas.resize(outerareas.Size());
  for (i = 0; i < areas.Size(); i++)
    if ((areas[i] = Area->InnerArea(outerareas[i])) == -1)
      handle.UndefinedArea(outerareas[i]);

  //Open the printfile
  readWordAndValue(infile, "printfile", filename);
  outfile.open(filename, ios::out);
  checkIfFailure(outfile, filename);

  infile >> text >> ws;
  if (!(strcasecmp(text, "yearsandsteps") == 0))
    handle.Unexpected("yearsandsteps", text);
  if (!aat.ReadFromFile(infile, TimeInfo))
    handle.Message("Error in stockstdprinter - wrong format for yearsandsteps");

  //prepare for next printfile component
  infile >> ws;
  if (!infile.eof()) {
    infile >> text >> ws;
    if (!(strcasecmp(text, "[component]") == 0))
      handle.Unexpected("[component]", text);
  }

  //finished initializing. Now print first lines
  outfile << "; ";
  RUNID.print(outfile);
  outfile << "; Standard output file for the stock " << stockname;
  if (Scale != 1.0)
    outfile << "\n; Scaling factor for the number and number consumed is " << Scale;

  outfile << "\n; year-step-area-age-number-mean length-mean weight-"
    << "stddev length-number consumed-biomass consumed\n";
  outfile.flush();

  //areaindex is not required - free up memory
  for (i = 0; i < areaindex.Size(); i++)
    delete[] areaindex[i];
}

StockStdPrinter::~StockStdPrinter() {
  outfile.close();
  outfile.clear();
  delete preyinfo;
  delete aggregator;
  delete LgrpDiv;
  delete[] stockname;
}

void StockStdPrinter::SetStock(StockPtrVector& stockvec) {
  CharPtrVector stocknames(1, stockname);
  //by using the vector stocknames, some of the code below can be used
  //even if the StockStdPrinter is used for aggregation of many stocks.
  StockPtrVector stocks;
  int index = 0;
  int i, j, tmpage;

  for (i = 0; i < stockvec.Size(); i++)
    for (j = 0; j < stocknames.Size(); j++)
      if (strcasecmp(stockvec[i]->Name(), stocknames[j]) == 0) {
        stocks.resize(1);
        stocks[index++] = stockvec[i];
      }

  if (stocks.Size() != stocknames.Size()) {
    cerr << "Error in printer when searching for stock(s) with name matching:\n";
    for (i = 0; i < stocknames.Size(); i++)
      cerr << (const char*)stocknames[i] << sep;
    cerr << "\nDid only find the stock(s)\n";
    for (i = 0; i < stocks.Size(); i++)
      cerr << (const char*)stocks[i]->Name() << sep;
    cerr << endl;
    exit(EXIT_FAILURE);
  }

  //check that the stock lives in the areas.
  for (i = 0; i < stocks.Size(); i++)
    for (j = 0; j < areas.Size(); j++)
      if (!stocks[i]->IsInArea(areas[j])) {
        cerr << "Error in standard printout for stocks. The stock "
          << stocks[i]->Name() << " is not defined on area "
          << areas[j] << endl;
        exit(EXIT_FAILURE);
      }

  //Prepare for the creation of the aggregator
  minage = 100;
  maxage = 0;
  for (i = 0; i < areas.Size(); i++) {
    tmpage = stocks[0]->Agelengthkeys(areas[i]).Minage();
    if (tmpage < minage)
      minage = tmpage;
    tmpage = stocks[0]->Agelengthkeys(areas[i]).Maxage();
    if (tmpage > maxage)
      maxage = tmpage;
  }

  IntMatrix agematrix(maxage - minage + 1, 1);
  for (i = 0; i < agematrix.Nrow(); i++)
    agematrix[i][0] = i + minage;
  IntMatrix areamatrix(areas.Size(), 1);
  for (i = 0; i < areamatrix.Nrow(); i++)
    areamatrix[i][0] = areas[i];

  LgrpDiv = new LengthGroupDivision(*stocks[0]->ReturnLengthGroupDiv());
  aggregator = new StockAggregator(stocks, LgrpDiv, areamatrix, agematrix);
  //Here comes some code that is only useful when handling one stock.
  if (stocks[0]->IsEaten())
    preyinfo = new StockPreyStdInfo((StockPrey*)stocks[0]->ReturnPrey(), areas);
}

void StockStdPrinter::Print(const TimeClass* const TimeInfo) {
  if (!aat.AtCurrentTime(TimeInfo))
    return;
  aggregator->Sum();
  int a, age;
  double tmpnumber, tmpbiomass;

  for (a = 0; a < areas.Size(); a++) {
    if (preyinfo)
      preyinfo->Sum(TimeInfo, areas[a]);
    const AgeBandMatrix& alk = aggregator->ReturnSum()[a];

    for (age = alk.Minage(); age <= alk.Maxage(); age++) {
      PopStatistics popstat(alk[age], LgrpDiv);
      outfile << setw(smallwidth) << TimeInfo->CurrentYear() << sep
        << setw(smallwidth) << TimeInfo->CurrentStep() << sep
        << setw(smallwidth) << outerareas[a] << sep << setw(smallwidth)
        << age + minage << sep;

        //JMB crude filters to remove the 'silly' values from the output
        if (popstat.TotalNumber() < rathersmall) {
          outfile << setw(fullwidth) << 0 << sep << setw(largewidth) << 0
            << sep << setw(largewidth) << 0 << sep << setw(largewidth) << 0
            << sep << setw(largewidth) << 0 << sep << setw(largewidth) << 0;

        } else {
          outfile << setprecision(largeprecision) << setw(fullwidth)
            << popstat.TotalNumber() / Scale  << sep << setprecision(printprecision)
            << setw(largewidth) << popstat.MeanLength() << sep << setprecision(printprecision)
            << setw(largewidth) << popstat.MeanWeight() << sep << setprecision(printprecision)
            << setw(largewidth) << popstat.StdDevOfLength() << sep;

          if (preyinfo) {
            tmpnumber = preyinfo->NconsumptionByAge(areas[a])[age + minage];
            tmpbiomass = preyinfo->BconsumptionByAge(areas[a])[age + minage];

            if ((tmpnumber < rathersmall) || (tmpbiomass < rathersmall)) {
              outfile << setw(largewidth) << 0 << sep << setw(largewidth) << 0;
            } else {
              outfile << setprecision(smallprecision) << setw(largewidth) << tmpnumber / Scale
                << sep << setprecision(smallprecision) << setw(largewidth) << tmpbiomass;
            }
          } else
            outfile << setw(largewidth) << 0 << sep << setw(largewidth) << 0;

        }
      outfile << endl;
    }
  }
  outfile.flush();
}
