#include "stock.h"
#include "keeper.h"
#include "areatime.h"
#include "naturalm.h"
#include "lennaturalm.h"
#include "grower.h"
#include "stockprey.h"
#include "mortprey.h"
#include "stockpredator.h"
#include "initialcond.h"
#include "migration.h"
#include "readfunc.h"
#include "errorhandler.h"
#include "maturity.h"
#include "renewal.h"
#include "transition.h"
#include "print.h"
#include "spawner.h"
#include "readword.h"
#include "readaggregation.h"
#include "checkconversion.h"
#include "gadget.h"

Stock::Stock(CommentStream& infile, const char* givenname,
  const AreaClass* const Area, const TimeClass* const TimeInfo, Keeper* const keeper)
  : BaseClass(givenname), spawner(0), renewal(0), maturity(0), transition(0),
    migration(0), prey(0), predator(0), initial(0), LgrpDiv(0), grower(0), NatM(0) {

  haslgr = 1; //fixme: read this in from data file?
  int i, tmpint;
  ErrorHandler handle;
  char text[MaxStrLength];
  strncpy(text, "", MaxStrLength);
  char filename[MaxStrLength];
  strncpy(filename, "", MaxStrLength);
  ifstream datafile;
  CommentStream subdata(datafile);

  keeper->SetString(this->Name());

  //Read the area data
  infile >> ws;
  if (infile.eof())
    handle.Eof();
  infile >> text;
  IntVector tmpareas;
  if (strcasecmp(text, "livesonareas") == 0) {
    infile >> ws;
    i = 0;
    while (isdigit(infile.peek()) && !infile.eof() && (i < Area->NoAreas())) {
      tmpareas.resize(1);
      infile >> tmpint >> ws;
      if ((tmpareas[i] = Area->InnerArea(tmpint)) == -1)
        handle.UndefinedArea(tmpint);
      i++;
    }
  } else
    handle.Unexpected("livesonareas", text);

  this->LetLiveOnAreas(tmpareas);

  //Read the stock age and length data
  int minage;
  int maxage;
  double minlength;
  double maxlength;
  double dl;
  readWordAndVariable(infile, "minage", minage);
  readWordAndVariable(infile, "maxage", maxage);
  readWordAndVariable(infile, "minlength", minlength);
  readWordAndVariable(infile, "maxlength", maxlength);
  readWordAndVariable(infile, "dl", dl);

  //JMB need to read the location of the reference weights file
  char refweight[MaxStrLength];
  strncpy(refweight, "", MaxStrLength);
  readWordAndValue(infile, "refweightfile", refweight);

  LgrpDiv = new LengthGroupDivision(minlength, maxlength, dl);
  if (LgrpDiv->Error())
    printLengthGroupError(minlength, maxlength, dl, "length group for stock");

  //JMB need to set the lowerlgrp and size vectors to a default
  //value to allow the whole range of lengths to be calculated
  tmpint = int((maxlength - minlength) / dl);
  IntVector lowerlgrp(maxage - minage + 1, 0);
  IntVector size(maxage - minage + 1, tmpint);

  Alkeys.resize(areas.Size(), minage, lowerlgrp, size);
  tagAlkeys.resize(areas.Size(), minage, lowerlgrp, size);
  for (i = 0; i < Alkeys.Size(); i++)
    Alkeys[i].SettoZero();

  //Read the growth length group data
  DoubleVector grlengths;
  CharPtrVector grlenindex;

  readWordAndValue(infile, "growthandeatlengths", filename);
  datafile.open(filename);
  checkIfFailure(datafile, filename);
  handle.Open(filename);
  i = readLengthAggregation(subdata, grlengths, grlenindex);
  handle.Close();
  datafile.close();
  datafile.clear();

  LengthGroupDivision* GrowLgrpDiv = new LengthGroupDivision(grlengths);
  if (GrowLgrpDiv->Error())
    printLengthGroupError(grlengths, "growthandeatlengths for stock");

  checkLengthGroupIsFiner(LgrpDiv, GrowLgrpDiv, this->Name(), "growth and eat lengths");

  //Read the growth function data
  readWordAndVariable(infile, "doesgrow", doesgrow);
  if (doesgrow)
    grower = new Grower(infile, LgrpDiv, GrowLgrpDiv, areas, TimeInfo, keeper, refweight, Area, grlenindex);
  else
    grower = 0;

  //Read the natural mortality data
  infile >> text;
  if (strcasecmp(text, "naturalmortality") == 0)
    NatM = new NaturalM(infile, minage, maxage, TimeInfo, keeper);
  else
    handle.Unexpected("naturalmortality", text);

  //Read the prey data
  readWordAndVariable(infile, "iseaten", iseaten);
  if (iseaten)
    prey = new StockPrey(infile, areas, this->Name(), minage, maxage, keeper);
  else
    prey = 0;

  //Read the predator data
  readWordAndVariable(infile, "doeseat", doeseat);
  if (doeseat)
    predator = new StockPredator(infile, this->Name(), areas, LgrpDiv,
      GrowLgrpDiv, minage, maxage, TimeInfo, keeper);
  else
    predator = 0;

  //Read the initial conditions
  infile >> text;
  if (strcasecmp(text, "initialconditions") == 0)
    initial = new InitialCond(infile, areas, keeper, refweight, Area);
  else
    handle.Unexpected("initialconditions", text);

  //Read the migration data
  readWordAndVariable(infile, "doesmigrate", doesmigrate);
  if (doesmigrate) {
    readWordAndVariable(infile, "agedependentmigration", AgeDepMigration);
    readWordAndValue(infile, "migrationfile", filename);
    ifstream subfile(filename);
    CommentStream subcomment(subfile);
    checkIfFailure(subfile, filename);
    handle.Open(filename);
    migration = new Migration(subcomment, AgeDepMigration, areas, Area, TimeInfo, keeper);
    handle.Close();
    subfile.close();
    subfile.clear();

  } else
    migration = 0;

  //Read the maturation data
  readWordAndVariable(infile, "doesmature", doesmature);
  if (doesmature) {
    readWordAndValue(infile, "maturityfunction", text);
    readWordAndValue(infile, "maturityfile", filename);
    ifstream subfile(filename);
    CommentStream subcomment(subfile);
    checkIfFailure(subfile, filename);
    handle.Open(filename);

    if (strcasecmp(text, "continuous") == 0)
      maturity = new MaturityA(subcomment, TimeInfo, keeper, minage, lowerlgrp, size, areas, LgrpDiv, 3);
    else if (strcasecmp(text, "fixedlength") == 0)
      maturity = new MaturityB(subcomment, TimeInfo, keeper, minage, lowerlgrp, size, areas, LgrpDiv);
    else if (strcasecmp(text, "ageandlength") == 0)
      maturity = new MaturityC(subcomment, TimeInfo, keeper, minage, lowerlgrp, size, areas, LgrpDiv, 4);
    else if (strcasecmp(text, "constant") == 0)
      maturity = new MaturityD(subcomment, TimeInfo, keeper, minage, lowerlgrp, size, areas, LgrpDiv, 4);
    else
      handle.Message("Error in stock file - unrecognised maturity", text);

    handle.Close();
    subfile.close();
    subfile.clear();

    if (!doesgrow)
      handle.Warning("The stock does not grow, so it is unlikely to mature!");

  } else
    maturity = 0;

  //Read the movement data
  readWordAndVariable(infile, "doesmove", doesmove);
  if (doesmove) {
    //transition handles the movements of the age group maxage:
    transition = new Transition(infile, areas, maxage, LgrpDiv, keeper);

  } else
    transition = 0;

  //Read the renewal data
  readWordAndVariable(infile, "doesrenew", doesrenew);
  if (doesrenew) {
    readWordAndValue(infile, "renewaldatafile", filename);
    ifstream subfile(filename);
    CommentStream subcomment(subfile);
    checkIfFailure(subfile, filename);
    handle.Open(filename);
    renewal = new RenewalData(subcomment, areas, Area, TimeInfo, keeper);
    handle.Close();
    subfile.close();
    subfile.clear();

  } else
    renewal = 0;

  //Read the spawning data
  readWordAndVariable(infile, "doesspawn", doesspawn);
  if (doesspawn) {
    spawner = new Spawner(infile, maxage, LgrpDiv->NoLengthGroups(), Area, TimeInfo, keeper);

  } else
    spawner = 0;

  //Set the birthday for the stock
  birthdate = TimeInfo->StepsInYear();

  //Finished reading from infile - resize objects and clean up
  NumberInArea.AddRows(areas.Size(), LgrpDiv->NoLengthGroups());
  delete GrowLgrpDiv;
  for (i = 0; i < grlenindex.Size(); i++)
    delete[] grlenindex[i];
  keeper->ClearAll();
}

Stock::Stock(const char* givenname)
  : BaseClass(givenname), spawner(0), renewal(0), maturity(0), transition(0),
    migration(0), prey(0), predator(0), initial(0), LgrpDiv(0), grower(0), NatM(0) {

}

Stock::~Stock() {
  if (migration != 0)
    delete migration;
  if (prey != 0)
    delete prey;
  if (predator != 0)
    delete predator;
  if (initial != 0)
    delete initial;
  if (LgrpDiv != 0)
    delete LgrpDiv;
  if (grower != 0)
    delete grower;
  if (NatM != 0)
    delete NatM;
  if (transition != 0)
    delete transition;
  if (renewal != 0)
    delete renewal;
  if (maturity != 0)
    delete maturity;
  if (spawner != 0)
    delete spawner;
}

void Stock::Reset(const TimeClass* const TimeInfo) {
  //What is only done in the beginning.
  //Naturalm, maturity and growth will later be moved.

  if (TimeInfo->CurrentTime() == 1) {
    this->Clear();
    initial->Initialize(Alkeys);
    if (iseaten)
      prey->Reset();
    if (doesmature)
      maturity->Precalc(TimeInfo);
    if (doesrenew)
      renewal->Reset();
    if (doesgrow)
      grower->Reset();
    if (doesmigrate)
      migration->Reset();
  }
  NatM->Reset(TimeInfo);
  if (doeseat)
    predator->Reset(TimeInfo);
}

void Stock::Clear() {
  if (doesmigrate)
    migration->Clear();
}

Prey* Stock::ReturnPrey() const {
  return prey;
}

PopPredator* Stock::ReturnPredator() const {
  return predator;
}

void Stock::SetStock(StockPtrVector& stockvec) {
  if (doesmature)
    maturity->SetStock(stockvec);
  if (doesmove)
    transition->SetStock(stockvec);
}

void Stock::SetCI() {
  initial->SetCI(LgrpDiv);
  if (iseaten)
    prey->SetCI(LgrpDiv);
  if (doesrenew)
    renewal->SetCI(LgrpDiv);
}

void Stock::Print(ofstream& outfile) const {
  int i;

  outfile << "\nStock\nName" << sep << this->Name() << "\niseaten" << sep
    << iseaten << "\ndoeseat" << sep << doeseat << "\ndoesmove" << sep
    << doesmove << "\ndoesspawn" << sep << doesspawn << "\ndoesmature" << sep
    << doesmature << "\ndoesrenew" << sep << doesrenew << "\ndoesgrow" << sep
    << doesgrow << "\ndoesmigrate" << sep << doesmigrate << "\nInner areas";

  for (i = 0; i < areas.Size(); i++)
    outfile << sep << areas[i];
  outfile << endl;

  initial->Print(outfile);
  NatM->Print(outfile);
  if (doesmature)
    maturity->Print(outfile);
  if (iseaten)
    prey->Print(outfile);
  if (doeseat)
    predator->Print(outfile);
  if (doesmove)
    transition->Print(outfile);
  if (doesrenew)
    renewal->Print(outfile);
  if (doesgrow)
    grower->Print(outfile);
  if (doesmigrate)
    migration->Print(outfile);

  outfile << "\nAgelength keys\nCurrent status\n";
  for (i = 0; i < areas.Size(); i++) {
    outfile << "Inner Area " << areas[i] << "\nNumbers\n";
    Printagebandm(outfile, Alkeys[i]);
    outfile << "Mean weights\n";
    PrintWeightinagebandm(outfile, Alkeys[i]);
  }
}

int Stock::Birthday(const TimeClass* const TimeInfo) const {
  return (TimeInfo->CurrentStep() == birthdate);
}

const AgeBandMatrix& Stock::Agelengthkeys(int area) const {
  return Alkeys[AreaNr[area]];
}

const AgeBandMatrix& Stock::getMeanN(int area) const {
  assert(iseaten && prey->preyType() == MORTPREYTYPE);
  return (((MortPrey*)prey)->getMeanN(area));
}

const StockPtrVector& Stock::GetMatureStocks() {
  assert(doesmature != 0);
  return maturity->GetMatureStocks();
}
