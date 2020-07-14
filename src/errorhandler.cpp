#include "errorhandler.h"
#include "gadget.h"
#include "global.h"
#ifndef GADGET_NETWORK
//JMB dont access runid on a network run
#include "runid.h"
#endif

#include <Rcpp.h>

ErrorHandler::ErrorHandler() {
  files = new StrStack();
  uselog = 0;
  numwarn = 0;
  runopt = 0;
  nanflag = 0;
  loglevel = LOGINFO;
}

ErrorHandler::~ErrorHandler() {
  delete files;
}

void ErrorHandler::setLogLevel(int level) {
  switch (level) {
    case 0:
      //no messages displayed at all - only used for paramin runs
      loglevel = LOGNONE;
      break;
    case 1:
      //only get the failure messages written to Rcpp::Rcerr
      //gadget will exit with Rcpp::stop(REXIT_FAILURE) once it receives a message here
      loglevel = LOGFAIL;
      break;
    case 2:
      //also get the information messages written to Rcpp::Rcout
      loglevel = LOGINFO;
      break;
    case 3:
      //also get the warning messages written to Rcpp::Rcerr
      loglevel = LOGWARN;
      break;
    case 4:
      //also get debug messages to be written to the logfile (if it exists)
      loglevel = LOGDEBUG;
      break;
    case 5:
      //also get information messages to be written to the logfile (if it exists)
      loglevel = LOGMESSAGE;
      break;
    case 6:
      //also get more detailed messages to be written to the logfile (if it exists)
      loglevel = LOGDETAIL;
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << level << endl;
      break;
  }
}

void ErrorHandler::setLogFile(const char* filename) {
  uselog = 1;
  logfile.open(filename, ios::binary);
  this->checkIfFailure(logfile, filename);
#ifndef GADGET_NETWORK
  RUNID.Print(logfile);
#endif
  logfile << "Log file to record Gadget actions that take place during this run\n\n";
  logfile.flush();
}

void ErrorHandler::Open(const char* filename) {
  this->logMessage(LOGMESSAGE, "Opening file", filename);
  files->storeString(filename);
}

void ErrorHandler::Close() {
  if (loglevel >= LOGMESSAGE) {
    char* strFilename = files->sendTop();
    this->logMessage(LOGMESSAGE, "Closing file", strFilename);
    delete[] strFilename;
  }
  files->clearString();
}

void ErrorHandler::logMessage(LogLevel mlevel, const char* msg) {
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << msg << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << msg << endl;
        logfile.flush();
      }
      Rcpp::Rcout << msg << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << msg << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << msg << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessage(LogLevel mlevel, const char* msg1, const char* msg2) {
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg1 << sep << msg2 << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcout << msg1 << sep << msg2 << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg1 << sep << msg2 << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessage(LogLevel mlevel, const char* msg, int number) {
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg << sep << number << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      Rcpp::Rcout << msg << sep << number << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg << sep << number << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessage(LogLevel mlevel, const char* msg, double number) {
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg << sep << number << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      Rcpp::Rcout << msg << sep << number << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg << sep << number << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << msg << sep << number << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessage(LogLevel mlevel, const char* msg1, int number, const char* msg2) {
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg1 << sep << number << sep << msg2 << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcout << msg1 << sep << number << sep << msg2 << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg1 << sep << number << sep << msg2 << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessage(LogLevel mlevel, const char* msg1, double number, const char* msg2) {
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg1 << sep << number << sep << msg2 << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcout << msg1 << sep << number << sep << msg2 << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << msg1 << sep << number << sep << msg2 << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << msg1 << sep << number << sep << msg2 << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessage(LogLevel mlevel, DoubleVector vec) {
  if (mlevel > loglevel)
    return;

  int i;
  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        for (i = 0; i < vec.Size(); i++)
          logfile << vec[i] << sep;
        logfile << endl;
        logfile.flush();
      }
      for (i = 0; i < vec.Size(); i++)
        Rcpp::Rcerr << vec[i] << sep;
      Rcpp::Rcerr << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        for (i = 0; i < vec.Size(); i++)
          logfile << vec[i] << sep;
        logfile << endl;
        logfile.flush();
      }
      for (i = 0; i < vec.Size(); i++)
        Rcpp::Rcout << vec[i] << sep;
      Rcpp::Rcout << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        for (i = 0; i < vec.Size(); i++)
          logfile << vec[i] << sep;
        logfile << endl;
        logfile.flush();
      }
      for (i = 0; i < vec.Size(); i++)
        Rcpp::Rcerr << vec[i] << sep;
      Rcpp::Rcerr << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        for (i = 0; i < vec.Size(); i++)
          logfile << vec[i] << sep;
        logfile << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logMessageNaN(LogLevel mlevel, const char* msg) {

  nanflag = 1;
  if (mlevel > loglevel)
    return;

  switch (mlevel) {
    case LOGNONE:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << "Error in model - NaN found" << sep << msg << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << "Error in model - NaN found" << sep << msg << endl;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGINFO:
      if (uselog) {
        logfile << "Error in model - NaN found" << sep << msg << endl;
        logfile.flush();
      }
      Rcpp::Rcout << "Error in model - NaN found" << sep << msg << endl;
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << "Error in model - NaN found" << sep << msg << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << "Error in model - NaN found" << sep << msg << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Error in model - NaN found" << sep << msg << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
}

void ErrorHandler::logFileMessage(LogLevel mlevel, const char* msg) {
  if (mlevel > loglevel)
    return;

  char* strFilename = files->sendTop();
  switch (mlevel) {
    case LOGNONE:
    case LOGINFO:
      break;
    case LOGFAIL:
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Error on commandline - " << msg << endl;
        else
          logfile << "Error in file " << strFilename << " - " << msg << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Error on commandline - " << msg << endl;
      else
        Rcpp::Rcerr << "Error in file " << strFilename << " - " << msg << endl;
      delete[] strFilename;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Warning on commandline - " << msg << endl;
        else
          logfile << "Warning in file " << strFilename << " - " << msg << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Warning on commandline - " << msg << endl;
      else
        Rcpp::Rcerr << "Warning in file " << strFilename << " - " << msg << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Message in file " << strFilename << " - " << msg << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
  delete[] strFilename;
}


void ErrorHandler::logFileMessage(LogLevel mlevel, const char* msg, int number) {
  if (mlevel > loglevel)
    return;

  char* strFilename = files->sendTop();
  switch (mlevel) {
    case LOGNONE:
    case LOGINFO:
      break;
    case LOGFAIL:
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Error on commandline - " << msg << sep << number << endl;
        else
          logfile << "Error in file " << strFilename << " - " << msg << sep << number << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Error on commandline - " << msg << sep << number << endl;
      else
        Rcpp::Rcerr << "Error in file " << strFilename << " - " << msg << sep << number << endl;
      delete[] strFilename;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Warning on commandline - " << msg << sep << number << endl;
        else
          logfile << "Warning in file " << strFilename << " - " << msg << sep << number << endl;
        logfile.flush();
      }
     if (files->getSize() == 0)
        Rcpp::Rcerr << "Warning on commandline - " << msg << sep << number << endl;
      else
        Rcpp::Rcerr << "Warning in file " << strFilename << " - " << msg << sep << number << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Message in file " << strFilename << " - " << msg << sep << number << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
  delete[] strFilename;
}

void ErrorHandler::logFileMessage(LogLevel mlevel, const char* msg, double number) {
  if (mlevel > loglevel)
    return;

  char* strFilename = files->sendTop();
  switch (mlevel) {
    case LOGNONE:
    case LOGINFO:
      break;
    case LOGFAIL:
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Error on commandline - " << msg << sep << number << endl;
        else
          logfile << "Error in file " << strFilename << " - " << msg << sep << number << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Error on commandline - " << msg << sep << number << endl;
      else
        Rcpp::Rcerr << "Error in file " << strFilename << " - " << msg << sep << number << endl;
      delete[] strFilename;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Warning on commandline - " << msg << sep << number << endl;
        else
          logfile << "Warning in file " << strFilename << " - " << msg << sep << number << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Warning on commandline - " << msg << sep << number << endl;
      else
        Rcpp::Rcerr << "Warning in file " << strFilename << " - " << msg << sep << number << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Message in file " << strFilename << " - " << msg << sep << number << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
  delete[] strFilename;
}

void ErrorHandler::logFileMessage(LogLevel mlevel, const char* msg1, const char* msg2) {
  if (mlevel > loglevel)
    return;

  char* strFilename = files->sendTop();
  switch (mlevel) {
    case LOGNONE:
    case LOGINFO:
      break;
    case LOGFAIL:
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Error on commandline - " << msg1 << sep << msg2 << endl;
        else
          logfile << "Error in file " << strFilename << " - " << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Error on commandline - " << msg1 << sep << msg2 << endl;
      else
        Rcpp::Rcerr << "Error in file " << strFilename << " - " << msg1 << sep << msg2 << endl;
      delete[] strFilename;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        if (files->getSize() == 0)
          logfile << "Warning on commandline - " << msg1 << sep << msg2 << endl;
        else
          logfile << "Warning in file " << strFilename << " - " << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      if (files->getSize() == 0)
        Rcpp::Rcerr << "Warning on commandline - " << msg1 << sep << msg2 << endl;
      else
        Rcpp::Rcerr << "Warning in file " << strFilename << " - " << msg1 << sep << msg2 << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Message in file " << strFilename << " - " << msg1 << sep << msg2 << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
  delete[] strFilename;
}

void ErrorHandler::logFileEOFMessage(LogLevel mlevel) {
  if (mlevel > loglevel)
    return;

  char* strFilename = files->sendTop();
  switch (mlevel) {
    case LOGNONE:
    case LOGINFO:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << "Unexpected end of file " << strFilename << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << "Unexpected end of file " << strFilename << endl;
      delete[] strFilename;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << "Unexpected end of file " << strFilename << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << "Unexpected end of file " << strFilename << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Unexpected end of file " << strFilename << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
  delete[] strFilename;
}

void ErrorHandler::logFileUnexpected(LogLevel mlevel, const char* msg1, const char* msg2) {
  if (mlevel > loglevel)
    return;

  char* strFilename = files->sendTop();
  switch (mlevel) {
    case LOGNONE:
    case LOGINFO:
      break;
    case LOGFAIL:
      if (uselog) {
        logfile << "Error in file " << strFilename << endl
          << "Expected " << msg1 << " but found instead " << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << "Error in file " << strFilename << endl
        << "Expected " << msg1 << " but found instead " << msg2 << endl;
      delete[] strFilename;
      Rcpp::stop(REXIT_FAILURE);
      break;
    case LOGWARN:
      numwarn++;
      if (uselog) {
        logfile << "Warning in file " << strFilename << endl
          << "Expected " << msg1 << " but found instead " << msg2 << endl;
        logfile.flush();
      }
      Rcpp::Rcerr << "Warning in file " << strFilename << endl
        << "Expected " << msg1 << " but found instead " << msg2 << endl;
      break;
    case LOGDEBUG:
    case LOGMESSAGE:
    case LOGDETAIL:
      if (uselog) {
        logfile << "Message in file " << strFilename << endl
          << "Expected " << msg1 << " but found instead " << msg2 << endl;
        logfile.flush();
      }
      break;
    default:
      Rcpp::Rcerr << "Error in errorhandler - invalid log level " << mlevel << endl;
      break;
  }
  delete[] strFilename;
}

void ErrorHandler::checkIfFailure(ios& infile, const char* text) {
  if (infile.fail()) {
    if ((uselog) && (loglevel >= LOGMESSAGE)) {
      logfile << "Checking to see if file " << text << " can be opened ... failed" << endl;
      logfile.flush();
    }
    this->logFileMessage(LOGFAIL, "failed to open datafile", text);
  }

  if ((uselog) && (loglevel >= LOGMESSAGE)) {
    logfile << "Checking to see if file " << text << " can be opened ... OK" << endl;
    logfile.flush();
  }
}

void ErrorHandler::logFinish() {
  if (numwarn > 0)
    this->logMessage(LOGINFO, "\nTotal number of warnings was", numwarn);

  if (uselog) {
    if (runopt)
      logfile << "\nGadget optimisation finished OK - runtime was ";
    else
      logfile << "\nGadget simulation finished OK - runtime was ";
    logfile.flush();
#ifndef GADGET_NETWORK
    RUNID.printTime(logfile);
#endif
    logfile.close();
    logfile.clear();
    uselog = 0;
  }

  if (loglevel >= LOGINFO) {
    if (runopt) {
      Rcpp::Rcout << "\nGadget optimisation finished OK - runtime was ";
#ifndef GADGET_NETWORK
      RUNID.printTime(Rcpp::Rcout);
#endif
    } else
      Rcpp::Rcout << "\nGadget simulation finished OK\n";
    Rcpp::Rcout << endl;
  }
}
