#ifndef readword_h
#define readword_h

#include "commentstream.h"

//functions to read a variable name and then the numeric value
void readWordAndVariable(CommentStream& infile, const char* str, int& number);
void readWordAndVariable(CommentStream& infile, const char* str, double& number);

//JMB - added function to read a string value as well
void readWordAndValue(CommentStream& infile, const char* str, char* value);

//JMB - also added functions to read 2 numbers
void readWordAndTwoVariables(CommentStream& infile, const char* str, int& number1, int& number2);
void readWordAndTwoVariables(CommentStream& infile, const char* str, double& number1, double& number2);

#endif
