/* -*- Mode: bison -*-
/*
 *  The scanner definition for FFC.
 */

%{

#include <string>
#include <iostream>

using namespace std;

string string_buf;
string operator_buf;

int curr_lineno = 1;
extern int verbose_flag;
extern "C" int yylex();

#include "tokens.hpp"
YYSTYPE yylval;

extern "C" int optind;

void handle_flags(int argc, char *argv[]);

FILE *fin;

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
    if ((result = fread((char *)buf, 1, max_size, fin)) < 0) { \
        YY_FATAL_ERROR("read() failed in lexer"); \
    }

extern void dump_token(int lineno, int token, YYSTYPE yylval);

int main(int argc, char** argv) {
    int token;

    while (optind < argc) {
        fin = fopen(argv[optind], "r");
        if (fin == NULL) {
            cerr << "Could not open input file " << argv[optind] << endl;
            exit(1);
        }

        curr_lineno = 1;

        //
        // Scan and print all tokens.
        //
        cout << "#name \"" << argv[optind] << "\"" << endl;
        while ((token = yylex()) != 0) {
            dump_token(curr_lineno, token, yylval);
        }
        fclose(fin);
        optind++;
    }
    exit(0);
}

%}
%x IN_STRING

INT [1-9][0-9]*
FLOAT [0-9]+\.[0-9]+
ID [A-z_][A-z0-9_]*
OP [!~@#$%&^*-+\\/<>][!~@#$%&^*-+\\/<>=]*
%%

fnc        return Fnc;
extern     return Extern;
operator   return Operator;
include    return Include;
type       return Type;
ref        return Ref;
val        return Val;
implement  return Implement;
for        return For;
destructor return Destructor;
if         return If;
while      return While;
else       return Else;
ret        return Ret;
generic    return Generic;

{OP} {
    yylval.op = yytext;
    return Operator;
}

= return Eq;
"(" return '(';
")" return ')';
";" return ';';
"," return ',';
"{" return '{';
"}" return '}';

<INITIAL>\" {
    BEGIN(IN_STRING);
}

<IN_STRING><<EOF>> {
    yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    return Error;
}

<IN_STRING>\" {
    yylval.str = string_buf;
    BEGIN(INITIAL);
    return Str;
 }

<IN_STRING>. string_buf += *yytext;

true {
    yylval.bool_val = true;
    return Bool;
}

false {
    yylval.bool_val = false;
    return Bool;
}

{ID} {
    yylval.ident = yytext;
    return Ident;
}

{FLOAT} {
    yylval.float_num = stof(yytext);
    return Float;
}

{INT} {
    yylval.int_num = stoi(yytext);
    return Int;
}


"//".*

[ \t\f\r\v]+
\n curr_lineno++;

. {
    yylval.error_msg = yytext;
    return Error;
}

%%
