/*
 *  The scanner definition for FFC.
 */

%{

#include <string>

std::string string_buf;
std::string operator_buf;

int curr_lineno = 1;
extern int verbose_flag;
extern "C" int yylex();

typedef struct {
    std::string ident;
    std::string str;
    int int_num;
    float float_num;
    bool bool_val;
    std::string op;
    std::string error_msg;
} YYSTYPE;

YYSTYPE yylval;

enum TokType {
    Fnc,
    Extern,
    Operator,
    Include,
    Type,
    Ref,
    Val,
    Implement,
    For,
    Destructor,
    If,
    While,
    Else,
    Ret,
    Generic,
    Error,
    Bool,
    Int,
    Str,
    Float,
    Ident,
    Eq
};

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
    yylval.float_num = std::stof(yytext);
    return Float;
}

{INT} {
    yylval.int_num = std::stoi(yytext);
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
