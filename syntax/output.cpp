#include <iostream>
#include "tokens.hpp"

using namespace std;

static const char *token_to_string(int tok) {
    switch (tok) {
    case 0: return "EOF";
    case Fnc: return "FNC";
    case Extern: return "EXTERN";
    case Operator: return "op";
    case OperatorKw: return "OPERATOR";
    case Include: return "INCLUDE";
    case Type: return "TYPE";
    case Ref: return "REF";
    case Val: return "VAL";
    case Implement: return "IMPLEMENT";
    case For: return "FOR";
    case Destructor: return "DESTRUCTOR";
    case If: return "IF";
    case While: return "WHILE";
    case Else: return "ELSE";
    case Ret: return "RET";
    case Generic: return "GENERIC";
    case Bool: return "bool";
    case Int: return "int";
    case Str: return "str";
    case Float: return "float";
    case Ident: return "ID";
    case Eq: return "=";

    case '(': return "'('";
    case ')': return "')'";
    case ';': return "';'";
    case ',': return "','";
    case '{': return "'{'";
    case '}': return "'}'";

    default: return "<INVALID TOKEN>";
    }
}

void dump_token(int lineno, int token, YYSTYPE yylval) {
    cout << lineno << " " << token_to_string(token) << " ";

    switch (token) {
    case Str:
        cout << yylval.str;
        break;
    case Int:
        cout << yylval.int_num;
        break;
    case Float:
        cout << yylval.float_num;
        break;
    case Bool:
        cout << (yylval.bool_val ? "true" : "false");
        break;
    case Operator:
        cout << yylval.op;
        break;
    case Ident:
        cout << yylval.ident;
        break;
    case Error:
        cout << yylval.error_msg;
        break;
    }
    cout << '\n';
}
