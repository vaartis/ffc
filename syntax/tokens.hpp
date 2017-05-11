#pragma once

#include <string>

enum TokType {
    Fnc = 1,
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

typedef struct {
    std::string ident;
    std::string str;
    int int_num;
    float float_num;
    bool bool_val;
    std::string op;
    std::string error_msg;
} YYSTYPE;
