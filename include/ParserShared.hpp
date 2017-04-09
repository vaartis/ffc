#pragma once

#include <variant>
#include <string>

using std::variant;
using std::string;

enum class _TType {
    Int,
    Float,
    Bool,
    Str,
    Void
};

typedef std::variant<_TType, string> TType;

enum class Token {
    Operator,
    Include,
    OperatorDef,
    None, // Initial
    Fnc,
    While,
    Extern,
    Ident,
    IntLit,
    FloatLit,
    BoolLit,
    StrLit,
    OpP,
    ClP,
    OpCB,
    ClCB,
    Semicolon,
    Dot,
    Eq,
    Type,
    TypeDef,
    Ret,
    If,
    Else
};
