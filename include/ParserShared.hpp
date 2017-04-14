#pragma once

#include <variant>
#include <string>

using std::variant;
using std::string;
using std::get;
using std::shared_ptr;
using std::make_shared;

enum class _TType {
    Int,
    Float,
    Bool,
    Str,
    Void
};

class TType {
    public:
        using IT = std::variant<_TType, string>;

        template<class T> TType(T a) : inner(a) {}
        TType() {}

        static TType withRef(shared_ptr<TType> t) {
            TType res;
            res.referenceTo = t;
            return res;
        }

        bool isRef() {
            return bool(referenceTo);
        }

        operator string() const { return get<string>(inner); }
        operator _TType() const { return get<_TType>(inner); }

        shared_ptr<TType> referenceTo;
        IT inner;
};

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
    Comma,
    Eq,
    Type,
    TypeDef,
    Ref,
    Ret,
    If,
    Else
};
