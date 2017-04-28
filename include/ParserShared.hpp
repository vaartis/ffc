#pragma once

#include "variant"
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

/** Represents type of a value.
 *
 *  Type can build some of built-in type, like int and bool, or a custom
 *  type, which is represented as a string and checked at the code generation step,
 *  this type is just a proxy type for a variant, it also provides a functions
 *  to represent reference and dereference
 */
class TType {
    public:
        using IT = std::variant<_TType, string>;

        template<class T> TType(T a) : inner(a) {}
        TType() {}

        /** Create a reference type. */
        static TType withRef(shared_ptr<TType> t) {
            TType res;
            res.referenceTo = t;
            return res;
        }

        /** Dereference some type. */
        static TType withDeref(shared_ptr<TType> t) {
            TType res;
            res.dereferenceTo = t;
            return res;
        }

        /** Check if type is a reference type. */
        bool isRef() {
            return bool(referenceTo);
        }

        /** Check if this is a dereferene of some type. */
        bool isDeref() {
            return bool(dereferenceTo);
        }

        operator string() const { return get<string>(inner); }
        operator _TType() const { return get<_TType>(inner); }

        /** Type to which this type points to or nullptr. */
        shared_ptr<TType> referenceTo;

        /** Type to which this type can be dereferenced or nullptr. */
        shared_ptr<TType> dereferenceTo;

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
    OpP, // (
    ClP, // )
    OpCB, // {
    ClCB, // }
    Semicolon,
    Dot,
    Comma,
    Eq, // =
    Type,
    TypeDef,
    Ref,
    Val,
    Ret,
    If,
    Else,
    Implement,
    For,
    Destructor,
};
