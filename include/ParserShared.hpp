#pragma once

#include "mpark/variant.hpp"
#include <string>

using mpark::variant;
using std::string;
using std::get;
using std::shared_ptr;
using std::make_shared;
using std::to_string;

enum class _TType {
    Int,
    Float,
    Bool,
    Str,
    Void
};

/** Representation of a generic type
 */
class GenericType {
    public:
        GenericType(string nm) : name(nm) {}
        string name;
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
        using IT = variant<_TType, string, GenericType>;

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
        operator GenericType() const { return get<GenericType>(inner); }

        friend bool operator ==(TType &t, string &s) {
            try {
                return get<string>(t.inner) == s;
            } catch (mpark::bad_variant_access&) {
                return false;
            }
        }

        friend bool operator ==(TType &t, GenericType &s) {
            try {
                return get<GenericType>(t.inner).name == s.name;
            } catch (mpark::bad_variant_access&) {
                return false;
            }
        }

        bool isGeneric() {
            try {
                get<GenericType>(inner);
                return true;
            } catch (mpark::bad_variant_access&) {
                return false;
            }
        }

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
    Generic,
    Ref,
    Val,
    Ret,
    If,
    Else,
    Implement,
    For,
    Destructor,
    Eof,
};
