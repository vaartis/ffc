#pragma once

#include "mpark/variant.hpp"
#include <string>
#include <iostream>

using mpark::variant;
using mpark::holds_alternative;

using std::string;
using std::get;
using std::shared_ptr;
using std::make_shared;
using std::to_string;
using std::cout;
using std::endl;

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
 *  to represent reference and dereference, TType that represents reference/dereference
 *  MUST be allocated on the heap.
 */
class TType {
    public:
        using IT = variant<_TType, string, GenericType>;

        template<class T> TType(T a) : inner(a) {}
        TType() {}

        /** Create a reference type. */
        static TType withRef(TType *t) {
            TType res;
            res.referenceTo = t;
            return res;
        }

        /** Dereference some type. */
        static TType withDeref(TType *t) {
            TType res;
            res.dereferenceTo = t;
            return res;
        }

        /** Check if type is a reference type. */
        bool isRef() {
            if (referenceTo == nullptr) {
                return false;
            } else {
                return bool(referenceTo);
            }
        }

        /** Check if this is a dereferene of some type. */
        bool isDeref() {
            if (dereferenceTo == nullptr) {
                return false;
            } else {
                return bool(dereferenceTo);
            }
        }

        void dump() {
            if (isRef()) {
                cout << "ref " + referenceTo->to_string() << endl;;
            } else if (isDeref()) {
                cout << "val " + dereferenceTo->to_string() << endl;
            } else if (holds_alternative<GenericType>(inner)) {
                cout << "Generic " + get<GenericType>(inner).name << endl;
            } else if (holds_alternative<_TType>(inner)) {
                switch (get<_TType>(inner)) {
                    case _TType::Int:
                        cout << "int" << endl;
                        break;
                    case _TType::Float:
                        cout << "float" << endl;
                        break;
                    case _TType::Str:
                        cout << "str" << endl;
                        break;
                    case _TType::Void:
                        cout << "void" << endl;
                        break;
                    case  _TType::Bool:
                        cout << "bool" << endl;
                        break;
                }
            } else {
                cout << get<string>(inner) << endl;
            }
        }

        string to_string() {
            if (isRef()) {
                return "ref_" + referenceTo->to_string();
            } else if (isDeref()) {
                return "val_" + dereferenceTo->to_string();
            } else if (holds_alternative<GenericType>(inner)) {
                return get<GenericType>(inner).name;
            } else if (holds_alternative<_TType>(inner)) {
                switch (get<_TType>(inner)) {
                    case _TType::Int:
                        return "int";
                    case _TType::Float:
                        return "float";
                    case _TType::Str:
                        return "str";
                    case _TType::Void:
                        return "void";
                    case  _TType::Bool:
                        return "bool";
                }
            } else {
                return get<string>(inner);
            }
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

        bool isCustom() {
            try {
                get<string>(inner);
                return true;
            } catch (mpark::bad_variant_access&) {
                return false;
            }
        }

        /** Type to which this type points to or nullptr. */
        TType *referenceTo = nullptr;

        /** Type to which this type can be dereferenced or nullptr. */
        TType *dereferenceTo = nullptr;

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
