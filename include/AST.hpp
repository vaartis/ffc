#pragma once

#include <string>
#include <map>
#include <vector>
#include <deque>
#include <iostream>
#include <optional>

using std::string;
using std::unique_ptr;
using std::map;
using std::vector;
using std::deque;
using std::pair;
using std::ostream;
using std::cout;
using std::endl;
using std::optional;
using std::nullopt;

static unsigned int OFFSET = 0;
static bool WAS_O = false;

/** Offset-aware text printer.
 *
 * This class prints text to stdout and exists just for debugging purposes
 * It prints text with an offset, which should be set in AST node's BaseAST::dump() method
 */
class Print {
    public:
        static void print() {
            WAS_O = false;
            cout << endl;
        }

        template<typename First, typename ...Other> static void print(First const &f, Other const &...val) {
            if (!WAS_O) {
                WAS_O = true;
                cout << string(OFFSET, ' ');
            }
            cout << f;
            print(val...);
        }
};

/** Base class for AST nodes.
*/
class BaseAST {
    public:
        /** Dump element to stdout.
         */
        virtual void dump() = 0;
};

/** Represents a block that can hold a value. */
class Block {
    public:
        /** True, if this block returns some value */
        virtual bool hasValue() = 0;
};

/** Represents some expression with a type */
class Expression {
    public:
        TType expression_type = _TType::Void;
};

/** Represents, basically, a variable and it's type */
class TypedName {
    public:
        TypedName(string n, TType tp) : name(n), type(tp) {}
        string name;
        TType type;
};

/** While loop AST node. */
class WhileAST : public BaseAST {
    public:
        WhileAST(shared_ptr<BaseAST> c, /**< Condition */
                 vector<shared_ptr<BaseAST>> b /**< While loop body */
                 ) : cond(c), body(b)  {}
        shared_ptr<BaseAST> cond;
        vector<shared_ptr<BaseAST>> body;

        void dump() {
            Print::print("While (");

            OFFSET++;

            cond->dump();

            OFFSET--;

            Print::print(")");

            OFFSET++;

            for (auto &el : body) {
                Print::print();
                el->dump();
            }

            OFFSET--;
        }
};

/** Type definition AST node. */
class TypeDefAST : public BaseAST {
    public:
        TypeDefAST(string nm, /**< Type name */
                   vector<pair<string, TType>> vals /**< Type fields, vector is used because order of the arguments is important */
                   ) : name(nm), fields(vals) {}
        string name;
        vector<pair<string, TType>> fields;

        void dump() {
            Print::print("TypeDef", name, " {");

            OFFSET++;

            for (auto &f : fields) {
                Print::print(f.first, " <TType>");
            }

            OFFSET--;
        }
};

/** AST node that represents storing a value into type's field. */
class TypeFieldStoreAST : public BaseAST {
    public:
        TypeFieldStoreAST(string s_n, /**< Instance name */
                          string f_n, /**< Field name*/
                          shared_ptr<BaseAST> v /**< Value */
                          ) : struct_name(s_n), field_name(f_n), value(v) {}
        string struct_name;
        string field_name;
        shared_ptr<BaseAST> value;

        void dump() {
            Print::print("TypeFieldStore (", struct_name, ".", field_name, ") = ");

            OFFSET++;

            value->dump();

            OFFSET--;
        }
};

/** AST node that represents loading a value from type's field. */
class TypeFieldLoadAST : public BaseAST, public Expression {
    public:
        TypeFieldLoadAST(shared_ptr<BaseAST> fr, /**< Instance name */
                         string f_n /**< Field name */
            ) : from(fr), field_name(f_n) {}
        shared_ptr<BaseAST> from;
        string field_name;

        void dump() {
            Print::print("TypeFieldLoad (", expression_type.to_string(), ".", field_name, ")");
        }
};

/** This node represents in-place creation of a type instance */
class TypeAST : public BaseAST, public Expression {
    public:
        TypeAST(string nm, /**< Type name */
                map<string, shared_ptr<BaseAST>> flds /**< Names of fields and their values, order is not important here */
                ) : name(nm), fields(flds) {}
        string name;
        map<string, shared_ptr<BaseAST>> fields;

        void dump() {
            Print::print("Type (", name, ") {");

            OFFSET++;

            for (auto &el : fields) {
                Print::print(el.first, " (");

                OFFSET++;

                el.second->dump();

                OFFSET--;

                Print::print(")");
            }

            OFFSET--;
            Print::print("}");
        }
};

/** This node represents one or more includes. */
class IncludeAST : public BaseAST {
    public:
        IncludeAST(vector<string> mod /**< List of includes */
                   ) : modules(mod) {}
        vector<string> modules;

        void dump() {
            Print::print("Include (");

            OFFSET++;

            for (string m : modules) {
                Print::print(m);
            }

            OFFSET--;

            Print::print(")");
        }
};

/** Represents a call to some function.
 *
 * Deque is needed for an implicit type parameter
 */
class FncCallAST : public BaseAST, public Expression {
    public:
        FncCallAST(string nm, /**< Function name */
                   deque<shared_ptr<BaseAST>> ar, /**< Function arguments */
                   char tt,  /**< Function or operator */
                   optional<string> t = nullopt /**< Type, to which this function belongs or an empty string*/
            ) : name(nm), args(ar), type(t), f_or_op(tt) {}

        string name;
        deque<shared_ptr<BaseAST>> args;
        optional<string> type;
        char f_or_op;

        bool operator ==(FncCallAST &other) {
            return (name == other.name) && (args == other.args) && (type == other.type);
        }

        void dump() {
            Print::print("FncCall (", name, ")", "(");

            OFFSET++;

            for (auto &ar : args) {
                ar->dump();
            }

            OFFSET--;

            Print::print(")");
        }
};

/** AST node that represents usage of an operator */
class OperatorAST : public FncCallAST {
    public:
        OperatorAST(string nm, /**< Operator name, e.g. `+-` */
                    shared_ptr<BaseAST> l, /**< Value of LHS */
                    shared_ptr<BaseAST> r /**< Value of RHS */
            ) : FncCallAST(nm, deque<shared_ptr<BaseAST>>{l,r}, 'O', nullopt) {}
};

/** Node that represents if (and optionally else) branch.
 *
 * Note, that `if` is an expression and therefore can return something,
 * compiler checks if it does and forces users to write an else branch.
 * Value can be returned by ommiting the semicolon after the last expression in if/else block
 */
class IfAST : public BaseAST, public Block, public Expression {
    public:
        IfAST(shared_ptr<BaseAST> c, /**< Condition */
              vector<shared_ptr<BaseAST>> bd, /**< `If` branch body */
              vector<shared_ptr<BaseAST>> el, /**< `Else` branch body*/
              shared_ptr<BaseAST> v, /**< Value of a branch if there is one or nullptr otherwise */
              shared_ptr<BaseAST> ev /**< Likewise, but for `else`*/
              ) : cond(c), body(bd), else_body(el), value(v), else_value(ev) {}

        shared_ptr<BaseAST> cond;
        vector<shared_ptr<BaseAST>> body;
        vector<shared_ptr<BaseAST>> else_body;
        shared_ptr<BaseAST> value;
        shared_ptr<BaseAST> else_value;

        bool hasValue() {
            return value && else_value;
        }

        void dump() {
            Print::print("If (");

            OFFSET++;

            cond->dump();

            OFFSET--;

            Print::print(")");
            Print::print("IfBody {");

            OFFSET++;

            for (auto &b : body) {
                b->dump();
            }

            OFFSET--;

            Print::print("}");
            Print::print("ElseBody {");

            OFFSET++;

            for (auto &b : else_body) {
                b->dump();
            }

            OFFSET--;

            Print::print("}");
        }
};

/** Integer literal. */
class IntAST : public BaseAST, public Expression {
    public:
        IntAST(int i) : value(i) {}

        int value;

        void dump() {
            Print::print(value);
        }
};

/** Float literal. */
class FloatAST : public BaseAST, public Expression {
    public:
        FloatAST(float f) : value(f) {}

        float value;

         void dump() {
            Print::print(value);
        }
};

/** String literal. */
class StrAST : public BaseAST, public Expression {
    public:
        StrAST(string s) : value(s) {}

        string value;

        void dump() {
            Print::print(value);
        }
};

/** Boolean literal. */
class BoolAST : public BaseAST, public Expression {
    public:
        BoolAST(bool b) : value(b) {}

        bool value;

        void dump() {
            Print::print(value);
        }
};

/** Represents reference to some value. */
class RefToValAST : public BaseAST, public Expression {
    public:
        RefToValAST(shared_ptr<BaseAST> v) : value(v) {}
        shared_ptr<BaseAST> value;

        void dump() {
            Print::print("RefToVal (");
            value->dump();
            Print::print(")");
        }
};

/** Represents dereference. */
class ValOfRefAST : public BaseAST, public Expression {
    public:
        ValOfRefAST(shared_ptr<BaseAST> v) : value(v) {}
        shared_ptr<BaseAST> value;

        void dump() {
            Print::print("ValOfRef (");
            value->dump();
            Print::print(")");
        }
};

/** Represents definition of some external function. */
class ExternFncAST : public BaseAST {
    public:
        ExternFncAST(string s, /**< Function name */
                     vector<TType> ar, /**< Function arguments' types */
                     TType r /**< Return type*/
                     ) : name(s), args(ar), ret_type(r) {}

        string name;
        vector<TType> args;
        TType ret_type;

        void dump() {
            Print::print("ExternFnc (", name, ")");
        }
};

/** Declaration of a variable and optionally it's value. */
class DeclAST : public BaseAST {
    public:
        DeclAST(string nm, TType ty, shared_ptr<BaseAST> val) : name(nm), type(ty), value(val) {}

        string name;
        TType type;
        shared_ptr<BaseAST> value;

        void dump() {
            Print::print("Decl (", name, ") {");

            OFFSET++;

            value->dump();

            OFFSET--;

            Print::print("}");
        }
};

/** Node that represents function definition.
 *
 * `Deque` is needed to insert implicit type parameter
 * when function is defined inside of `implement`
 */
class FncDefAST : public BaseAST {
    public:
        FncDefAST(string nm, /**< Function name*/
                  deque<pair<string, TType>> ar, /**< Function arguments */
                  TType ret_t, /**< Function return type*/
                  vector<shared_ptr<BaseAST>> bd, /**< Function body */
                  map<string, TypedName> d_v
                  ) : name(nm), args(ar), body(bd), ret_type(ret_t), defined_variables(d_v) {}
        FncDefAST() {}

        string name;
        deque<pair<string, TType>> args;
        vector<shared_ptr<BaseAST>> body;
        TType ret_type;
        map<string, TypedName> defined_variables;

        void dump() {
            Print::print("FncDef (", name, ") (");

            OFFSET++;

            for (auto &a : args) {
                Print::print(a.first);
            }

            OFFSET--;

            Print::print(") {");

            OFFSET++;

            for (auto &b : body) {
                b->dump();
            }

            OFFSET--;
        }
};

/** Node that represents definition of an operator. */
class OperatorDefAST : public FncDefAST {
    public:
        OperatorDefAST(string nm, /**< Operator full name, e.g. `int+-!int` */
                       string b_n, /**< Operator base name, e.g. `+-!` */
                       deque<pair<string, TType>> params, /**< Name and type of LHS & RHS */
                       TType t, /**< Return type */
                       vector<shared_ptr<BaseAST>> bd, /**< Function body */
                       map<string, TypedName> d_v
            ) : FncDefAST(nm, params, t, bd, d_v), base_name(b_n) { }

        string base_name;

        void dump() {
            Print::print("OperatorDef (", args.at(0).first, name, args.at(1).first, ") {");
            OFFSET++;

            for (auto &b : body)
                b->dump();

            OFFSET--;
            Print::print("}");
        }
};

/** Node that represents implementing some functions for a type.
 *
 * Implementing a function really just means inserting an implicit type parameter (self)
 * as function's first argument
 */
class ImplementAST : public BaseAST {
    public:
        ImplementAST(string t, /**< Type name */
                     vector<FncDefAST> f /**< Implemented functions */
                     ) : type(t), fncs(f) {}
        string type;
        vector<FncDefAST> fncs;

        void dump() {
            Print::print("ImplementAST (", type, ") {");

            OFFSET++;

            for (auto &f : fncs) {
                f.dump();
            }

            OFFSET--;

            Print::print("}");
        }
};

/** Assignments to a variable. */
class AssAST : public BaseAST {
    public:
        AssAST(string nm, shared_ptr<BaseAST> val) : name(nm), value(val) {}
        string name;
        shared_ptr<BaseAST> value;

         void dump() {
             Print::print("Assign (", name, ") {");

             OFFSET++;

             value->dump();

             OFFSET--;

             Print::print("}");
        }
};

/** Identifier, e.g. variable name. */
class IdentAST : public BaseAST, public Expression {
    public:
        IdentAST(string v) : value(v) {}
        string value;

        void dump() {
            Print::print(value);
        }
};

/** Represents returning some value from a function. */
class RetAST : public BaseAST {
    public:
        RetAST(shared_ptr<BaseAST> v) : value(v) {}
        shared_ptr<BaseAST> value;

        void dump() {
            Print::print("Ret (");

            OFFSET++;

            if (value != nullptr)
                value->dump();

            OFFSET--;

            Print::print(")");
        }
};
