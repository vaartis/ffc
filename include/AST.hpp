#pragma once

#include <string>
#include <map>
#include <vector>
#include <deque>
#include <iostream>

using std::string;
using std::unique_ptr;
using std::map;
using std::vector;
using std::deque;
using std::pair;
using std::ostream;
using std::cout;
using std::endl;

unsigned static int OFFSET = 0;
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

/** Represents a callable node.
 */
class Call {
    public:
        /** Get name of this node. */
        virtual string getName() = 0;

        /** Get arguments of this callable node. */
        virtual deque<pair<string, TType>> getArgs() = 0;

        /** Type of node, `F` for function, `O` for operator. */
        virtual char getType() = 0;
};

/** Represents a block that can hold a value. */
class Block {
    public:
        /** True, if this block returns some value */
        virtual bool hasValue() = 0;
};

/** While loop AST node. */
class WhileAST : public BaseAST {
    public:
        WhileAST(unique_ptr<BaseAST> c, /**< Condition */
                 vector<unique_ptr<BaseAST>> b /**< While loop body */
                 ) : cond(move(c)), body(move(b))  {}
        unique_ptr<BaseAST> cond;
        vector<unique_ptr<BaseAST>> body;

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
                          unique_ptr<BaseAST> v /**< Value */
                          ) : struct_name(s_n), field_name(f_n), value(move(v)) {}
        string struct_name;
        string field_name;
        unique_ptr<BaseAST> value;

        void dump() {
            Print::print("TypeFieldStore (", struct_name, ".", field_name, ") = ");

            OFFSET++;

            value->dump();

            OFFSET--;
        }
};

/** AST node that represents loading a value from type's field. */
class TypeFieldLoadAST : public BaseAST {
    public:
        TypeFieldLoadAST(string st_n, /**< Instance name */
                         string f_n /**< Field name */
                         ) : struct_name(st_n), field_name(f_n) {}
        string struct_name;
        string field_name;

        void dump() {
            Print::print("TypeFieldLoad (", struct_name, ".", field_name, ")");
        }
};

/** This node represents in-place creation of a type instance */
class TypeAST : public BaseAST {
    public:
        TypeAST(string nm, /**< Type name */
                map<string, unique_ptr<BaseAST>> flds /**< Names of fields and their values, order is not important here */
                ) : name(nm), fields(move(flds)) {}
        string name;
        map<string, unique_ptr<BaseAST>> fields;

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

/** AST node that represents usage of an operator */
class OperatorAST : public BaseAST {
    public:
        OperatorAST(string nm, /**< Operator name, e.g. `+-` */
                    unique_ptr<BaseAST> l, /**< Value of LHS */
                    unique_ptr<BaseAST> r /**< Value of RHS */
                    ) : name(nm), lhs(move(l)), rhs(move(r)) {}
        string name;
        unique_ptr<BaseAST> lhs;
        unique_ptr<BaseAST> rhs;

        void dump() {
            Print::print("Operator (", name, ") (LHS, RHS) {");

            OFFSET++;

            lhs->dump();
            rhs->dump();

            Print::print("}");
        }
};

/** Node that represents if (and optionally else) branch.
 *
 * Note, that `if` is an expression and therefore can return something,
 * compiler checks if it does and forces users to write an else branch.
 * Value can be returned by ommiting the semicolon after the last expression in if/else block
 */
class IfAST : public BaseAST, public Block {
    public:
        IfAST(unique_ptr<BaseAST> c, /**< Condition */
              vector<unique_ptr<BaseAST>> bd, /**< `If` branch body */
              vector<unique_ptr<BaseAST>> el, /**< `Else` branch body*/
              unique_ptr<BaseAST> v, /**< Value of a branch if there is one or nullptr otherwise */
              unique_ptr<BaseAST> ev /**< Likewise, but for `else`*/
              ) : cond(move(c)), body(move(bd)), else_body(move(el)), value(move(v)), else_value(move(ev)) {}

        unique_ptr<BaseAST> cond;
        vector<unique_ptr<BaseAST>> body;
        vector<unique_ptr<BaseAST>> else_body;
        unique_ptr<BaseAST> value;
        unique_ptr<BaseAST> else_value;

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
class IntAST : public BaseAST {
    public:
        IntAST(int i) : value(i) {}

        int value;

        void dump() {
            Print::print(value);
        }
};

/** Float literal. */
class FloatAST : public BaseAST {
    public:
        FloatAST(float f) : value(f) {}

        float value;

         void dump() {
            Print::print(value);
        }
};

/** String literal. */
class StrAST : public BaseAST {
    public:
        StrAST(string s) : value(s) {}

        string value;

        void dump() {
            Print::print(value);
        }
};

/** Boolean literal. */
class BoolAST : public BaseAST {
    public:
        BoolAST(bool b) : value(b) {}

        bool value;

        void dump() {
            Print::print(value);
        }
};
/** .Node that represents function definition.
 *
 * `Deque` is needed to insert implicit type parameter
 * when function is defined inside of `implement`
 */
class FncDefAST : public BaseAST, public Call {
    public:
        FncDefAST(string nm, /**< Function name*/
                  deque<pair<string, TType>> ar, /**< Function arguments */
                  TType ret_t, /**< Function return type*/
                  vector<unique_ptr<BaseAST>> bd /**< Function body */
                  ) : name(nm), args(ar), ret_type(ret_t), body(move(bd)) {}

        string name;
        deque<pair<string, TType>> args;
        vector< unique_ptr<BaseAST> > body;
        TType ret_type;

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

        string getName() { return name; }
        deque<pair<string, TType>> getArgs() { return args; }
        char getType() { return 'F'; }
};

/** Node that represents definition of an operator. */
class OperatorDefAST : public FncDefAST {
    public:
        OperatorDefAST(string nm, /**< Operator name, e.g. `+-!` */
                       deque<pair<string, TType>> params, /**< Name and type of LHS & RHS */
                       TType t, /**< Return type */
                       vector<unique_ptr<BaseAST>> bd /**< Function body */
            ) : FncDefAST(nm, params, t, move(bd)) { }

        void dump() {
            Print::print("OperatorDef (", args.at(0).first, name, args.at(1).first, ") {");
            OFFSET++;

            for (auto &b : body)
                b->dump();

            OFFSET--;
            Print::print("}");
        }

        char getType() { return 'O'; }
};

/** Node that represents implementing some functions for a type.
 *
 * Implementing a function really just means inserting an implicit type parameter (self)
 * as function's first argument
 */
class ImplementAST : public BaseAST {
    public:
        ImplementAST(string t, /**< Type name */
                     vector<unique_ptr<FncDefAST>> f /**< Implemented functions */
                     ) : type(t), fncs(move(f)) {}
        string type;
        vector<unique_ptr<FncDefAST>> fncs;

        void dump() {
            Print::print("ImplementAST (", type, ") {");

            OFFSET++;

            for (auto &f : fncs) {
                f->dump();
            }

            OFFSET--;

            Print::print("}");
        }
};

/** Represents reference to some value. */
class RefToValAST : public BaseAST {
    public:
        RefToValAST(unique_ptr<BaseAST> v) : value(move(v)) {}
        unique_ptr<BaseAST> value;

        void dump() {
            Print::print("RefToVal (");
            value->dump();
            Print::print(")");
        }
};

/** Represents dereference. */
class ValOfRefAST : public BaseAST {
    public:
        ValOfRefAST(unique_ptr<BaseAST> v) : value(move(v)) {}
        unique_ptr<BaseAST> value;

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

/** Represents a call to some function.
 *
 * Deque is needed for an implicit type parameter
 */
class FncCallAST : public BaseAST {
    public:
        FncCallAST(string nm, /**< Function name */
                   deque<unique_ptr<BaseAST>> ar, /**< Function arguments */
                   string t = "" /**< Type, to which this function belongs or an empty string*/
                   ) : name(nm), args(move(ar)), type(t) {}

        string name;
        deque<unique_ptr<BaseAST>> args;
        string type;

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

/** Declaration of a variable and optionally it's value. */
class DeclAST : public BaseAST {
    public:
        DeclAST(string nm, TType ty, unique_ptr<BaseAST> val) : name(nm), type(ty), value(move(val)) {}

        string name;
        TType type;
        unique_ptr<BaseAST> value;

        void dump() {
            Print::print("Decl (", name, ") {");

            OFFSET++;

            value->dump();

            OFFSET--;

            Print::print("}");
        }
};

/** Assignments to a variable. */
class AssAST : public BaseAST {
    public:
        AssAST(string nm, unique_ptr<BaseAST> val) : name(nm), value(move(val)) {}
        string name;
        unique_ptr<BaseAST> value;

         void dump() {
             Print::print("Assign (", name, ") {");

             OFFSET++;

             value->dump();

             OFFSET--;

             Print::print("}");
        }
};

/** Identifier, e.g. variable name. */
class IdentAST : public BaseAST {
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
        RetAST(unique_ptr<BaseAST> v) : value(move(v)) {}
        unique_ptr<BaseAST> value;

        void dump() {
            Print::print("Ret (");

            OFFSET++;

            if (value != nullptr)
                value->dump();

            OFFSET--;

            Print::print(")");
        }
};
