#pragma once

#include <string>
#include <map>
#include <vector>
#include <iostream>

using std::string;
using std::unique_ptr;
using std::map;
using std::vector;
using std::pair;
using std::ostream;
using std::cout;
using std::endl;

unsigned static int OFFSET = 0;
static bool WAS_O = false;

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

class BaseAST {
    public:
        virtual ~BaseAST() {}
        virtual void dump() = 0; // Dump element to stdin
};

class WhileAST : public BaseAST {
    public:
        WhileAST(unique_ptr<BaseAST> c, vector<unique_ptr<BaseAST>> b) : cond(move(c)), body(move(b))  {}
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

class TypeDefAST : public BaseAST {
    public:
        TypeDefAST(string nm, vector<pair<string, TType>> vals) : name(nm), fields(vals) {}
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

class TypeFieldStoreAST : public BaseAST {
    public:
    TypeFieldStoreAST(string s_n, string f_n, unique_ptr<BaseAST> v) : struct_name(s_n), field_name(f_n), value(move(v)) {}
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

class TypeFieldLoadAST : public BaseAST {
    public:
        TypeFieldLoadAST(string st_n, string f_n) : struct_name(st_n), field_name(f_n) {}
        string struct_name;
        string field_name;

        void dump() {
            Print::print("TypeFieldLoad (", struct_name, ".", field_name, ")");
        }
};

class TypeAST : public BaseAST {
    public:
        TypeAST(string nm, map<string, unique_ptr<BaseAST>> flds) : name(nm), fields(move(flds)) {}
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

class IncludeAST : public BaseAST {
    public:
        IncludeAST(vector<string> mod) : modules(mod) {}
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

class OperatorDefAST : public BaseAST {
    public:
        OperatorDefAST(string nm, pair<string, TType> l, pair<string, TType> r, TType t, vector<unique_ptr<BaseAST>> bd) : name(nm), lhs(l), rhs(r), ret_type(t), body(move(bd)) {}

        string name;
        pair<string, TType> lhs;
        pair<string, TType> rhs;
        vector< unique_ptr<BaseAST> > body;
        TType ret_type;

        void dump() {
            Print::print("OperatorDef (", lhs.first, name, rhs.first, ") {");
            OFFSET++;

            for (auto &b : body)
                b->dump();

            OFFSET--;
            Print::print("}");
        }
};

class OperatorAST : public BaseAST {
    public:
        OperatorAST(string nm, unique_ptr<BaseAST> l, unique_ptr<BaseAST> r) : name(nm), lhs(move(l)), rhs(move(r)) {}
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

class IfAST : public BaseAST {
    public:
        IfAST(unique_ptr<BaseAST> c, vector< unique_ptr<BaseAST> > bd, vector< unique_ptr<BaseAST> > el) : cond(move(c)), body(move(bd)), else_body(move(el)) {}

        unique_ptr<BaseAST> cond;
        vector< unique_ptr<BaseAST> > body;
        vector< unique_ptr<BaseAST> > else_body;

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

class IntAST : public BaseAST {
    public:
        IntAST(int i) : value(i) {}

        int value;

        void dump() {
            Print::print(value);
        }
};

class FloatAST : public BaseAST {
    public:
        FloatAST(float f) : value(f) {}

        float value;

         void dump() {
            Print::print(value);
        }
};

class StrAST : public BaseAST {
    public:
        StrAST(string s) : value(s) {}

        string value;

        void dump() {
            Print::print(value);
        }
};

class BoolAST : public BaseAST {
    public:
        BoolAST(bool b) : value(b) {}

        bool value;

        void dump() {
            Print::print(value);
        }
};

class FncDefAST : public BaseAST {
    public:
        FncDefAST(string nm, map<string, TType> ar, TType ret_t, vector< unique_ptr<BaseAST> > bd) : name(nm), args(ar), ret_type(ret_t), body(move(bd)) {}

        string name;
        map<string, TType> args;
        vector< unique_ptr<BaseAST> > body;
        TType ret_type;

        void dump() {
            Print::print("FncDef (", name, ") (");

            OFFSET++;

            for (auto &a : args) {
                Print::print(&a.first);
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

class ExternFncAST : public BaseAST {
    public:
        ExternFncAST(string s, vector<TType> ar, TType r) : name(s), args(ar), ret_type(r) {}

        string name;
        vector<TType> args;
        TType ret_type;

        void dump() {
            Print::print("ExternFnc (", name, ")");
        }
};

class FncCallAST : public BaseAST {
    public:
        FncCallAST(string nm, vector< unique_ptr<BaseAST> > ar) : name(nm), args(move(ar)) {}

        string name;
        vector< unique_ptr<BaseAST> > args;

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

class IdentAST : public BaseAST {
    public:
        IdentAST(string v) : value(v) {}
        string value;

        void dump() {
            Print::print(value);
        }
};

class RetAST : public BaseAST {
    public:
        RetAST(unique_ptr<BaseAST> v) : value(move(v)) {}
        unique_ptr<BaseAST> value;

        void dump() {
            Print::print("Ret (");

            OFFSET++;

            value->dump();

            OFFSET--;

            Print::print(")");
        }
};
