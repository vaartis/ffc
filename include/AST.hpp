#pragma once

#include <string>
#include <map>
#include <vector>

using std::string;
using std::unique_ptr;
using std::map;
using std::vector;
using std::pair;

class BaseAST {
    public:
        virtual ~BaseAST() {}
};

class TypeDefAST : public BaseAST {
    public:
        TypeDefAST(string nm, vector<pair<string, TType>> vals) : name(nm), fields(vals) {}
        string name;
        vector<pair<string, TType>> fields;
};

class TypeFieldLoadAST : public BaseAST {
    public:
        TypeFieldLoadAST(string st_n, string f_n) : struct_name(st_n), field_name(f_n) {}
        string struct_name;
        string field_name;
};

class TypeAST : public BaseAST {
    public:
    TypeAST(string nm, map<string, unique_ptr<BaseAST>> flds) : name(nm), fields(move(flds)) {}
    string name;
    map<string, unique_ptr<BaseAST>> fields;
};

class IncludeAST : public BaseAST {
    public:
        IncludeAST(vector<string> mod) : modules(mod) {}
        vector<string> modules;
};

class OperatorDefAST : public BaseAST {
    public:
    OperatorDefAST(string nm, pair<string, TType> l, pair<string, TType> r, TType t, vector<unique_ptr<BaseAST>> bd) : name(nm), lhs(l), rhs(r), ret_type(t), body(move(bd)) {}

        string name;
        pair<string, TType> lhs;
        pair<string, TType> rhs;
        vector< unique_ptr<BaseAST> > body;
        TType ret_type;
};

class OperatorAST : public BaseAST {
    public:
    OperatorAST(string nm, unique_ptr<BaseAST> l, unique_ptr<BaseAST> r) : name(nm), lhs(move(l)), rhs(move(r)) {}
    string name;
    unique_ptr<BaseAST> lhs;
    unique_ptr<BaseAST> rhs;
};

class IfAST : public BaseAST {
    public:
        IfAST(unique_ptr<BaseAST> c, vector< unique_ptr<BaseAST> > bd, vector< unique_ptr<BaseAST> > el) : cond(move(c)), body(move(bd)), else_body(move(el)) {}

        unique_ptr<BaseAST> cond;
        vector< unique_ptr<BaseAST> > body;
        vector< unique_ptr<BaseAST> > else_body;
};

class IntAST : public BaseAST {
    public:
        IntAST(int i) : value(i) {}

        int value;
};

class FloatAST : public BaseAST {
    public:
        FloatAST(float f) : value(f) {}

        float value;
};

class StrAST : public BaseAST {
    public:
        StrAST(string s) : value(s) {}

        string value;
};

class BoolAST : public BaseAST {
    public:
        BoolAST(bool b) : value(b) {}

        bool value;
};

class FncDefAST : public BaseAST {
    public:
        FncDefAST(string nm, map<string, TType> ar, TType ret_t, vector< unique_ptr<BaseAST> > bd) : name(nm), args(ar), ret_type(ret_t), body(move(bd)) {}

        string name;
        map<string, TType> args;
        vector< unique_ptr<BaseAST> > body;
        TType ret_type;
};

class ExternFncAST : public BaseAST {
    public:
        ExternFncAST(string s, vector<TType> ar, TType r) : name(s), args(ar), ret_type(r) {}

        string name;
        vector<TType> args;
        TType ret_type;
};

class FncCallAST : public BaseAST {
    public:
        FncCallAST(string nm, vector< unique_ptr<BaseAST> > ar) : name(nm), args(move(ar)) {}

        string name;
        vector< unique_ptr<BaseAST> > args;
};

class DeclAST : public BaseAST {
    public:
        DeclAST(string nm, TType ty, unique_ptr<BaseAST> val) : name(nm), type(ty), value(move(val)) {}

        string name;
        TType type;
        unique_ptr<BaseAST> value;
};

class AssAST : public BaseAST {
    public:
        AssAST(string nm, unique_ptr<BaseAST> val) : name(nm), value(move(val)) {}
        string name;
        unique_ptr<BaseAST> value;
};

class IdentAST : public BaseAST {
    public:
        IdentAST(string v) : value(v) {}
        string value;
};

class RetAST : public BaseAST {
    public:
        RetAST(unique_ptr<BaseAST> v) : value(move(v)) {}
        unique_ptr<BaseAST> value;
};
