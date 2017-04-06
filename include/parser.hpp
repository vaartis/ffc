#pragma once

#include <variant>

using std::string;
using std::unique_ptr;
using std::vector;
using std::map;
using std::pair;
using std::exception;
using std::stringstream;

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
    None,
    Fnc,
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

class TokenStream {
    public:
        TokenStream(string s);

        struct EOFException : exception {};

        long length() {
            return vec.size();
        }

        pair<Token, string> get() {
            if (index++ >= vec.size())
                throw TokenStream::EOFException();
            return vec[index - 1];
        }

        pair<Token, string> peek() {
            return vec[index];
        }

        vector< string> getTypes() {
            return types;
        }
    
    private:
        long index = 0;
        unique_ptr<stringstream> text;
        vector< pair<Token, string> > vec;
        char lastchr = ' ';
        vector<string> types;

        pair<Token, string> getTok();
};

class ASTParser {
    public:
        ASTParser(string s);
        ~ASTParser() {}

        #define gen_getter(ty, nm) vector<unique_ptr<ty>> get_##nm() { return move(nm); }
        gen_getter(FncDefAST, functions);
        gen_getter(ExternFncAST, ext_functions);
        gen_getter(OperatorDefAST, operators);
        gen_getter(IncludeAST, includes);
        map<string, std::shared_ptr<TypeDefAST>> get_typedefs() {return typedefs; }

    private:
        TokenStream tokens;

    vector<string> types;
        string IdentStr;
        Token currTok = Token::None;

        vector<unique_ptr<FncDefAST>> functions;
        vector<unique_ptr<ExternFncAST>> ext_functions;
        vector<unique_ptr<OperatorDefAST>> operators;
        vector<unique_ptr<IncludeAST>> includes;
        map<string, std::shared_ptr<TypeDefAST>> typedefs;

        Token getNextTok() { 
            pair<Token, string> r = tokens.get();
            IdentStr = r.second;
            return currTok = r.first;
        }

        bool isType(string);
        TType strToType(string s);

        unique_ptr<IncludeAST> parseInclude();
        unique_ptr<FncDefAST> parseFncDef();
        unique_ptr<ExternFncAST> parseExternFnc();
        unique_ptr<OperatorDefAST> parseOperatorDef();
        void parseTypeDef();

        unique_ptr<BaseAST> parseIntLiteral();
        unique_ptr<BaseAST> parseFloatLiteral();
        unique_ptr<BaseAST> parseBoolLiteral();
        unique_ptr<BaseAST> parseStrLiteral();

        unique_ptr<BaseAST> parseTypeFieldLoad();
        unique_ptr<BaseAST> parseType();
        unique_ptr<BaseAST> parseStmt();
        unique_ptr<BaseAST> parseFncCall();
        unique_ptr<BaseAST> parseVar();
        unique_ptr<BaseAST> parseExpr();
        unique_ptr<BaseAST> parseRet();
        unique_ptr<BaseAST> parseIf();
        unique_ptr<BaseAST> parseOperator(unique_ptr<BaseAST> lhs);
};
