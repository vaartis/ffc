#pragma once

using std::string;
using std::unique_ptr;
using std::vector;
using std::map;
using std::pair;
using std::exception;
using std::stringstream;

enum class TType {
    Int,
    Float,
    Bool,
    Void
};

enum class Token {
    None,
    Fnc,
    Ident,
    IntLit,
    FloatLit,
    BoolLit,
    OpP,
    ClP,
    OpCB,
    ClCB,
    Semicolon,
    Eq,
    Type,
    Ret,
    If,
    Else
};

class BaseAST {
    public:
        virtual ~BaseAST() {}
};

class IntAST : public BaseAST {
    public:
        IntAST(int i) : value(i) {}

        int value;
};

class IfAST : public BaseAST {
    public:
        IfAST(unique_ptr<BaseAST> c, vector< unique_ptr<BaseAST> > bd, vector< unique_ptr<BaseAST> > el) : cond(move(c)), body(move(bd)), else_body(move(el)) {}

        unique_ptr<BaseAST> cond;
        vector< unique_ptr<BaseAST> > body;
        vector< unique_ptr<BaseAST> > else_body;
};

class FloatAST : public BaseAST {
    public:
        FloatAST(float f) : value(f) {}

        float value;
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
    private:
        long index = 0;
        unique_ptr<stringstream> text;
        vector< pair<Token, string> > vec;

        pair<Token, string> getTok();
};

class ASTParser {
    public:
        ASTParser(string s);
        ~ASTParser() {}

        vector< unique_ptr<FncDefAST> > get_functions() {
            return move(fns);
        }
    private:
        TokenStream tokens;

        string IdentStr;
        Token currTok = Token::None;

        vector< unique_ptr<FncDefAST> > fns;

        Token getNextTok() {
            pair<Token, string> r = tokens.get();
            IdentStr = r.second;
            return currTok = r.first;
        }

        TType strToType(string s);

        unique_ptr<FncDefAST> parseFncDef();

        unique_ptr<BaseAST> parseIntLiteral();
        unique_ptr<BaseAST> parseFloatLiteral();
        unique_ptr<BaseAST> parseBoolLiteral();

        unique_ptr<BaseAST> parseStmt();
        unique_ptr<BaseAST> parseFncCall();
        unique_ptr<BaseAST> parseVar();
        unique_ptr<BaseAST> parseExpr();
        unique_ptr<BaseAST> parseRet();
        unique_ptr<BaseAST> parseIf();
};
