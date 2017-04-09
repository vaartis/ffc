#include <sstream>
#include <map>
#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>

#include <assert.h>

#include "ASTParser.hpp"
#include "TokenStream.hpp"

using namespace std;

Token ASTParser::getNextTok() {
    pair<Token, string> r = tokens.get();
    IdentStr = r.second;
    return currTok = r.first;
}

ASTParser::ASTParser(string s) : tokens(TokenStream(s)) {
    types = tokens.getTypes();
    while (true) {
        try {
            getNextTok();
            switch(currTok) {
                case Token::Fnc:
                    functions.push_back(parseFncDef());
                    break;
                case Token::Extern:
                    ext_functions.push_back(parseExternFnc());
                    break;
                case Token::Include:
                    includes.push_back(parseInclude());
                    break;
                case Token::TypeDef:
                    parseTypeDef();
                default:
                    break;
            }
        } catch (TokenStream::EOFException) {
            break;
        }
    }
}

bool ASTParser::isType(string s) {
    if (any_of(begin(types), end(types), [&](string c) { return c == s; })) {
        return true;
    }

    if (any_of(begin(typedefs), end(typedefs), [&](pair<const string, shared_ptr<TypeDefAST>> c) { return c.first == s; })) {
        return true;
    }

    return false;
}

#define if_type(t, s) (t == Token::Type || isType(s))
#define if_ident(t, s) (t == Token::Ident && !isType(s))

unique_ptr<BaseAST> ASTParser::parseWhile() {
    getNextTok(); // eat while
    unique_ptr<BaseAST> cond = parseExpr();

    assert(currTok == Token::OpCB);
    getNextTok(); // eat {

    vector<unique_ptr<BaseAST>> body;

    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    getNextTok(); // eat }

    return make_unique<WhileAST>(move(cond), move(body));
}

void ASTParser::parseTypeDef() {
    getNextTok(); // eat type token
    assert(if_ident(currTok, IdentStr));
    string name = IdentStr;
    getNextTok();
    assert(currTok == Token::OpCB);
    getNextTok();

    vector<pair<string, TType>> fields;
    while (currTok != Token::ClCB) {
        assert(isType(IdentStr));
        TType tp = strToType(IdentStr);
        getNextTok();
        assert(if_ident(currTok, IdentStr));
        string name = IdentStr;
        getNextTok();
        assert(currTok == Token::Semicolon);
        getNextTok(); // eat ;
        fields.push_back({name, tp});
    }
    typedefs.emplace(name, make_shared<TypeDefAST>(name, fields));
}

/// include = include <string literal> <string literal>*
unique_ptr<IncludeAST> ASTParser::parseInclude() {
    getNextTok(); // eat include
    assert(currTok == Token::StrLit);
    vector<string> mods;
    while (currTok != Token::Semicolon) {
        assert(currTok == Token::StrLit);
        mods.push_back(IdentStr);
        getNextTok();
    }

    return make_unique<IncludeAST>(mods);
}

/// str ::= "char+"
unique_ptr<BaseAST> ASTParser::parseStrLiteral() {
    auto res = make_unique<StrAST>(IdentStr);
    getNextTok();
    return move(res);
}

/// number ::= <number>
unique_ptr<BaseAST> ASTParser::parseIntLiteral() {
    auto res = make_unique<IntAST>(stoi(IdentStr));
    getNextTok();
    return move(res);
}

unique_ptr<BaseAST> ASTParser::parseFloatLiteral() {
    auto res = make_unique<FloatAST>(stof(IdentStr));
    getNextTok();
    return move(res);
}

unique_ptr<BaseAST> ASTParser::parseBoolLiteral() {
    bool b = (IdentStr == "true") ? true : false;
    auto res = make_unique<BoolAST>(b);
    getNextTok();

    return move(res);
}

TType ASTParser::strToType(string s) {
    if (s == "int")
        return _TType::Int;
    else if (s == "float")
        return _TType::Float;
    else if (s == "bool")
        return _TType::Bool;
    else if (s == "str")
        return _TType::Str;
    else if (isType(s))
        return s; // custom type
    else
        throw runtime_error("Unknown type:" + s);
}

unique_ptr<ExternFncAST> ASTParser::parseExternFnc() {
    getNextTok(); // eat extern

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    vector<TType> args;

    while (currTok != Token::ClP) {
        assert(if_type(currTok, IdentStr));

        TType t = strToType(IdentStr);
        args.push_back(t);

        getNextTok();
    }

    getNextTok(); // eat )

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = strToType(IdentStr);
        getNextTok(); // eat type
    }

    assert(currTok == Token::Semicolon);

    auto res = make_unique<ExternFncAST>(name, args, ret_type);

    return res;
}

unique_ptr<OperatorDefAST> ASTParser::parseOperatorDef() {
    getNextTok(); // eat operator

    assert(currTok == Token::Operator);

    string name = IdentStr;

    getNextTok(); // eat name

    pair<string, TType> lhs;
    pair<string, TType> rhs;

    assert(currTok == Token::OpP);

    getNextTok();
    assert(if_type(currTok, IdentStr));
    lhs.second = strToType(IdentStr);
    getNextTok();
    assert(if_ident(currTok, IdentStr));
    lhs.first = IdentStr;

    getNextTok();
    assert(if_type(currTok, IdentStr));
    rhs.second = strToType(IdentStr);
    getNextTok();
    assert(if_ident(currTok, IdentStr));
    rhs.first = IdentStr;
    getNextTok();

    assert(currTok == Token::ClP);
    getNextTok(); // eat )

    assert(if_type(currTok, IdentStr));
    TType ret_type = strToType(IdentStr);
    getNextTok(); // eat type

    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector< unique_ptr<BaseAST> > body;
    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    return make_unique<OperatorDefAST>(name, lhs, rhs, ret_type, move(body));
}

/// fncdef ::= 'fnc' <literal> '(' (<type> <literal>)* ')' <type>*
unique_ptr<FncDefAST> ASTParser::parseFncDef() {
    getNextTok(); // eat fnc

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    map <string, TType> args;
    while (currTok != Token::ClP) {
        TType t = strToType(IdentStr);

        getNextTok();

        assert(if_ident(currTok, IdentStr));

        args.insert(make_pair(IdentStr, t));

        getNextTok();
    }
    getNextTok(); // eat )

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = strToType(IdentStr);
        getNextTok(); // eat type
    }

    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector< unique_ptr<BaseAST> > body;

    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    auto res = make_unique<FncDefAST>(name, args, ret_type, move(body));
    return res;
}

unique_ptr<BaseAST> ASTParser::parseRet() {
    getNextTok(); // eat ret

    if (currTok != Token::Semicolon)
        return make_unique<RetAST>(parseExpr());
    else
        return make_unique<RetAST>(nullptr);
}

unique_ptr<BaseAST> ASTParser::parseStmt() {
    auto cTok = currTok;
    auto nTok = tokens.peek().first;

    string maybe_tmp = IdentStr;
    string maybe_tmp2 = tokens.peek().second;

    bool skip_sm = false;

    unique_ptr<BaseAST> stmt = [&]() -> unique_ptr<BaseAST> { // hackery
        if ((if_type(cTok, maybe_tmp) && if_ident(nTok, maybe_tmp2)) || (if_ident(cTok, maybe_tmp) && nTok == Token::Eq)) {
            return parseVar();
        } else if (if_ident(cTok, maybe_tmp) && nTok == Token::OpP) {
            return parseFncCall();
        } else if (if_ident(cTok, maybe_tmp) && nTok == Token::Dot) {
            return parseTypeFieldStore();
        }

        switch (currTok) {
            case Token::IntLit:
                return parseIntLiteral();
            case Token::FloatLit:
                return parseFloatLiteral();
            case Token::StrLit:
                return parseStrLiteral();
            case Token::Ret:
                return parseRet();
            case Token::If:
                skip_sm = true;
                return parseIf();
            case Token::While:
                skip_sm = true;
                return parseWhile();
            default:
                throw runtime_error("Parssing failed:" + IdentStr);
        }
    }();

    if (!skip_sm) {
        assert(currTok == Token::Semicolon);
        getNextTok();
    }

    return move(stmt);
}

// if ::= if <expr> { <stmt>* }
unique_ptr<BaseAST> ASTParser::parseIf() {
    getNextTok(); // eat if

    auto cond = parseExpr();

    assert(currTok == Token::OpCB);

    vector< unique_ptr<BaseAST> > body;
    vector< unique_ptr<BaseAST> > else_body;

    getNextTok(); // eat {

    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    getNextTok(); // eat } or get to else

    if (currTok == Token::Else) {
        getNextTok(); // eat else

        assert(currTok == Token::OpCB);

        getNextTok(); // eat {

        while (currTok != Token::ClCB) {
            else_body.push_back(parseStmt());
        }
        getNextTok(); // eat }
    }

    return make_unique<IfAST>(move(cond), move(body), move(else_body));
}

unique_ptr<BaseAST> ASTParser::parseExpr() {
    static bool parsing_op = false;

    Token nxt = tokens.peek().first;

    if (!parsing_op && nxt == Token::Operator) {
        parsing_op = true;
        auto lhs = parseExpr();
        parsing_op = false;
        return parseOperator(move(lhs));
    }

    switch(currTok) { // Simple casese
        case Token::IntLit:
            return parseIntLiteral();
        case Token::FloatLit:
            return parseFloatLiteral();
        case Token::BoolLit:
            return parseBoolLiteral();
        case Token::StrLit:
            return parseStrLiteral();
        default: // Cases with variables
            if (currTok == Token::OpP) {
                getNextTok(); // eat (
                auto res = parseExpr();
                assert(currTok == Token::ClP);
                getNextTok(); // eat )

                if (currTok == Token::Operator) // FIXME
                    return parseOperator(move(res));

                return res;
            } else if (if_ident(currTok, IdentStr)) {
                if (nxt == Token::OpP) {
                    return parseFncCall();
                } else if (nxt == Token::Dot) {
                    return parseTypeFieldLoad();
                } else {
                    auto res = make_unique<IdentAST>(IdentStr);
                    getNextTok();
                    return move(res);
                }
            } else if (if_type(currTok, IdentStr)) {
                return parseType();
            }
    }

    throw runtime_error("Unknown expr");
}

unique_ptr<BaseAST> ASTParser::parseTypeFieldStore() {
    string st_name = IdentStr;
    assert(!if_type(currTok, st_name));
    getNextTok();
    assert(currTok == Token::Dot);
    getNextTok(); // eat dor
    string f_name = IdentStr;
    getNextTok();

    assert(currTok == Token::Eq);
    getNextTok();
    unique_ptr<BaseAST> val = parseExpr();
    return make_unique<TypeFieldStoreAST>(st_name, f_name, move(val));
}

unique_ptr<BaseAST> ASTParser::parseTypeFieldLoad() {
    string st_name = IdentStr;
    assert(!if_type(currTok, st_name));
    getNextTok();
    assert(currTok == Token::Dot);
    getNextTok(); // eat dot
    string f_name = IdentStr;
    getNextTok();

    auto res = make_unique<TypeFieldLoadAST>(st_name, f_name);

    if (currTok == Token::Operator) {
        return parseOperator(move(res));
    }

    return res;
}

unique_ptr<BaseAST> ASTParser::parseType() {
    string name = IdentStr;
    getNextTok();
    assert(currTok == Token::OpCB);
    getNextTok();
    map<string, unique_ptr<BaseAST>> fields;
    while (currTok != Token::ClCB) {
        assert(if_ident(currTok, IdentStr));
        string f_name = IdentStr;
        getNextTok();
        assert(currTok == Token::Eq);
        getNextTok(); // eat =
        unique_ptr<BaseAST> f_val = parseExpr();
        fields.emplace(f_name, move(f_val));
    }
    getNextTok();
    return make_unique<TypeAST>(name, move(fields));
}

// var ::= <type>* <literal> ( '=' <expr> )* ';'
unique_ptr<BaseAST> ASTParser::parseVar() {
    if (if_type(currTok, IdentStr)) { // Varible creation
        string type = IdentStr;

        TType t = strToType(type);

        getNextTok(); // eat type

        assert(if_ident(currTok, IdentStr));

        string name = IdentStr;

        getNextTok();

        if (currTok == Token::Semicolon) {
            return make_unique<DeclAST>(name, t, unique_ptr<BaseAST>(nullptr));
        } else if (currTok == Token::Eq) {
            getNextTok();

            return make_unique<DeclAST>(name, t, parseExpr());
        }
    } else if (if_ident(currTok, IdentStr)) { // varible assignment
        string name = IdentStr;

        getNextTok(); // eat =
        getNextTok(); // set to next

        return make_unique<AssAST>(name, parseExpr());
    }

    throw runtime_error("Unknown token");
}

unique_ptr<BaseAST> ASTParser::parseOperator(unique_ptr<BaseAST> lhs) {
    string name = IdentStr;
    getNextTok(); // eat op
    auto rhs = parseExpr();

    return make_unique<OperatorAST>(name, move(lhs), move(rhs));
}

// fncall ::= <literal> '(' <literal>* ')'
unique_ptr<BaseAST> ASTParser::parseFncCall() {
    vector< unique_ptr<BaseAST> > args;

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok(); // eat name
    getNextTok(); // eat (

    while (currTok != Token::ClP) {
        args.push_back(parseExpr());
    }

    getNextTok(); // eat }

    auto res = make_unique<FncCallAST>(name, move(args));

    return move(res);
}
