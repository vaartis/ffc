#include <sstream>
#include <map>
#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>

#include <assert.h>

#include "parser.hpp"

using namespace std;

TokenStream::TokenStream(string s) {
    text = make_unique<stringstream>(s);

    while (!text->eof()) {
        vec.push_back(getTok());
    }
}

pair<Token, string> TokenStream::getTok() {
    string IdentStr;
    static char lastchr = ' ';

    while (isspace(lastchr))
        lastchr = text->get();

    if (lastchr == '"') {
        lastchr = text->get();
        while (lastchr != '"') {
            IdentStr += lastchr;
            lastchr = text->get();
        }
        lastchr = text->get();
        return make_pair(Token::StrLit, IdentStr);
    }

    static const char op_chars[] = "!~@#$%^&*-+\\/<>";

    if (any_of(begin(op_chars), end(op_chars), [](char c) { return lastchr == c; })) {
        IdentStr = lastchr;
        lastchr = text->get();
        while (any_of(begin(op_chars), end(op_chars), [](char c) { return lastchr == c; })) {
            IdentStr += lastchr;
            lastchr = text->get();
        }
        return make_pair(Token::Operator, IdentStr);
    }

    bool f = false;
    if (isalpha(lastchr) || lastchr == '_') {
        f = true;
        IdentStr = lastchr;
        while (isalnum((lastchr = text->get())) || lastchr == '_')
            IdentStr += lastchr;
    }

    if (isdigit(lastchr)) {
        string numstr;
        bool f;

        do {
            if (lastchr == '.')
                f = true;
            numstr += lastchr;
            lastchr = this->text->get();
        } while (isdigit(lastchr) || lastchr == '.');

        if (!f)
            return make_pair(Token::IntLit, numstr);
        else
            return make_pair(Token::FloatLit, numstr);
    }

    #define match(wh, to, type) if (wh == to)\
            return make_pair(type, IdentStr);

    #define match_char(to, type) if (lastchr == to) {\
        lastchr = text->get();\
        IdentStr = string(1, to);\
        return make_pair(type, IdentStr);\
    }

    match(IdentStr, "fnc", Token::Fnc);
    match(IdentStr, "extern", Token::Extern);
    match(IdentStr, "operator", Token::OperatorDef);
    
    match(IdentStr, "int", Token::Type);
    match(IdentStr, "float", Token::Type);
    match(IdentStr, "bool", Token::Type);
    match(IdentStr, "str", Token::Type);

    match(IdentStr, "true", Token::BoolLit);
    match(IdentStr, "false", Token::BoolLit);

    match(IdentStr, "if", Token::If);
    match(IdentStr, "else", Token::Else);
    match(IdentStr, "ret", Token::Ret);

    if (!f) {
        match_char('(', Token::OpP);
        match_char(')', Token::ClP);
        match_char('{', Token::OpCB);
        match_char('}', Token::ClCB);
        match_char('=', Token::Eq);
        match_char(';', Token::Semicolon);
    }

    return make_pair(Token::Ident, IdentStr);
}

ASTParser::ASTParser(string s) : tokens(TokenStream(s)) {
    while (true) {
        try {
            if (currTok == Token::Fnc) {
                fns.push_back(parseFncDef());
            } else if (currTok == Token::Extern) {
                exts.push_back(parseExternFnc());
            } else if (currTok == Token::OperatorDef) {
                ops.push_back(parseOperatorDef());
            }
            getNextTok();
        } catch (TokenStream::EOFException) {
            break;
        }
    }
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
    bool b = (IdentStr == "true") ? true  : false;
    auto res = make_unique<BoolAST>(b);
    getNextTok();

    return move(res);
}

TType ASTParser::strToType(string s) {
    if (s == "int")
        return TType::Int;
    else if (s == "float")
        return TType::Float;
    else if (s == "bool")
        return TType::Bool;
    else if (s == "str")
        return TType::Str;
    else
        throw runtime_error("Unknown type");
}

unique_ptr<ExternFncAST> ASTParser::parseExternFnc() {
    getNextTok(); // eat extern

    assert(currTok == Token::Ident);

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    vector<TType> args;

    while (currTok != Token::ClP) {
        assert(currTok == Token::Type);

        TType t = strToType(IdentStr);
        args.push_back(t);

        getNextTok();
    }

    getNextTok(); // eat )

    TType ret_type = TType::Void;

    if (currTok == Token::Type) {
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
    assert(currTok == Token::Type);
    lhs.second = strToType(IdentStr);
    getNextTok();
    assert(currTok == Token::Ident);
    lhs.first = IdentStr;

    getNextTok();
    assert(currTok == Token::Type);
    rhs.second = strToType(IdentStr);
    getNextTok();
    assert(currTok == Token::Ident);
    rhs.first = IdentStr;
    getNextTok();

    assert(currTok == Token::ClP);
    getNextTok(); // eat )

    assert(currTok == Token::Type);
    TType ret_type = strToType(IdentStr);
    getNextTok(); // eat type

    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector< unique_ptr<BaseAST> > body;

    while (currTok != Token::ClCB) {
        bool skip_sm = false;

        if (currTok == Token::If)
            skip_sm = true;

        body.push_back(parseStmt());

        if (currTok != Token::ClCB && skip_sm == false) {
            assert(currTok == Token::Semicolon);
            getNextTok();
        }
    }

    return make_unique<OperatorDefAST>(name, lhs, rhs, ret_type, move(body));
}

/// fncdef ::= 'fnc' <literal> '(' (<type> <literal>)* ')' <type>*
unique_ptr<FncDefAST> ASTParser::parseFncDef() {
    getNextTok(); // eat fnc

    assert(currTok == Token::Ident);

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    map <string, TType> args;
    while (currTok != Token::ClP) {
        assert(currTok == Token::Type);

        TType t = strToType(IdentStr);

        getNextTok();

        assert (currTok == Token::Ident);

        args.insert(make_pair(IdentStr, t));

        getNextTok();
    }
    getNextTok(); // eat )

    TType ret_type = TType::Void;

    if (currTok == Token::Type) {
        ret_type = strToType(IdentStr);
        getNextTok(); // eat type
    }

    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector< unique_ptr<BaseAST> > body;

    while (currTok != Token::ClCB) {
        bool skip_sm = false;

        if (currTok == Token::If)
            skip_sm = true;

        body.push_back(parseStmt());

        if (currTok != Token::ClCB && skip_sm == false) {
            assert(currTok == Token::Semicolon);
            getNextTok();
        }
    }

    auto res = make_unique<FncDefAST>(name, args, ret_type, move(body));
    return res;
}

unique_ptr<BaseAST> ASTParser::parseRet() {
    getNextTok();

    if (currTok != Token::Semicolon)
        return make_unique<RetAST>(parseExpr());
    else
        return make_unique<RetAST>(nullptr);
}

// stmt ::= var | expr ';'
// expr ::= <expr> | <fncall> | <return>
unique_ptr<BaseAST> ASTParser::parseStmt() {
    auto cTok = currTok;
    auto nTok = tokens.peek().first;

    string maybe_tmp = IdentStr;
    string maybe_tmp2 = tokens.peek().second;

    // var ::= <type>* <literal> ( '=' <expr> )* ';'

    if ((cTok == Token::Type && nTok == Token::Ident) || (cTok == Token::Ident && nTok == Token::Eq)) {
        return parseVar();
    } else if (cTok == Token::Ident && nTok == Token::OpP) {
        return parseFncCall();
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
            return parseIf();
        default:
            break;
    }

    return nullptr;
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

        if (currTok != Token::ClCB) {
            assert(currTok == Token::Semicolon);
            getNextTok();
        }
    }

    getNextTok(); // eat { or get to else

    if (currTok == Token::Else) {
        getNextTok(); // eat else
        
        assert(currTok == Token::OpCB);
        
        getNextTok(); // eat {

        else_body.push_back(parseStmt());

        if (currTok != Token::ClCB) {
            assert(currTok == Token::Semicolon);
            getNextTok();
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

    if (currTok == Token::IntLit) {
        return parseIntLiteral();
    } else if (currTok == Token::FloatLit) {
        return parseFloatLiteral();
    } else if (currTok == Token::BoolLit) {
        return parseBoolLiteral();
    } else if (currTok == Token::StrLit) {
        return parseStrLiteral();
    } else if (currTok == Token::Ident) {
        if (nxt == Token::OpP) {
            return parseFncCall();
        } else {
            auto res = make_unique<IdentAST>(IdentStr);
            getNextTok();
            return move(res);
        }
    }

    throw runtime_error("Unknown expr");
}

// var ::= <type>* <literal> ( '=' <expr> )* ';'
unique_ptr<BaseAST> ASTParser::parseVar() {
    if (currTok == Token::Type) { // Varible creation
        string type = IdentStr;

        TType t = strToType(type);
        
        getNextTok(); // eat type

        assert(currTok == Token::Ident);

        string name = IdentStr;

        getNextTok();

        if (currTok == Token::Semicolon) {
            return make_unique<DeclAST>(name, t, unique_ptr<BaseAST>(nullptr));
        } else if (currTok == Token::Eq) {
            getNextTok();

            return make_unique<DeclAST>(name, t, parseExpr());
        }
    } else if (currTok == Token::Ident) { // varible assignment
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

    assert(currTok == Token::Ident);

    string name = IdentStr;

    getNextTok(); // eat name
    getNextTok(); // eat (

    while (currTok != Token::ClP) {
        args.push_back(parseExpr());
    }

    getNextTok(); // eat ;

    auto res = make_unique<FncCallAST>(name, move(args));

    return move(res);
}
