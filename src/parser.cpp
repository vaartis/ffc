#include <sstream>
#include <map>
#include <iostream>
#include <memory>
#include <vector>

#include <assert.h>

#include "parser.hpp"

using namespace std;

enum class TType;
enum class Token;

class BaseAST;
class IntAST;
class FloatAST;
class BoolAST;
class FncDefAST;
class FncCallAST;
class DeclAST;
class AssAST;
class IdentAST;
class RetAST;

class TokenStream;

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

    bool f = false;
    if (isalpha(lastchr)) {
        f = true;
        IdentStr = lastchr;
        while (isalnum((lastchr = text->get())))
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
            return pair<Token, string>(Token::IntLit, numstr);
        else
            return pair<Token, string>(Token::FloatLit, numstr);
    }

    #define match(wh, to, type) if (wh == to) return pair<Token, string>(type, IdentStr);

    #define match_char(to, type) if (lastchr == to) {\
        lastchr = text->get();\
        IdentStr = string(1, to);\
        return pair<Token, string>(type, IdentStr);\
    }

    match(IdentStr, "fnc", Token::Fnc);
    
    match(IdentStr, "int", Token::Type);
    match(IdentStr, "float", Token::Type);
    match(IdentStr, "bool", Token::Type);

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

    return pair<Token, string>(Token::Ident, IdentStr);
}

ASTParser::ASTParser(string s) : tokens(TokenStream(s)) {
    while (true) {
        try {
            if (currTok == Token::Fnc) {
                fns.push_back(parseFncDef());
            }
            getNextTok();
        } catch (TokenStream::EOFException) {
            break;
        }
    }
}

/// number ::= <number>
unique_ptr<BaseAST> ASTParser::parseIntLiteral() {
    auto res = make_unique<IntAST>(stoi(IdentStr));
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
    else
        throw runtime_error("Unknown type");
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

        args.insert(pair<string, TType>(IdentStr, t));

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
    } else if (currTok == Token::IntLit) {
        return parseIntLiteral();
    } else if (cTok == Token::Ret) {
        return parseRet();
    } else if (cTok == Token::If) {
        return parseIf();
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

unique_ptr<BaseAST> ASTParser::parseExpr() {
    if (currTok == Token::IntLit) {
        return parseIntLiteral();
    } else if (currTok == Token::FloatLit) {
        return parseFloatLiteral();
    } else if (currTok == Token::BoolLit) {
        return parseBoolLiteral();
    } else if (currTok == Token::Ident) {
        Token nxt = tokens.peek().first;

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
