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
    TokenInfo r = tokens.get();
    IdentStr = r.IdentStr;
    symbol = r.symbol;
    line = r.line;
    return currTok = r.tok;
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
                    break;
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

#define if_type(t, s) (t == Token::Type || t == Token::Ref || isType(s))
#define if_ident(t, s) (t == Token::Ident && !isType(s))

TType ASTParser::parseTType() {
    if (if_type(currTok, IdentStr) && currTok != Token::Ref) {
        string s = IdentStr;
        getNextTok();

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
    } else if (currTok == Token::Ref) {
        getNextTok();
        return TType::withRef(make_shared<TType>(parseTType()));
    } else {
         throw runtime_error("Expected TYPE at " + to_string(line) + ":" + to_string(symbol));
    }
}

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
        TType tp = parseTType();
        assert(if_ident(currTok, IdentStr));
        string name = IdentStr;
        getNextTok();

        if (currTok != Token::ClCB && currTok != Token::Comma) { // comma at the end is optional
            throw runtime_error("Expected comma/end of type in definition of " + name);
        } else {
            if (currTok == Token::Comma) {
                getNextTok(); // eat ,
                fields.push_back({name, tp});
            } else {
                fields.push_back({name, tp});
                break;
            }
        }
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

unique_ptr<ExternFncAST> ASTParser::parseExternFnc() {
    getNextTok(); // eat extern

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    vector<TType> args;

    while (currTok != Token::ClP) {
        TType t = parseTType();
        args.push_back(t);

        if (currTok != Token::ClP) {
            assert(currTok == Token::Comma);
            getNextTok();
        }
    }

    getNextTok(); // eat )

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = parseTType();
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
    lhs.second = parseTType();
    assert(if_ident(currTok, IdentStr));
    lhs.first = IdentStr;

    getNextTok();
    assert(currTok == Token::Comma);
    getNextTok(); // eat comma

    rhs.second = parseTType();
    assert(if_ident(currTok, IdentStr));
    rhs.first = IdentStr;
    getNextTok();

    assert(currTok == Token::ClP);
    getNextTok(); // eat )

    TType ret_type = parseTType();

    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector< unique_ptr<BaseAST> > body;
    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    return make_unique<OperatorDefAST>(name, lhs, rhs, ret_type, move(body));
}

map<string, TType> ASTParser::parseParams() {
    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    map <string, TType> args;
    while (currTok != Token::ClP) {
        TType tp = parseTType();

        assert(if_ident(currTok, IdentStr));
        string name = IdentStr;
        getNextTok();

        args.insert({name, tp});

        if (currTok != Token::ClP) {
            assert(currTok == Token::Comma);
            getNextTok();
        }

    }
    getNextTok(); // eat )

    return args;
}

unique_ptr<FncDefAST> ASTParser::parseFncDef() {
    getNextTok(); // eat fnc

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    map<string, TType> args = parseParams();

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = parseTType();
    }

    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector<unique_ptr<BaseAST>> body;

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

unique_ptr<BaseAST> ASTParser::parseVal() {
    getNextTok(); // eat val
    if (currTok == Token::Ident) {
        if (tokens.peek().tok == Token::Eq) {
            return make_unique<ValOfRefAST>(parseAss());
        } else if (tokens.peek().tok == Token::Dot) {
            return make_unique<ValOfRefAST>(parseTypeFieldStore());
        } else {
            return make_unique<ValOfRefAST>(parseExpr());
        }
    } else {
        return make_unique<ValOfRefAST>(parseExpr());
    }
}

unique_ptr<BaseAST> ASTParser::parseStmt() {
    auto cTok = currTok;
    auto nTok = tokens.peek().tok;

    string maybe_tmp = IdentStr;
    string maybe_tmp2 = tokens.peek().IdentStr;

    bool skip_sm = false;

    unique_ptr<BaseAST> stmt;

    if (if_type(cTok, maybe_tmp)) {
        TType t = parseTType();

        if (!if_ident(currTok, IdentStr))
            throw runtime_error("Expected IDENT at " + to_string(line) + ":" + to_string(symbol));

        stmt = parseVar(t);
    } else if (if_ident(cTok, maybe_tmp) && nTok == Token::Eq) {
        stmt = parseAss();
    } else if (if_ident(cTok, maybe_tmp) && nTok == Token::OpP) {
        stmt = parseFncCall();
    } else if (if_ident(cTok, maybe_tmp) && nTok == Token::Dot) {
         stmt = parseTypeFieldStore();
    } else {
        switch (currTok) {
            case Token::Val:
                stmt = parseVal();
                break;
            case Token::Ret:
                stmt = parseRet();
                break;
            case Token::If:
                skip_sm = true;
                stmt = parseIf();
                break;
            case Token::While:
                skip_sm = true;
                stmt = parseWhile();
                break;
            default:
                throw runtime_error("Parsing failed: " + IdentStr);
        }
    }

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

    Token nxt = tokens.peek().tok;

    if (!parsing_op && nxt == Token::Operator) {
        parsing_op = true;
        auto lhs = parseExpr();
        parsing_op = false;
        return parseOperator(move(lhs));
    }

    switch(currTok) { // Simple casese
        case Token::Val:
            return parseVal();
        case Token::Ref:
             getNextTok();
             return make_unique<RefToValAST>(parseExpr());
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
            } else if (if_type(currTok, IdentStr) && currTok != Token::Ref) {
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

        if (currTok != Token::ClCB) {
            assert(currTok == Token::Comma);
            getNextTok();
        }

    }
    getNextTok();
    return make_unique<TypeAST>(name, move(fields));
}

// var ::= <type>* <literal> ( '=' <expr> )* ';'
unique_ptr<BaseAST> ASTParser::parseVar(TType t) {
    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    if (currTok == Token::Semicolon) {
        return make_unique<DeclAST>(name, t, unique_ptr<BaseAST>(nullptr));
    } else if (currTok == Token::Eq) {
        getNextTok();

        return make_unique<DeclAST>(name, t, parseExpr());
    }

    throw runtime_error("Failed variable parse at " + to_string(line) + ":" + to_string(symbol));
}

unique_ptr<BaseAST> ASTParser::parseAss() {
    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::Eq);

    getNextTok(); // eat =

    return make_unique<AssAST>(name, parseExpr());
}

unique_ptr<BaseAST> ASTParser::parseOperator(unique_ptr<BaseAST> lhs) {
    string name = IdentStr;
    getNextTok(); // eat op
    auto rhs = parseExpr();

    return make_unique<OperatorAST>(name, move(lhs), move(rhs));
}

unique_ptr<BaseAST> ASTParser::parseFncCall() {
    vector< unique_ptr<BaseAST> > args;

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok(); // eat name
    getNextTok(); // eat (

    while (currTok != Token::ClP) {
        args.push_back(parseExpr());
        if (currTok != Token::ClP) {
            assert(currTok == Token::Comma);
            getNextTok();
        }
    }

    getNextTok(); // eat }

    auto res = make_unique<FncCallAST>(name, move(args));

    if (currTok == Token::Operator) {
        return parseOperator(move(res));
    }

    return move(res);
}
