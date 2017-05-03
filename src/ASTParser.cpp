#include <sstream>
#include <map>
#include <deque>
#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>

#include <assert.h>

#include "ASTParser.hpp"
#include "TokenStream.hpp"

using namespace std;

/** Get next token from a token stream. */
Token ASTParser::getNextTok() {
    TokenInfo r = tokens.get();
    IdentStr = r.IdentStr;
    symbol = r.symbol;
    line = r.line;
    return currTok = r.tok;
}

ASTParser::ASTParser(string s) : tokens(TokenStream(s)) {
    types = tokens.getTypes();
    currTok = Token::None;
    if (IdentStr.length() == 0)
        getNextTok();
    while (true) {
        switch(currTok) {
            case Token::Eof:
                goto end;
            case Token::Fnc:
                functions.push_back(parseFncDef<FncDefAST>());
                break;
            case Token::OperatorDef:
                operators.push_back(parseFncDef<OperatorDefAST>());
                break;
            case Token::Extern:
                ext_functions.push_back(parseExternFnc());
                break;
            case Token::Implement:
                impls.push_back(parseImplement());
                break;
            case Token::Include:
                includes.push_back(parseInclude());
                break;
            case Token::TypeDef:
                parseTypeDef();
                break;
            default:
                if (IdentStr.length() > 0)
                    cerr << "WARNING: unknown toplevel token " << IdentStr << endl;
                getNextTok();
                break;
        }
    }
end:
    return;
}

/** Checks if provided string is a custom type. */
bool ASTParser::isType(string s) {
    if (any_of(begin(types), end(types), [&](string c) { return c == s; })) {
        return true;
    }

    if (any_of(begin(typedefs), end(typedefs), [&](pair<const string, TypeDefAST> c) { return c.first == s; })) {
        return true;
    }

    return false;
}

#define if_type(t, s) (t == Token::Type || t == Token::Ref || isType(s))
#define if_ident(t, s) (t == Token::Ident && !isType(s))

/** Parse type at current position. */
TType ASTParser::parseTType() {
    try {
        auto curr_f_g = generic_types.at(curr_fn_name);
        if (find(curr_f_g.begin(), curr_f_g.end(), IdentStr) != curr_f_g.end()) {
            string s = IdentStr;
            getNextTok();
            return GenericType(s);
        }
    } catch (out_of_range) {}
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

/** Parses statements, this includes expressions when thir value is discarded. */
shared_ptr<BaseAST> ASTParser::parseStmt(bool test_semicolon = true /** There are cases where we need
                                                                     * to check semicolon explicitly, e.g. ASTParser::parseIf()
                                                                     */) {
    auto cTok = currTok;
    auto nTok = tokens.peek().tok;

    string maybe_tmp = IdentStr;
    string maybe_tmp2 = tokens.peek().IdentStr;

    bool skip_sm = false;

    shared_ptr<BaseAST> stmt;

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
        getNextTok(); // eat name
        getNextTok(); // eat dot

        if (tokens.peek().tok == Token::OpP) {
            stmt = parseTypeFncCall(maybe_tmp);
        } else {
            stmt = parseTypeFieldStore(maybe_tmp);
        }
    } else {
        switch (currTok) {
            case Token::Val:
                stmt = parseVal();
                break;
            case Token::Ret:
                stmt = parseRet();
                break;
            case Token::While:
                skip_sm = true;
                stmt = parseWhile();
                break;
            default:
                if (currTok == Token::If || currTok == Token::Else)
                    skip_sm = true;
                stmt = parseExpr();
                break;
        }
    }

    if (!skip_sm) {
        if (test_semicolon && currTok != Token::Semicolon)
            throw runtime_error("Expected SEMICOLON at " + to_string(line) + ":" + to_string(symbol));
        else if (test_semicolon && currTok == Token::Semicolon) {
            getNextTok();
        }
    }

    return stmt;
}

/** Parses function implementation for types */
ImplementAST ASTParser::parseImplement() {
    getNextTok(); // eat implement

    if (/* TODO: Traits */ false) {
    }

    assert(currTok == Token::For);

    getNextTok();

    assert(if_type(currTok, IdentStr));

    string type = IdentStr;

    getNextTok();

    assert(currTok == Token::OpCB);

    vector<FncDefAST> fncs;
    unique_ptr<FncDefAST> destructor;

    getNextTok();

    while (currTok != Token::ClCB) {
        FncDefAST curr_fnc;

        if (currTok == Token::Fnc) {
            curr_fnc = parseFncDef<FncDefAST>();
        } else if (currTok == Token::Destructor) {
            getNextTok(); // eat destructor
            curr_fnc = FncDefAST("destructor", deque<pair<string, TType>>{}, _TType::Void, parseFncBody());
        } else {
            throw runtime_error("Unknown token in IMPLEMENT");
        }

        curr_fnc.args.push_front({"self", type});
        fncs.push_back(move(curr_fnc));
    }

    getNextTok(); // eat }

    return ImplementAST(type, move(fncs));
}

/** Parse while loop */
shared_ptr<BaseAST> ASTParser::parseWhile() {
    getNextTok(); // eat while
    shared_ptr<BaseAST> cond = parseExpr();

    assert(currTok == Token::OpCB);
    getNextTok(); // eat {

    vector<shared_ptr<BaseAST>> body;

    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    getNextTok(); // eat }

    return make_shared<WhileAST>(move(cond), move(body));
}

/** Parses type definition. */
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

    getNextTok(); // eat }

    typedefs.emplace(name, TypeDefAST(name, fields));
}

/** Parses includes. */
IncludeAST ASTParser::parseInclude() {
    getNextTok(); // eat include
    assert(currTok == Token::StrLit);
    vector<string> mods;
    while (currTok != Token::Semicolon) {
        assert(currTok == Token::StrLit);
        mods.push_back(IdentStr);
        getNextTok();
    }
    getNextTok(); // eat ;

    return IncludeAST(mods);
}

/** Parses string literals. */
shared_ptr<BaseAST> ASTParser::parseStrLiteral() {
    auto res = make_shared<StrAST>(IdentStr);
    getNextTok();
    return res;
}

/** Parses integer literals. */
shared_ptr<BaseAST> ASTParser::parseIntLiteral() {
    auto res = make_shared<IntAST>(stoi(IdentStr));
    getNextTok();
    return res;
}

/** Parses floating pointer number literals. */
shared_ptr<BaseAST> ASTParser::parseFloatLiteral() {
    auto res = make_shared<FloatAST>(stof(IdentStr));
    getNextTok();
    return res;
}

/** Parses boolean literals. */
shared_ptr<BaseAST> ASTParser::parseBoolLiteral() {
    bool b = (IdentStr == "true") ? true : false;
    auto res = make_shared<BoolAST>(b);
    getNextTok();

    return res;
}

/** Parses function externs. */
ExternFncAST ASTParser::parseExternFnc() {
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

    getNextTok(); // eat ;

    return ExternFncAST(name, args, ret_type);
}

vector<shared_ptr<BaseAST>> ASTParser::parseFncBody() {
    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector<shared_ptr<BaseAST>> body;

    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    getNextTok(); // eat }

    return body;
}

/** Parses function or operator definition. */
template<class T> T ASTParser::parseFncDef() {
    Token type = currTok;
    getNextTok(); // eat fnc

    vector<string> generics;
    if (currTok == Token::Generic) {
        while (!(if_ident(currTok,IdentStr) && tokens.peek().tok == Token::OpP)) {
            getNextTok();
            assert(if_ident(currTok, IdentStr));
            generics.push_back(IdentStr);
            getNextTok();
            if (!(if_ident(currTok,IdentStr) && tokens.peek().tok == Token::OpP)) {
                assert(currTok == Token::Comma);
            }
        }
    }

    if (type == Token::Fnc)
        assert(if_ident(currTok, IdentStr));
    else if (type == Token::OperatorDef)
        assert(currTok == Token::Operator);

    string name = IdentStr;
    curr_fn_name = name;

    generic_types.emplace(name, generics);

    getNextTok();

    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    deque<pair<string, TType>> args;
    while (currTok != Token::ClP) {
        TType tp = parseTType();

        assert(if_ident(currTok, IdentStr));
        string name = IdentStr;
        getNextTok();

        args.push_back({name, tp});

        if (currTok != Token::ClP) {
            assert(currTok == Token::Comma);
            getNextTok();
        }

    }
    getNextTok(); // eat )

    if (type == Token::OperatorDef)
        assert(args.size() == 2 && "More then two arguments for an operator!");

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr) || find(generics.begin(), generics.end(), IdentStr) != generics.end()) {
        ret_type = parseTType();
    }

    vector<shared_ptr<BaseAST>> body = parseFncBody();

    if (type == Token::Fnc)
        return T(name, args, ret_type, body);
    else
        return T(name, args, ret_type, body);
}

/** Parses function's return statements. */
shared_ptr<BaseAST> ASTParser::parseRet() {
    getNextTok(); // eat ret

    if (currTok != Token::Semicolon)
        return make_shared<RetAST>(parseExpr());
    else
        return make_shared<RetAST>(nullptr);
}

/** Parses pointer dereferencing. */
shared_ptr<BaseAST> ASTParser::parseVal() {
    getNextTok(); // eat val
    if (currTok == Token::Ident) {
        if (tokens.peek().tok == Token::Eq) {
            return make_shared<ValOfRefAST>(parseAss());
        } else if (tokens.peek().tok == Token::Dot) {
            return make_shared<ValOfRefAST>(parseTypeFieldStore(IdentStr));
        } else {
            return make_shared<ValOfRefAST>(parseExpr());
        }
    } else {
        return make_shared<ValOfRefAST>(parseExpr());
    }
}

/** Parse blocks that can return value.
 *
 * This function returns body of a block and optionally it's result (or nullptr if none)
 */
pair<vector<shared_ptr<BaseAST>>, shared_ptr<BaseAST>> ASTParser::parseBlock() {
    assert(currTok == Token::OpCB);

    getNextTok();

    vector<shared_ptr<BaseAST>> body;
    shared_ptr<BaseAST> value = nullptr;

    while (currTok != Token::ClCB) {
        auto v = parseStmt(false);

        if (currTok == Token::ClCB) {
            BaseAST *inner = v.get();

            if (auto expr = dynamic_cast<Block *>(inner)) {
                if (expr->hasValue())
                    value = v;
                else
                    body.push_back(v);
            } else {
                value = v;
            }
        } else if (currTok == Token::Semicolon) {
            body.push_back(v);
            getNextTok();
        } else {
            throw runtime_error("Expected SEMICOLON at " + to_string(line) + ":" + to_string(symbol));
        }
    }

    assert(currTok == Token::ClCB);
    getNextTok();

    return {body, value};
}

/** Parse if (and else). */
shared_ptr<BaseAST> ASTParser::parseIf() {
    getNextTok(); // eat if

    auto cond = parseExpr();

    pair<vector<shared_ptr<BaseAST>>, shared_ptr<BaseAST>> parsed_then = parseBlock();
    vector<shared_ptr<BaseAST>> body = parsed_then.first;
    shared_ptr<BaseAST> value = parsed_then.second;

    if (value != nullptr && currTok != Token::Else) {
        throw runtime_error("If expression returning value must have an else branch");
    }

    vector<shared_ptr<BaseAST>> else_body;
    shared_ptr<BaseAST> else_value = nullptr;
    if (currTok == Token::Else) {
        getNextTok(); // eat else

        pair<vector<shared_ptr<BaseAST>>, shared_ptr<BaseAST>> parsed_else = parseBlock();
        else_body = parsed_else.first;
        else_value = parsed_else.second;
    }

    return make_shared<IfAST>(cond, body, else_body, value, else_value);
}

/** Parse expressions. */
shared_ptr<BaseAST> ASTParser::parseExpr() {
    static bool parsing_op = false;

    Token nxt = tokens.peek().tok;

    if (!parsing_op && nxt == Token::Operator) {
        parsing_op = true;
        auto lhs = parseExpr();
        parsing_op = false;
        return parseOperator(lhs);
    }

    switch(currTok) { // Simple casese
        case Token::Val:
            return parseVal();
        case Token::Ref:
             getNextTok();
             return make_shared<RefToValAST>(parseExpr());
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
                    return parseOperator(res);

                return res;
            } else if (if_ident(currTok, IdentStr)) {
                if (nxt == Token::OpP) {
                    return parseFncCall();
                } else if (nxt == Token::Dot) {
                    string type = IdentStr;

                    getNextTok(); // eat type
                    getNextTok(); // eat dot

                    if (tokens.peek().tok == Token::OpP) {
                        return parseTypeFncCall(type);
                    } else {
                        return parseTypeFieldLoad(type);
                    }
                } else {
                    auto res = make_shared<IdentAST>(IdentStr);
                    getNextTok();
                    return res;
                }
            } else if (if_type(currTok, IdentStr) && currTok != Token::Ref) {
                return parseType();
            } else if (currTok == Token::If) {
                return parseIf();
            }
    }

    throw runtime_error("Unknow expr");
}

/** Parse type field storing. */
shared_ptr<BaseAST> ASTParser::parseTypeFieldStore(string st_name /**< Instance name */) {
    string f_name = IdentStr;
    getNextTok();

    assert(currTok == Token::Eq);
    getNextTok();
    shared_ptr<BaseAST> val = parseExpr();
    return make_shared<TypeFieldStoreAST>(st_name, f_name, val);
}

/**Parse type field loading. */
shared_ptr<BaseAST> ASTParser::parseTypeFieldLoad(string st_name /**< Instance name */) {
    string f_name = IdentStr;
    getNextTok();

    auto res = make_shared<TypeFieldLoadAST>(st_name, f_name);

    if (currTok == Token::Operator) {
        return parseOperator(res);
    }

    return res;
}

/** Parse in-place type instance creationg. */
shared_ptr<BaseAST> ASTParser::parseType() {
    string name = IdentStr;
    getNextTok();
    assert(currTok == Token::OpCB);
    getNextTok();
    map<string, shared_ptr<BaseAST>> fields;
    while (currTok != Token::ClCB) {
        assert(if_ident(currTok, IdentStr));
        string f_name = IdentStr;
        getNextTok();
        assert(currTok == Token::Eq);
        getNextTok(); // eat =
        shared_ptr<BaseAST> f_val = parseExpr();
        fields.emplace(f_name, f_val);

        if (currTok != Token::ClCB) {
            assert(currTok == Token::Comma);
            getNextTok();
        }

    }
    getNextTok();
    return make_shared<TypeAST>(name, fields);
}

/** Parse variable creation. */
shared_ptr<BaseAST> ASTParser::parseVar(TType t) {
    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    if (currTok == Token::Semicolon) {
        return make_shared<DeclAST>(name, t, shared_ptr<BaseAST>(nullptr));
    } else if (currTok == Token::Eq) {
        getNextTok();

        return make_shared<DeclAST>(name, t, parseExpr());
    }

    throw runtime_error("Failed variable parse at " + to_string(line) + ":" + to_string(symbol));
}

/** Parse variable assignment. */
shared_ptr<BaseAST> ASTParser::parseAss() {
    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::Eq);

    getNextTok(); // eat =

    return make_shared<AssAST>(name, parseExpr());
}

/** Parse operator usage. */
shared_ptr<BaseAST> ASTParser::parseOperator(shared_ptr<BaseAST> lhs) {
    string name = IdentStr;
    getNextTok(); // eat op
    auto rhs = parseExpr();

    return make_shared<OperatorAST>(name, lhs, rhs);
}

/** Parse type's method call */
shared_ptr<BaseAST> ASTParser::parseTypeFncCall(string st_name) {
    deque<shared_ptr<BaseAST>> args;

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

    auto res = make_shared<FncCallAST>(name, args, st_name);

    res->args.push_front(make_shared<IdentAST>(st_name));

    if (currTok == Token::Operator) {
        return parseOperator(res);
    }

    return res;
}

/** Parse function call*/
shared_ptr<BaseAST> ASTParser::parseFncCall() {
    deque<shared_ptr<BaseAST>> args;

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

    auto res = make_shared<FncCallAST>(name, args);

    if (currTok == Token::Operator) {
        return parseOperator(res);
    }

    return res;
}
