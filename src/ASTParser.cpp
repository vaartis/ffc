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
    getNextTok();
    while (true) {
        try {
            switch(currTok) {
                case Token::OperatorDef:
                    operators.push_back(parseOperatorDef());
                    break;
                case Token::Fnc:
                    functions.push_back(parseFncDef());
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
        } catch (TokenStream::EOFException) {
            break;
        }
    }
}

/** Checks if provided string is a custom type. */
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

/** Parse type at current position. */
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

/** Parses statements, this includes expressions when thir value is discarded. */
unique_ptr<BaseAST> ASTParser::parseStmt(bool test_semicolon = true /** There are cases where we need
                                                                     * to check semicolon explicitly, e.g. ASTParser::parseIf()
                                                                     */) {
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
        else if (test_semicolon)
            getNextTok();
    }

    return move(stmt);
}

/** Parses function implementation for types */
unique_ptr<ImplementAST> ASTParser::parseImplement() {
    getNextTok(); // eat implement

    if (/* TODO: Traits */ false) {
    }

    assert(currTok == Token::For);

    getNextTok();

    assert(if_type(currTok, IdentStr));

    string type = IdentStr;

    getNextTok();

    assert(currTok == Token::OpCB);

    vector<unique_ptr<FncDefAST>> fncs;
    unique_ptr<FncDefAST> destructor;

    getNextTok();

    while (currTok != Token::ClCB) {
        unique_ptr<FncDefAST> curr_fnc;

        if (currTok == Token::Fnc) {
            curr_fnc = parseFncDef();
        } else if (currTok == Token::Destructor) {
            getNextTok(); // eat destructor
            curr_fnc = make_unique<FncDefAST>("destructor", deque<pair<string, TType>>{}, _TType::Void, parseFncBody());
        } else {
            throw runtime_error("Unknown token in IMPLEMENT");
        }

        curr_fnc->args.push_front({"self", type});
        fncs.push_back(move(curr_fnc));
    }

    getNextTok(); // eat }

    return make_unique<ImplementAST>(type, move(fncs));
}

/** Parse while loop */
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

    typedefs.emplace(name, make_shared<TypeDefAST>(name, fields));
}

/** Parses includes. */
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

/** Parses string literals. */
unique_ptr<BaseAST> ASTParser::parseStrLiteral() {
    auto res = make_unique<StrAST>(IdentStr);
    getNextTok();
    return move(res);
}

/** Parses integer literals. */
unique_ptr<BaseAST> ASTParser::parseIntLiteral() {
    auto res = make_unique<IntAST>(stoi(IdentStr));
    getNextTok();
    return move(res);
}

/** Parses floating pointer number literals. */
unique_ptr<BaseAST> ASTParser::parseFloatLiteral() {
    auto res = make_unique<FloatAST>(stof(IdentStr));
    getNextTok();
    return move(res);
}

/** Parses boolean literals. */
unique_ptr<BaseAST> ASTParser::parseBoolLiteral() {
    bool b = (IdentStr == "true") ? true : false;
    auto res = make_unique<BoolAST>(b);
    getNextTok();

    return move(res);
}

/** Parses function externs. */
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

    getNextTok(); // eat ;

    auto res = make_unique<ExternFncAST>(name, args, ret_type);

    return res;
}

/** Parses operator definition. */
unique_ptr<OperatorDefAST> ASTParser::parseOperatorDef() {
    getNextTok(); // eat operator

    assert(currTok == Token::Operator);

    string name = IdentStr;

    getNextTok(); // eat name

    auto params = parseParams();
    assert(params.size() == 2 && "More then two arguments for an operator!");

    TType ret_type = parseTType();

    auto body = parseFncBody();

    return make_unique<OperatorDefAST>(name, params, ret_type, move(body));
}

/** Parse function's parameters. */
deque<pair<string, TType>> ASTParser::parseParams() {
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

    return args;
}

vector<unique_ptr<BaseAST>> ASTParser::parseFncBody() {
    assert(currTok == Token::OpCB);

    getNextTok(); //eat {

    vector<unique_ptr<BaseAST>> body;

    while (currTok != Token::ClCB) {
        body.push_back(parseStmt());
    }

    getNextTok(); // eat }

    return body;
}

/** Parses function definition. */
unique_ptr<FncDefAST> ASTParser::parseFncDef() {
    getNextTok(); // eat fnc

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    deque<pair<string, TType>> args = parseParams();

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = parseTType();
    }

    auto body = parseFncBody();

    auto res = make_unique<FncDefAST>(name, args, ret_type, move(body));
    return res;
}

/** Parses function's return statements. */
unique_ptr<BaseAST> ASTParser::parseRet() {
    getNextTok(); // eat ret

    if (currTok != Token::Semicolon)
        return make_unique<RetAST>(parseExpr());
    else
        return make_unique<RetAST>(nullptr);
}

/** Parses pointer dereferencing. */
unique_ptr<BaseAST> ASTParser::parseVal() {
    getNextTok(); // eat val
    if (currTok == Token::Ident) {
        if (tokens.peek().tok == Token::Eq) {
            return make_unique<ValOfRefAST>(parseAss());
        } else if (tokens.peek().tok == Token::Dot) {
            return make_unique<ValOfRefAST>(parseTypeFieldStore(IdentStr));
        } else {
            return make_unique<ValOfRefAST>(parseExpr());
        }
    } else {
        return make_unique<ValOfRefAST>(parseExpr());
    }
}

/** Parse blocks that can return value.
 *
 * This function returns body of a block and optionally it's result (or nullptr if none)
 */
pair<vector<unique_ptr<BaseAST>>, unique_ptr<BaseAST>> ASTParser::parseBlock() {
    assert(currTok == Token::OpCB);

    getNextTok();

    vector<unique_ptr<BaseAST>> body;
    unique_ptr<BaseAST> value = nullptr;

    while (currTok != Token::ClCB) {
        auto v = parseStmt(false);

        if (currTok == Token::ClCB) {
            BaseAST *inner = v.get();

            if (auto expr = dynamic_cast<Block *>(inner)) {
                if (expr->hasValue())
                    value = move(v);
                else
                    body.push_back(move(v));
            } else {
                value = move(v);
            }
        } else if (currTok == Token::Semicolon) {
            body.push_back(move(v));
            getNextTok();
        } else {
            throw runtime_error("Expected SEMICOLON at " + to_string(line) + ":" + to_string(symbol));
        }
    }

    assert(currTok == Token::ClCB);
    getNextTok();

    return {move(body), move(value)};
}

/** Parse if (and else). */
unique_ptr<BaseAST> ASTParser::parseIf() {
    getNextTok(); // eat if

    auto cond = parseExpr();

    pair<vector<unique_ptr<BaseAST>>, unique_ptr<BaseAST>> parsed_then = parseBlock();
    vector<unique_ptr<BaseAST>> body = move(parsed_then.first);
    unique_ptr<BaseAST> value = move(parsed_then.second);

    if (value != nullptr && currTok != Token::Else) {
        throw runtime_error("If expression returning value must have an else branch");
    }

    vector<unique_ptr<BaseAST>> else_body;
    unique_ptr<BaseAST> else_value = nullptr;
    if (currTok == Token::Else) {
        getNextTok(); // eat else

        pair<vector<unique_ptr<BaseAST>>, unique_ptr<BaseAST>> parsed_else = parseBlock();
        else_body = move(parsed_else.first);
        else_value = move(parsed_else.second);
    }

    return make_unique<IfAST>(move(cond), move(body), move(else_body), move(value), move(else_value));
}

/** Parse expressions. */
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
                    string type = IdentStr;

                    getNextTok(); // eat type
                    getNextTok(); // eat dot

                    if (tokens.peek().tok == Token::OpP) {
                        return parseTypeFncCall(type);
                    } else {
                        return parseTypeFieldLoad(type);
                    }
                } else {
                    auto res = make_unique<IdentAST>(IdentStr);
                    getNextTok();
                    return move(res);
                }
            } else if (if_type(currTok, IdentStr) && currTok != Token::Ref) {
                return parseType();
            } else if (currTok == Token::If) {
                return parseIf();
            }
    }

    throw runtime_error("Unknown expr");
}

/** Parse type field storing. */
unique_ptr<BaseAST> ASTParser::parseTypeFieldStore(string st_name /**< Instance name */) {
    string f_name = IdentStr;
    getNextTok();

    assert(currTok == Token::Eq);
    getNextTok();
    unique_ptr<BaseAST> val = parseExpr();
    return make_unique<TypeFieldStoreAST>(st_name, f_name, move(val));
}

/**Parse type field loading. */
unique_ptr<BaseAST> ASTParser::parseTypeFieldLoad(string st_name /**< Instance name */) {
    string f_name = IdentStr;
    getNextTok();

    auto res = make_unique<TypeFieldLoadAST>(st_name, f_name);

    if (currTok == Token::Operator) {
        return parseOperator(move(res));
    }

    return res;
}

/** Parse in-place type instance creationg. */
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

/** Parse variable creation. */
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

/** Parse variable assignment. */
unique_ptr<BaseAST> ASTParser::parseAss() {
    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;

    getNextTok();

    assert(currTok == Token::Eq);

    getNextTok(); // eat =

    return make_unique<AssAST>(name, parseExpr());
}

/** Parse operator usage. */
unique_ptr<BaseAST> ASTParser::parseOperator(unique_ptr<BaseAST> lhs) {
    string name = IdentStr;
    getNextTok(); // eat op
    auto rhs = parseExpr();

    return make_unique<OperatorAST>(name, move(lhs), move(rhs));
}

/** Parse type's method call */
unique_ptr<BaseAST> ASTParser::parseTypeFncCall(string st_name) {
    deque<unique_ptr<BaseAST>> args;

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

    auto res = make_unique<FncCallAST>(name, move(args), st_name);

    res->args.push_front(make_unique<IdentAST>(st_name));

    if (currTok == Token::Operator) {
        return parseOperator(move(res));
    }

    return move(res);
}

/** Parse function call*/
unique_ptr<BaseAST> ASTParser::parseFncCall() {
    deque<unique_ptr<BaseAST>> args;

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
