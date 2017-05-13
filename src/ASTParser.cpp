#include <sstream>
#include <map>
#include <deque>
#include <iostream>
#include <memory>
#include <vector>
#include <algorithm>
#include <tuple>
#include <optional>

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
                functions.push_back(parseFncDef());
                break;
            case Token::OperatorDef:
                operators.push_back(parseOperatorDef());
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
                if (currTok == Token::Generic) {
                    auto r = parseGenericFncDef();
                    generic_fncs.emplace(r.function.name, r);
                    continue;
                }

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

    if (curr_fn_name.size() > 0) {
        try {
            auto curr_f_g = generic_types.at(curr_fn_name);
            if (find(curr_f_g.begin(), curr_f_g.end(), s) != curr_f_g.end()) {
                return true;
            }
        } catch (out_of_range) { return false; }
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
        return TType::withRef(new TType(parseTType()));
    } else {
         throw runtime_error("Expected TYPE at " + to_string(line) + ":" + to_string(symbol));
    }
}

/** Parses statements, this includes expressions when thir value is discarded. */
shared_ptr<BaseAST> ASTParser::parseStmt(bool test_semicolon = true /** There are cases where we need
                                                                     * to check semicolon explicitly, e.g. ASTParser::parseIf()
                                                                     */) {
    bool skip_sm = false;

    shared_ptr<BaseAST> stmt;

    if (if_type(currTok, IdentStr)) {
        TType t = parseTType();

        if (!if_ident(currTok, IdentStr))
            throw runtime_error("Expected IDENT at " + to_string(line) + ":" + to_string(symbol));

        stmt = parseVar(t);
    } else if (if_ident(currTok, IdentStr) && tokens.peek().tok == Token::Eq) {
        stmt = parseAss();
    } else if (if_ident(currTok, IdentStr) && tokens.peek().tok == Token::Dot && tokens.peek(3).tok == Token::Eq) {
        string name = IdentStr;
        getNextTok(); // eat name
        getNextTok(); // eat dot
        stmt = parseTypeFieldStore(name);
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

        curr_fnc.args.push_front({"self", type});
        curr_defined_variables.emplace("self", TypedName("self", type));

        if (currTok == Token::Fnc) {
            curr_fnc = parseFncDef();
        } else if (currTok == Token::Destructor) {
            getNextTok(); // eat destructor

            auto body = parseFncBody();

            curr_fnc = FncDefAST("destructor", deque<pair<string, TType>>{}, _TType::Void, body, curr_defined_variables);
        } else {
            throw runtime_error("Unknown token in IMPLEMENT");
        }

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
    res->expression_type = _TType::Str;
    return res;
}

/** Parses integer literals. */
shared_ptr<BaseAST> ASTParser::parseIntLiteral() {
    auto res = make_shared<IntAST>(stoi(IdentStr));
    getNextTok();
    res->expression_type = _TType::Int;
    return res;
}

/** Parses floating pointer number literals. */
shared_ptr<BaseAST> ASTParser::parseFloatLiteral() {
    auto res = make_shared<FloatAST>(stof(IdentStr));
    getNextTok();
    res->expression_type = _TType::Float;
    return res;
}

/** Parses boolean literals. */
shared_ptr<BaseAST> ASTParser::parseBoolLiteral() {
    bool b = (IdentStr == "true") ? true : false;
    auto res = make_shared<BoolAST>(b);
    getNextTok();
    res->expression_type = _TType::Bool;

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
        auto res = parseStmt();
        body.push_back(res);
        if (auto d = dynamic_cast<DeclAST *>(res.get()))
            curr_defined_variables.emplace(d->name, TypedName(d->name, d->type));
    }

    getNextTok(); // eat }

    return body;
}

deque<pair<string, TType>> ASTParser::parseFncArgs() {
    return parseFncArgs(curr_defined_variables);
}

deque<pair<string, TType>> ASTParser::parseFncArgs(map<string, TypedName> &where) {
    assert(currTok == Token::OpP);

    getNextTok(); // eat (

    deque<pair<string, TType>> args;
    while (currTok != Token::ClP) {
        TType tp = parseTType();

        assert(if_ident(currTok, IdentStr));
        string name = IdentStr;
        getNextTok();

        args.push_back({name, tp});
        where.emplace(name, TypedName(name, tp));

        if (currTok != Token::ClP) {
            assert(currTok == Token::Comma);
            getNextTok();
        }

    }
    getNextTok(); // eat )

    return args;
}

GenericFncInfo ASTParser::parseGenericFncDef() {
    getNextTok(); // eat generic, don't eat fnc

    vector<string> generics;
    while (!(if_ident(currTok,IdentStr) && tokens.peek().tok == Token::OpP)) {
        getNextTok();
        assert(if_ident(currTok, IdentStr));
        generics.push_back(IdentStr);
        getNextTok();
        if (!(if_ident(currTok,IdentStr) && tokens.peek().tok == Token::OpP)) {
            assert(currTok == Token::Comma);
        }
    }

    string name = IdentStr;
    curr_fn_name = name;
    generic_types.emplace(name, generics);

    getNextTok();

    deque<pair<string, TType>> args = parseFncArgs();

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr) || find(generics.begin(), generics.end(), IdentStr) != generics.end()) {
        ret_type = parseTType();
    }

    auto body = parseFncBody();

    auto res = GenericFncInfo(FncDefAST(name, args, ret_type, body, curr_defined_variables), generics);
    curr_defined_variables.clear();
    return res;
}

OperatorDefAST ASTParser::parseOperatorDef() {
    getNextTok(); // eat operator

    assert(currTok == Token::Operator);

    string name = IdentStr;

    getNextTok();

    map<string, TypedName> tmp_curr_defined_variables;
    auto args = parseFncArgs(tmp_curr_defined_variables);

    assert(args.size() == 2 && "More then two arguments for an operator!");

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = parseTType();
    }

    string base_name = name;
    name = args[0].second.to_string() + name + args[1].second.to_string();

    curr_fn_name = name;
    curr_defined_variables = tmp_curr_defined_variables;

    auto body = parseFncBody();

    auto res = OperatorDefAST(name, base_name, args, ret_type, body, curr_defined_variables);
    curr_defined_variables.clear();
    return res;
}

/** Parses function definition. */
FncDefAST ASTParser::parseFncDef() {
    getNextTok(); // eat fnc

    assert(if_ident(currTok, IdentStr));

    string name = IdentStr;
    curr_fn_name = name;

    getNextTok();

    auto args = parseFncArgs();

    TType ret_type = _TType::Void;

    if (if_type(currTok, IdentStr)) {
        ret_type = parseTType();
    }

    auto body = parseFncBody();

    auto res = FncDefAST(name, args, ret_type, body, curr_defined_variables);
    curr_defined_variables.clear();
    return res;
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
            goto exit;
        }
    } else {
    exit:
        auto res = make_shared<ValOfRefAST>(parseExpr());

        auto v = dynamic_cast<Expression *>(res->value.get());
        res->expression_type = v->expression_type;

        return res;
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

    auto res = make_shared<IfAST>(cond, body, else_body, value, else_value);

    if (value != nullptr) {
        assert(else_value != nullptr);

        if (auto v = dynamic_cast<Expression *>(value.get())) {
            res->expression_type = v->expression_type;

            auto e = dynamic_cast<Expression *>(else_value.get()); // else must have a value here
            assert(v->expression_type == e->expression_type && "If and else have different types!");
        } else {
            throw runtime_error("Somehow not an expression");
        }
    }

    return res;
}

shared_ptr<BaseAST> ASTParser::parseIdent() {
    auto res = make_shared<IdentAST>(IdentStr);
    try {
        res->expression_type = curr_defined_variables.at(IdentStr).type;
    } catch (out_of_range) {
        throw runtime_error("Undefined variable " + IdentStr);
    }

    getNextTok();

    return res;
}

shared_ptr<BaseAST> ASTParser::parseRefToVal() {
    getNextTok();
    auto res = make_shared<RefToValAST>(parseExpr());

    if (auto e = dynamic_cast<Expression *>(res->value.get())) {
        res->expression_type = TType::withRef(new TType(e->expression_type));
    } else {
        throw runtime_error("Somehow not an expression");
    }

    return res;
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

    shared_ptr<BaseAST> expr = nullptr;

    switch(currTok) { // Simple casese
        case Token::Val:
            expr = parseVal();
            break;
        case Token::Ref:
             expr = parseRefToVal();
             break;
        case Token::IntLit:
            expr = parseIntLiteral();
            break;
        case Token::FloatLit:
            expr = parseFloatLiteral();
            break;
        case Token::BoolLit:
            expr =  parseBoolLiteral();
            break;
        case Token::StrLit:
            expr = parseStrLiteral();
            break;
        default: // Cases with variables
            if (currTok == Token::OpP) {
                getNextTok(); // eat (
                auto res = parseExpr();
                assert(currTok == Token::ClP);
                getNextTok(); // eat )

                if (currTok == Token::Operator) // FIXME
                    expr = parseOperator(res);

                expr = res;
            } else if (if_ident(currTok, IdentStr)) {
                if (nxt == Token::OpP) {
                    expr = parseFncCall();
                } else {
                    expr = parseIdent();
                }
            } else if (if_type(currTok, IdentStr) && currTok != Token::Ref && tokens.peek().tok == Token::OpCB) {
                expr = parseType();
            } else if (currTok == Token::If) {
                expr = parseIf();
            }
    }

    if (currTok == Token::Dot) {
        getNextTok(); // eat dot

        if (currTok == Token::Ident && tokens.peek().tok == Token::OpP) {
            expr = parseTypeFncCall(expr);
        } else if (if_ident(currTok, IdentStr) && tokens.peek().tok != Token::OpP) {
            expr = parseTypeFieldLoad(expr);
        }
    }

    if (expr == nullptr)
        throw runtime_error("Unknow expr");

    return expr;
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
shared_ptr<BaseAST> ASTParser::parseTypeFieldLoad(shared_ptr<BaseAST> st /**< Instance name */) {
    string f_name = IdentStr;
    getNextTok();

    auto from_expr = dynamic_cast<Expression *>(st.get());

    assert(from_expr->expression_type.isCustom());

    auto res = make_shared<TypeFieldLoadAST>(st, f_name);

    TypeDefAST type_type = typedefs.at(from_expr->expression_type.to_string());
    auto field_type = find_if(type_type.fields.begin(), type_type.fields.end(), [&](pair<string, TType> f) { return f.first == f_name; });
    if (field_type != type_type.fields.end())
        res->expression_type = field_type->second;
    else
        throw runtime_error("No field " + f_name + " in " + from_expr->expression_type.to_string());

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

    auto res = make_shared<TypeAST>(name, fields);

    if(isType(name)) {
        try {
            auto curr_f_g = generic_types.at(curr_fn_name);
            if (find(curr_f_g.begin(), curr_f_g.end(), name) != curr_f_g.end()) {
                res->expression_type = GenericType(name);
            } else {
                res->expression_type = name;
            }
        } catch (out_of_range) {
            res->expression_type = name;
        }
    } else {
        throw runtime_error("In-place creation of undefined type " + name);
    }

    return res;
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

    auto res = make_shared<OperatorAST>(name, lhs, rhs);

    if (auto l = dynamic_cast<Expression *>(lhs.get()), r = dynamic_cast<Expression *>(rhs.get()); l && r) {
        string op_full = l->expression_type.to_string() + name + r->expression_type.to_string();

        if (auto op = find_if(operators.begin(), operators.end(), [&](OperatorDefAST o) { return o.name == op_full; }); op != operators.end()) {
            res->expression_type = op->ret_type;
        }
    } else {
        throw runtime_error("Somehow not an expression");
    }

    return res;
}

/** Parse type's method call */
shared_ptr<BaseAST> ASTParser::parseTypeFncCall(shared_ptr<BaseAST> st) {
    assert(if_ident(currTok, IdentStr));
    string f_name = IdentStr;

    getNextTok(); // eat name
    getNextTok(); // eat (

    deque<shared_ptr<BaseAST>> args;
    while (currTok != Token::ClP) {
        args.push_back(parseExpr());
        if (currTok != Token::ClP) {
            assert(currTok == Token::Comma);
            getNextTok();
        }
    }

    getNextTok(); // eat }

    auto expr = dynamic_cast<Expression *>(st.get());

    assert(expr->expression_type.isCustom());

    string tp_name = expr->expression_type.to_string();

    auto res = make_shared<FncCallAST>(f_name, args, tp_name);
    res->args.push_front(make_shared<IdentAST>(tp_name));


    optional<TType> etp;
    for (auto impl : impls) {
        for (auto fnc : impl.fncs) {
            if (fnc.name == f_name)
                etp = fnc.ret_type;
            goto loope;
        }
    }
loope:
    if (!etp.has_value()) {
        throw runtime_error("No function " + f_name + " in type " + tp_name);
    } else {
        res->expression_type = etp.value();
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

    if (auto f = find_if(functions.begin(), functions.end(), [&](FncDefAST f) { return f.name == res->name; }); f != functions.end()) {
        res->expression_type = f->ret_type;
    } else if (auto f = find_if(generic_fncs.begin(),
                                generic_fncs.end(), [&](pair<string, GenericFncInfo> f) { return f.first == res->name; }); f != generic_fncs.end()) {
        res->expression_type = f->second.function.ret_type;
    } else if (auto f = find_if(ext_functions.begin(), ext_functions.end(), [&](ExternFncAST f) { return f.name == res->name; }); f != ext_functions.end()){
        res->expression_type = f->ret_type;
    } else {
        throw runtime_error("Undefined function " + res->name);
    }

    if (find_if(generic_fncs.begin(), generic_fncs.end(), [&](pair<string, GenericFncInfo> p) { return p.first == name; }) != generic_fncs.end() )
        generic_uses.emplace(name, *res);

    if (currTok == Token::Operator) {
        return parseOperator(res);
    }

    return res;
}
