#pragma once

#include <variant>
#include <map>
#include <string>
#include <vector>
#include <sstream>

#include "ParserShared.hpp"
#include "AST.hpp"
#include "TokenStream.hpp"

class TokenStream;

using std::string;
using std::unique_ptr;
using std::vector;
using std::map;
using std::pair;
using std::exception;
using std::stringstream;

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

        Token getNextTok();

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

        unique_ptr<BaseAST> parseTypeFieldStore();
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
