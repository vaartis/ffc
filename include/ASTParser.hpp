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

        long symbol = 0, line = 0;

        vector<unique_ptr<FncDefAST>> functions;
        vector<unique_ptr<ExternFncAST>> ext_functions;
        vector<unique_ptr<OperatorDefAST>> operators;
        vector<unique_ptr<IncludeAST>> includes;
        map<string, std::shared_ptr<TypeDefAST>> typedefs;

        Token getNextTok();

        bool isType(string);
        TType parseTType();

        unique_ptr<IncludeAST> parseInclude();
        unique_ptr<FncDefAST> parseFncDef();
        unique_ptr<ExternFncAST> parseExternFnc();
        unique_ptr<OperatorDefAST> parseOperatorDef();
        void parseTypeDef();

        #define gen_parse(wh) unique_ptr<BaseAST> parse##wh();

        map<string, TType> parseParams();

        unique_ptr<BaseAST> parseVar(TType t);
        gen_parse(Ass);

        gen_parse(IntLiteral);
        gen_parse(FloatLiteral);
        gen_parse(BoolLiteral);
        gen_parse(StrLiteral);

        gen_parse(TypeFieldStore);
        gen_parse(TypeFieldLoad);
        gen_parse(Type);
        gen_parse(Stmt);
        gen_parse(Val);
        gen_parse(FncCall);
        gen_parse(Expr);
        gen_parse(Ret);
        gen_parse(If);
        gen_parse(While);

        unique_ptr<BaseAST> parseOperator(unique_ptr<BaseAST> lhs);
};
