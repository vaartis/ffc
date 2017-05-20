#pragma once

#include "mpark/variant.hpp"
#include <map>
#include <string>
#include <vector>
#include <sstream>
#include <tuple>
#include "optional.hpp"
#include "ParserShared.hpp"
#include "AST.hpp"
#include "TokenStream.hpp"

using std::string;
using std::shared_ptr;
using std::vector;
using std::map;
using std::multimap;
using std::pair;
using std::exception;
using std::stringstream;
using std::tuple;

class GenericFncInfo {
    public:
        GenericFncInfo(FncDefAST fnc, vector<string> tps) : function(fnc), generic_types(tps) {}
        FncDefAST function;
        vector<string> generic_types;
};

/** Class that parses tokens and forms an AST
 *
 * This class is one of the main classes, it takes a string with program's text,
 * gives it to the inner TokenStream and then tries to form an AST, which then can
 * be extracted from their respective fields. Under the hood, it checks tokens
 * with switch/case, gets the next token and calls a method to parse some kind of structure,
 * like function definition. Then this structures are pushed to respective fields,
 * which are then used by CodeGen.
 *
 * Note that when method that parses something is called, it is this method's resposibility
 * to set ASTParser::currTok to the next token after it.
 */
class ASTParser {
    public:
        ASTParser(string s);
        ~ASTParser() {}

        vector<FncDefAST> functions; /**< Top level functions */
        vector<ExternFncAST> ext_functions; /**< Externs */
        vector<IncludeAST> includes; /**< Includes */
        vector<ImplementAST> impls; /**< Implementations of functions for some types */
        map<string, OperatorDefAST> operators; /**< Operator definitions */
        map<string, TypeDefAST> typedefs; /**< Custom type definitions */

        map<string, vector<string>> generic_types;
        map<string, GenericFncInfo> generic_fncs;
        multimap<string, FncCallAST> generic_uses;
    private:
        TokenStream tokens; /**< Inner TokenStream, which basically holds whole programm's tokenized code */

        vector<string> types; /**< List of strings that are recognized as types */
        string IdentStr; /**< Current token's string representation */
        Token currTok; /**< Current token */

        long symbol = 0, /**< Current symbol in line */
             line = 0;  /**< Current line in file */

        Token getNextTok();

        bool isType(string);

        TType parseTType();

        IncludeAST parseInclude();
        FncDefAST parseFncDef();
        GenericFncInfo parseGenericFncDef();
        ExternFncAST parseExternFnc();
        ImplementAST parseImplement();
        void parseTypeDef();
        void parseOperatorDef();

        #define gen_parse(wh, ...) shared_ptr<BaseAST> parse##wh(__VA_ARGS__);

        string curr_fn_name;
        map<string, TypedName> curr_defined_variables;

        vector<shared_ptr<BaseAST>> parseFncBody();

        deque<pair<string, TType>> parseFncArgs();
        deque<pair<string, TType>> parseFncArgs(map<string, TypedName> &where);

        pair<vector<shared_ptr<BaseAST>>, shared_ptr<BaseAST>> parseBlock();

        void genCompiledIn();

        gen_parse(Var, TType)
        gen_parse(Ass)

        gen_parse(IntLiteral)
        gen_parse(FloatLiteral)
        gen_parse(BoolLiteral)
        gen_parse(StrLiteral)

        gen_parse(RefToVal);

        gen_parse(TypeFieldStore, string)
        gen_parse(TypeFieldLoad, shared_ptr<BaseAST>)
        gen_parse(TypeFncCall, shared_ptr<BaseAST>)

        gen_parse(Ident);
        gen_parse(Type)
        gen_parse(Stmt, bool)
        gen_parse(Val)
        gen_parse(FncCall)
        gen_parse(Expr)
        gen_parse(Ret)
        gen_parse(If)
        gen_parse(While)

        gen_parse(Operator, shared_ptr<BaseAST>)
};
