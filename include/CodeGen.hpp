#pragma once

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/IR/PassManager.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "llvm/Linker/Linker.h"

#include <fstream>
#include <vector>
#include <iostream>
#include <map>
#include <algorithm>
#include <sstream>
#include <memory>
#include <functional>
#include <optional>
#include "mpark/variant.hpp"

#include "ASTParser.hpp"

using namespace llvm; // i am sorry

using std::ifstream;
using std::istreambuf_iterator;
using std::runtime_error;

static LLVMContext context;
static bool compiledInEmited = false;

static string getFileContent(const string pth) {
  ifstream file(pth);

  if (file.fail()) {
      throw runtime_error(string("File does not exist: ") + pth);
  }

  string content((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

  return content;
}

class CodeGen {
    public:
        CodeGen(string s, bool is_from_string = false) {
            if (!is_from_string) {
                fname = s.substr(0, s.length() - 3);
            }

            module = std::make_unique<Module>(fname, context); // .ff
            builder = make_shared<IRBuilder<>>(context);

            string parser_text;
            if (!is_from_string) {
                parser_text = getFileContent(fname + ".ff");
            } else {
                parser_text = s;
            }
            ASTParser parser(parser_text);

            ast = parser.functions;
            exts = parser.ext_functions;
            ops = parser.operators;
            incls = parser.includes;
            impls = parser.impls;
            typedefs = parser.typedefs;
            generic_fncs = parser.generic_fncs;
            generic_uses = parser.generic_uses;

            AST2IR();
        }

        unique_ptr<Module> module;
        vector<unique_ptr<Module>> includes;
    protected:
        string fname;

        vector<FncDefAST> ast;

        map<string, GenericFncInfo> generic_fncs;
        multimap<string, FncCallAST> generic_uses;
        map<string, TType> curr_generics_types;

        vector<string> real_names;

        vector<ExternFncAST> exts;
        vector<OperatorDefAST> ops;
        vector<IncludeAST> incls;
        vector<ImplementAST> impls;
        map<string, TypeDefAST> typedefs;

        Type *getLLVMType(TType t); // built-in

        struct LLVMCall {
            public:
                LLVMCall(string n, deque<Type *> ar, char c) : name(n), args(ar), type(c) {}
                string name;
                deque<Type *> args;
                char type;
        };

        struct LLVMFn {
            public:
                LLVMFn(Function *f, map<string, Value *> vars, Type *tp) : fn(f), variables(vars), ret_type(tp) {}
                LLVMFn() {}
                Function *fn;
                map<string, Value *> variables;
                Type *ret_type;

                BasicBlock *exit_block;
                Value *exit_value;
        };

        struct LLVMStruct {
            public:
                LLVMStruct(vector<pair<string, Type *>> flds, StructType *tp) : fields(flds), type(tp) {}
                LLVMStruct() {}

                StructType *type;
                vector<pair<string, Type *>> fields;
        };

        map<string, LLVMFn> functions;
        map<string, LLVMStruct> struct_types;

        shared_ptr<IRBuilder<>> builder;

        string curr_fn_name;

        string mangle(LLVMCall f, optional<Type *> tp);
        string mangle(Call *f, optional<string> tp);


        void genFnc(FncDefAST fn, optional<string> type, bool skipcheck);
        void genFnc(OperatorDefAST fn, optional<string> type, bool skipcheck);

        void AST2IR();
        void genCompiledIn();
        Value *genExpr(shared_ptr<BaseAST> obj, bool noload);
};
