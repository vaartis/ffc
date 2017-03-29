#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include <fstream>
#include <vector>
#include <iostream>
#include <map>
#include <algorithm>
#include <sstream>
#include <memory>

#include "parser.hpp"

using namespace std;
using namespace llvm;

string getFileContent(const string pth) {
  ifstream file(pth);
  string content((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

  return content;
}

template<class W, class T> bool isIn(W what, T where) {
    if (find(where.begin(), where.end(), what) == where.end()) {
        return false;
    } else {
        return true;
    }
}

bool isfloat(const std::string &s) {
    istringstream i(s);
    double x;
    if (!(i >> x))
        return false;
    return true;
}

class CodeGen {
    public:
        CodeGen(string fname) {
            module = make_shared<Module>(fname, context);
            builder = make_shared< IRBuilder<> >(context);

            ASTParser parser(getFileContent(fname));

            ast = parser.get_functions();

            AST2IR();

            ofstream file(fname + ".ll");
            raw_os_ostream outfile(file);

            module->print(outfile, nullptr);
        }

    private:
        vector< unique_ptr<FncDefAST> > ast;
        struct LLVMFn {
            LLVMFn(Function *f, vector<Argument *> ars, map<string, Value *> vars, TType tp) : fn(f), arguments(ars), variables(vars), ret_type(tp) {}
            Function *fn;
            vector<Argument *> arguments;
            map<string, Value *> variables;
            TType ret_type;
        };

        map<string, LLVMFn> functions;

        LLVMContext context;

        shared_ptr<Module> module;
        shared_ptr<IRBuilder<> > builder;

        string curr_fn_name;

        void AST2IR();
        Value *genExpr(unique_ptr<BaseAST> obj);
        Type *getLLVMType(TType t);
};

Type *CodeGen::getLLVMType(TType t) {
        switch (t) {
            case TType::Int:
                return builder->getInt32Ty();
            case TType::Void:
                return builder->getVoidTy();
            case TType::Float:
                return builder->getFloatTy();
            case TType::Bool:
                return builder->getInt1Ty();
        }
}

Value *CodeGen::genExpr(unique_ptr<BaseAST> obj) {
    if (auto decl = dynamic_cast<DeclAST *>(obj.get())) {
        auto t = getLLVMType(decl->type);

        Value *alloc = builder->CreateAlloca(t, nullptr, decl->name);

        functions.at(curr_fn_name).variables.emplace(decl->name, alloc);

        if (decl->value != nullptr) {
            Value *v = genExpr(move(decl->value));

            if (t != v->getType())
                throw runtime_error(string("Invalid type assigned to ") + decl->name);

            return builder->CreateStore(v, alloc, false);
        }
    } else if (auto ass = dynamic_cast<AssAST *>(obj.get())) {
        try {
            auto var = functions.at(curr_fn_name).variables.at(ass->name);

            Value *v = genExpr(move(ass->value));

            if (v->getType() != getLLVMType(functions.at(curr_fn_name).ret_type))
                throw runtime_error(string("Invalid type assigned to ") + ass->name);

            return builder->CreateStore(v, var, false);
        } catch (out_of_range) {
            throw runtime_error(string("Undefined variable: ") + ass->name);
        }
    } else if (auto ret = dynamic_cast<RetAST *>(obj.get())) {
        if (ret->value != nullptr) {
            Value *retxpr = genExpr(move(ret->value));

            if (retxpr->getType() != getLLVMType(functions.at(curr_fn_name).ret_type))
                throw runtime_error("Invalid return type");

            return builder->CreateRet(retxpr);
        } else {
            if (functions.at(curr_fn_name).ret_type != TType::Void)
                throw runtime_error("Can't return nothing from a non-void function");
            return builder->CreateRetVoid();
        }
    } else if (auto ca = dynamic_cast<FncCallAST *>(obj.get())) {
        try {
            LLVMFn callee = functions.at(ca->name);

            vector<Value *> argms;
            for (auto &ar : ca->args) {
                argms.push_back(genExpr(move(ar)));
            }
            return builder->CreateCall(callee.fn, argms);
        } catch(out_of_range) {
            throw runtime_error(string("Undefined function: ") + ca->name);
        }
    } else if (auto o = dynamic_cast<IntAST *>(obj.get())) {
        return ConstantInt::get(getLLVMType(TType::Int), o->value);
    } else if (auto f = dynamic_cast<FloatAST *>(obj.get())) {
        return ConstantFP::get(getLLVMType(TType::Float), f->value);
    } else if (auto b = dynamic_cast<BoolAST *>(obj.get())) {
        return ConstantInt::get(getLLVMType(TType::Bool), b->value);
    } else if (auto v = dynamic_cast<IdentAST *>(obj.get())) {
        try {
            auto val = functions.at(curr_fn_name).variables.at(v->value);;
            return builder->CreateLoad(val);
        } catch(out_of_range) {
            throw runtime_error(string("Undefined varible: ") + v->value);
        }
    } else if (auto ifb = dynamic_cast<IfAST *>(obj.get())) {
        BasicBlock *then = BasicBlock::Create(context, "if.then");
        BasicBlock *els = BasicBlock::Create(context, "if.else");
        BasicBlock *ifend = BasicBlock::Create(context, "if.end");

        Value *cond = genExpr(move(ifb->cond));

        Value *c = builder->CreateCondBr(cond, then, els);

        builder->SetInsertPoint(then);
        for (auto &v : ifb->body)
            genExpr(move(v));
        builder->CreateBr(ifend);

        builder->SetInsertPoint(els);
        for (auto &v : ifb->else_body)
            genExpr(move(v));
        builder->CreateBr(ifend);

        builder->SetInsertPoint(ifend);

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(then);
        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(els);
        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(ifend);

        return c;
    }
    throw runtime_error("strange expr");
}

void CodeGen::AST2IR() {
    for (auto &fn : this->ast) {
        cout << "Parsing " << fn->name << endl;
        curr_fn_name = fn->name;

        FunctionType *tp = FunctionType::get(getLLVMType(fn->ret_type), false); 
        Function *fnc(Function::Create(tp, Function::ExternalLinkage, fn->name, module.get()));

        BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
        builder->SetInsertPoint(enter);

        vector<Argument *> fn_args;
        map<string, Value *> fn_vars;

        for (auto &ar : fn->args) {
            string arg_name = ar.first;
            Type *arg_tt = getLLVMType(ar.second);

            auto a = new Argument(arg_tt, "", fnc);
            fn_args.push_back(a);

            Value *alloc = builder->CreateAlloca(a->getType(), nullptr, arg_name);

            fn_vars.emplace(arg_name, alloc);
        }

        functions.emplace(fn->name, LLVMFn(fnc, move(fn_args), fn_vars, fn->ret_type));

        for (auto &expr : fn->body) {
            genExpr(move(expr));
        }
    }
} 

int main(int argc, char *argv[]) {
    if (argc < 2) {
        cout << "No filename" << endl;
        return 1;
    }

    CodeGen codegen(argv[1]);

    return 0;
}
