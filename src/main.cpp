#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"

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

#include "parser.hpp"

using namespace std;
using namespace llvm;

LLVMContext context;

string getFileContent(const string pth) {
  ifstream file(pth);

  if (file.fail()) {
      throw runtime_error(string("File does not exist: ") + pth);
  }

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
        CodeGen(string fnm) {
            fname = fnm.substr(0, fnm.length() - 3);

            module = make_shared<Module>(fname, context); // .ff
            builder = make_shared< IRBuilder<> >(context);

            ASTParser parser(getFileContent(fname + ".ff"));

            ast = parser.get_functions();
            exts = parser.get_ext_functions();
            ops = parser.get_operators();
            incls = parser.get_includes();

            AST2IR();

            ofstream file(fname + ".ll");
            raw_os_ostream outfile(file);

            module->print(outfile, nullptr);
        }

    private:
        string fname;
        shared_ptr<Module> module;

        vector<unique_ptr<FncDefAST>> ast;
        vector<unique_ptr<ExternFncAST>> exts;
        vector<unique_ptr<OperatorDefAST>> ops;
        vector<unique_ptr<IncludeAST>> incls;

        struct LLVMFn {
            LLVMFn(Function *f, map<string, Value *> vars, Type *tp) : fn(f), variables(vars), ret_type(tp) {}
            Function *fn;
            map<string, Value *> variables;
            Type *ret_type;
        };

        map<string, LLVMFn> functions;

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
            case TType::Str:
                return builder->getInt8PtrTy();
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

            if (v->getType() != var->getType()->getContainedType(0)) // var is always <type>* (alloca type), so we check the type it points to
                throw runtime_error(string("Invalid type assigned to ") + ass->name);

            return builder->CreateStore(v, var, false);
        } catch (out_of_range) {
            throw runtime_error(string("Undefined variable: ") + ass->name);
        }
    } else if (auto ret = dynamic_cast<RetAST *>(obj.get())) {
        if (ret->value != nullptr) {
            Value *retxpr = genExpr(move(ret->value));

            if (retxpr->getType() != functions.at(curr_fn_name).ret_type)
                throw runtime_error("Invalid return type");

            return builder->CreateRet(retxpr);
        } else {
            if (functions.at(curr_fn_name).ret_type != FunctionType::get(getLLVMType(TType::Void), false))
                throw runtime_error("Can't return nothing from a non-void function");
            return builder->CreateRetVoid();
        }
    } else if (auto ca = dynamic_cast<FncCallAST *>(obj.get())) {
        try {
            #define gen_basic(fname, fn)\
                if (ca->name == (string("_basic_") + fname) && ca->args.size() == 2) { \
                    Value *lhs = genExpr(move(ca->args[0]));\
                    Value *rhs = genExpr(move(ca->args[1]));\
                    return fn(lhs, rhs);\
                }

            gen_basic("iadd", builder->CreateAdd);
            gen_basic("isub", builder->CreateSub);
            gen_basic("imul", builder->CreateMul);
            gen_basic("idiv", builder->CreateSDiv);

            gen_basic("fadd", builder->CreateFAdd);
            gen_basic("fsub", builder->CreateFSub);
            gen_basic("fmul", builder->CreateFMul);
            gen_basic("fdiv", builder->CreateFDiv);

            LLVMFn callee = functions.at(ca->name);

            if (callee.fn->getArgumentList().size() != ca->args.size())
                throw runtime_error("Wrong number of args for function call: " + ca->name);

            vector<Value *> argms;
            for (int i = 0; i < ca->args.size(); i++) {
                Value *this_arg = genExpr(move(ca->args[i]));

                int j = 0;
                for (auto &ar : callee.fn->getArgumentList())
                    if (j++ == i)
                        if (ar.getType() != this_arg->getType())
                            throw runtime_error("Wrong type in function call: " + ca->name);

                argms.push_back(this_arg);
            }

            return builder->CreateCall(callee.fn, argms);
        } catch(out_of_range) {
            throw runtime_error(string("Undefined function: ") + ca->name);
        }
    } else if (auto op = dynamic_cast<OperatorAST *>(obj.get())) {
        string useless;
        raw_string_ostream name_s(useless);
        Value *lhs = genExpr(move(op->lhs));
        Value *rhs = genExpr(move(op->rhs));

        lhs->getType()->print(name_s);
        name_s << op->name;
        rhs->getType()->print(name_s);

        string name = name_s.str();

        try {
            LLVMFn callee = functions.at(name);

            return builder->CreateCall(callee.fn, {move(lhs), move(rhs)});
        } catch(out_of_range) {
            throw runtime_error("Operator not defined: " + name);
        }
    } else if (auto o = dynamic_cast<IntAST *>(obj.get())) {
        return ConstantInt::get(getLLVMType(TType::Int), o->value);
    } else if (auto f = dynamic_cast<FloatAST *>(obj.get())) {
        return ConstantFP::get(getLLVMType(TType::Float), f->value);
    } else if (auto b = dynamic_cast<BoolAST *>(obj.get())) {
        return ConstantInt::get(getLLVMType(TType::Bool), b->value);
    } else if (auto s = dynamic_cast<StrAST *>(obj.get())) {
        return builder->CreateGlobalStringPtr(s->value);
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
    for (auto &in : this->incls) {
        for (string mod : in->modules) {
            CodeGen m(mod + ".ff");
            for (auto &o_fn : m.module->getFunctionList()) {
                Function *ef = cast<Function>(module->getOrInsertFunction(o_fn.getName(), o_fn.getFunctionType()));
                functions.emplace(o_fn.getName(), LLVMFn(ef, map<string, Value*>(), o_fn.getReturnType()));
            }
        }
    }

    for (auto &ex : this->exts) {
        vector<Type *> ext_args;
        for (auto &ar : ex->args) {
            ext_args.push_back(getLLVMType(ar));
        }

        FunctionType *extTy = FunctionType::get(getLLVMType(ex->ret_type), ext_args, false);

        Function *ext = cast<Function>(module->getOrInsertFunction(ex->name, extTy));

        functions.emplace(ex->name, LLVMFn(ext, map<string, Value *>(), getLLVMType(ex->ret_type)));
    }

    for (auto &op : this->ops) {
        Type *lhs_type = getLLVMType(op->lhs.second);
        Type *rhs_type = getLLVMType(op->rhs.second);

        string useless;
        raw_string_ostream name_s(useless);;

        lhs_type->print(name_s);
        name_s << op->name;
        rhs_type->print(name_s);

        string name = name_s.str();
        curr_fn_name = name;

        cout << "Parsing operator " << fname << "::" << name << endl;

        FunctionType *tp = FunctionType::get(getLLVMType(op->ret_type), {lhs_type, rhs_type},false);
        Function *fnc = Function::Create(tp, Function::ExternalLinkage, name, module.get());

        BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
        builder->SetInsertPoint(enter);

        vector<Argument *> fn_args;
        map<string, Value *> fn_vars;

        Value *alloc_lhs = builder->CreateAlloca(lhs_type, nullptr, op->lhs.first);
        builder->CreateStore(&*fnc->arg_begin(), alloc_lhs);
        fn_vars.emplace(op->lhs.first, alloc_lhs);

        Value *alloc_rhs = builder->CreateAlloca(rhs_type, nullptr, op->rhs.first);
        builder->CreateStore(&*(--fnc->arg_end()), alloc_rhs);
        fn_vars.emplace(op->rhs.first, alloc_rhs);

        functions.emplace(name, LLVMFn(fnc, fn_vars, getLLVMType(op->ret_type)));

        for (auto &expr : op->body) {
            genExpr(move(expr));
        }
    }

    for (auto &fn : this->ast) {
        cout << "Parsing " << fname << "::" << fn->name << endl;
        curr_fn_name = fn->name;

        vector<Type *> fn_args;
        map<string, Value *> fn_vars;

        for (auto &ar : fn->args)
            fn_args.push_back(getLLVMType(ar.second));

        FunctionType *tp = FunctionType::get(getLLVMType(fn->ret_type), fn_args, false);
        Function *fnc = Function::Create(tp, Function::ExternalLinkage, fn->name, module.get());

        BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
        builder->SetInsertPoint(enter);

        auto arg = fnc->arg_begin();
        auto fnc_arg = fn->args.begin();
        for (; arg != fnc->arg_end(); arg++, fnc_arg++) {
            Value *alloc = builder->CreateAlloca(getLLVMType(fnc_arg->second), nullptr, fnc_arg->first);
            builder->CreateStore(&*arg, alloc);
            fn_vars.emplace(fnc_arg->first, alloc);
        }

        functions.emplace(fn->name, LLVMFn(fnc, fn_vars, getLLVMType(fn->ret_type)));

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
