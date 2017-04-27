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
#include <variant>

#include "ASTParser.hpp"

using namespace std;

using namespace llvm;

static LLVMContext context;
static bool compiledInEmited = false;

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

            module = std::make_unique<Module>(fname, context); // .ff
            builder = make_shared<IRBuilder<>>(context);

            ASTParser parser(getFileContent(fname + ".ff"));

            ast = move(parser.functions);
            exts = move(parser.ext_functions);
            ops = move(parser.operators);
            incls = move(parser.includes);
            impls = move(parser.impls);
            typedefs = parser.typedefs;

            AST2IR();
        }

        unique_ptr<Module> module;
        vector<unique_ptr<Module>> includes;
    private:
        string fname;

        vector<unique_ptr<FncDefAST>> ast;
        vector<string> real_names;

        vector<unique_ptr<ExternFncAST>> exts;
        vector<unique_ptr<OperatorDefAST>> ops;
        vector<unique_ptr<IncludeAST>> incls;
        vector<unique_ptr<ImplementAST>> impls;
        map<string, shared_ptr<TypeDefAST>> typedefs;

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

        string mangle(LLVMCall f, Type *tp);
        string mangle(Call *f, string tp);


        void genFnc(FncDefAST &fnc, string tp);
        void AST2IR();
        void genCompiledIn();
        Value *genExpr(unique_ptr<BaseAST> obj, bool noload);

        Type *getLLVMType(TType t); // build-in
        Type *getLLVMType(string s); // custom
};

string CodeGen::mangle(LLVMCall f, Type *tp = nullptr) {
    string res_name("_FF" + string(1, f.type));

    if (tp != nullptr) {
        while (tp->isPointerTy())
            tp = tp->getContainedType(0);
        string st_name = tp->getStructName();

        res_name += "T" + to_string(st_name.length()) + st_name;
    }

    res_name += "N" + to_string(f.name.length()) + f.name;

    res_name += "A";
    for (auto &a : f.args) {
        string arg_name;
        if (a->isStructTy()) {
            arg_name = a->getStructName();
        } else {

            string useless;
            raw_string_ostream name_s(useless);

            a->print(name_s);

            arg_name = name_s.str();
        }

        res_name += to_string(arg_name.length()) + arg_name;
    }

    return res_name;
}

// _FF beginning
// N name
// A args
// T if function is a member of type
string CodeGen::mangle(Call *f, string tp = "") {
    string res_name("_FF" + string(1, f->getType()));

    if (tp.length() != 0) {
        res_name += "T" + to_string(tp.length()) + tp;
    }

    res_name += "N" + to_string(f->getName().length()) + f->getName();

    res_name += "A";
    for (auto &a : f->getArgs()) {
        string arg_name;
        if (getLLVMType(a.second)->isStructTy()) {
            arg_name = getLLVMType(a.second)->getStructName();
        } else {
            string useless;
            raw_string_ostream name_s(useless);
            Type *tt = getLLVMType(a.second);

            tt->print(name_s);

            arg_name = name_s.str();
        }

        res_name += to_string(arg_name.length()) + arg_name;
    }

    return res_name;
}

Type *CodeGen::getLLVMType(TType t) {
    if (t.isRef()) {
        Type *res;
        while (t.isRef()) {
            res = getLLVMType(*t.referenceTo)->getPointerTo();
            t = *t.referenceTo;
        }
        return res;
    }


    return visit([&](auto arg) -> Type * {
              using T = decltype(arg);
              if constexpr (is_same_v<_TType, T>) {
                  switch (arg) {
                      case _TType::Int:
                          return builder->getInt32Ty();
                      case _TType::Void:
                          return builder->getVoidTy();
                      case _TType::Float:
                          return builder->getFloatTy();
                      case _TType::Bool:
                          return builder->getInt1Ty();
                      case _TType::Str:
                          return builder->getInt8PtrTy();
                      }
              } else if constexpr (is_same_v<string, T>) {
                  try {
                      return struct_types.at(arg).type;
                  } catch (out_of_range) {
                      throw runtime_error("Unknown type: " + arg);
                  }
              } else {
                  throw runtime_error("Bug");
              }
          }, t.inner);
}

Value *CodeGen::genExpr(unique_ptr<BaseAST> obj, bool noload = false) {
    if (obj == nullptr)
        throw runtime_error("OBJECT IS A NULL POINTER");

    if (auto v = dynamic_cast<ValOfRefAST *>(obj.get())) {
        if (auto in = dynamic_cast<AssAST *>(v->value.get())) {
            Value *var;
            try {
                var = functions.at(curr_fn_name).variables.at(in->name);
            }  catch (out_of_range) {
                throw runtime_error(string("Undefined variable: ") + in->name);
            }

            Value *value = genExpr(move(in->value));

            Value *pointee = builder->CreateLoad(var);
            builder->CreateStore(value, pointee);

            return value;
        } else {
            return builder->CreateLoad(genExpr(move(v->value)));
        }
    } else if (auto r = dynamic_cast<RefToValAST *>(obj.get())) {
        return genExpr(move(r->value), true);
    } else if (auto decl = dynamic_cast<DeclAST *>(obj.get())) {
        auto t = getLLVMType(decl->type);

        Value *alloc = builder->CreateAlloca(t, nullptr, decl->name);

        functions.at(curr_fn_name).variables.emplace(decl->name, alloc);

        if (decl->value != nullptr) {
            Value *v = genExpr(move(decl->value));

            if (t != v->getType())
                throw runtime_error(string("Invalid type assigned to ") + decl->name);

            builder->CreateStore(v, alloc, false);

            return v;
        }
        return alloc;
    } else if (auto ass = dynamic_cast<AssAST *>(obj.get())) {
        try {
            auto var = functions.at(curr_fn_name).variables.at(ass->name);

            Value *v = genExpr(move(ass->value));

            if (v->getType() != var->getType()->getContainedType(0)) // var is always <type>* (alloca type), so we check the type it points to
                throw runtime_error(string("Invalid type assigned to ") + ass->name);

            builder->CreateStore(v, var, false);

            return v;
        } catch (out_of_range) {
            throw runtime_error(string("Undefined variable: ") + ass->name);
        }
    } else if (auto ret = dynamic_cast<RetAST *>(obj.get())) {
        if (ret->value != nullptr) {
            Value *retxpr = genExpr(move(ret->value));

            if (retxpr->getType() != functions.at(curr_fn_name).ret_type)
                throw runtime_error("Invalid return type");

            functions.at(curr_fn_name).exit_value = retxpr;
        } else {
            if (functions.at(curr_fn_name).ret_type != builder->getVoidTy())
                throw runtime_error("Can't return nothing from a non-void function");
        }

        return builder->CreateBr(functions.at(curr_fn_name).exit_block);
    } else if (auto ca = dynamic_cast<FncCallAST *>(obj.get())) {
        LLVMFn callee;
        vector<Value *> argms;

        try {
            deque<Type *> mangle_args;
            for (auto &arg : ca->args) {
                auto ar = genExpr(move(arg));

                mangle_args.push_back(ar->getType());

                argms.push_back(ar);
            }

            string name = ca->name;
            if (find_if(exts.begin(), exts.end(), [&](unique_ptr<ExternFncAST> &e){ return e->name == ca->name; }) == exts.end()) {
                if (ca->type.length() == 0)
                    name = mangle(LLVMCall(ca->name, mangle_args, 'F'));
                else
                    name = mangle(LLVMCall(ca->name, mangle_args, 'F'), functions.at(curr_fn_name).variables.at(ca->type)->getType());
            }

            callee = functions.at(name);
        } catch(out_of_range) {
            throw runtime_error(string("Undefined function: ") + ca->name);
        }

        if (callee.fn->arg_size() != ca->args.size())
            throw runtime_error("Wrong number of args for function call: " + ca->name);

        for (int i = 0; i < argms.size(); i++) {
            Value *this_arg = argms[i];

            int j = 0;
            for (auto &ar : callee.fn->args())
                if (j++ == i)
                    if (ar.getType() != this_arg->getType())
                        throw runtime_error("Wrong type in function call: " + ca->name);
        }

        return builder->CreateCall(callee.fn, argms);
    } else if (auto op = dynamic_cast<OperatorAST *>(obj.get())) {
        Value *lhs = genExpr(move(op->lhs));
        Value *rhs = genExpr(move(op->rhs));

        string name = mangle(LLVMCall(op->name, deque<Type *>{lhs->getType(), rhs->getType()}, 'O'));

        try {
            LLVMFn callee = functions.at(name);

            return builder->CreateCall(callee.fn, {move(lhs), move(rhs)});
        } catch(out_of_range) {
            throw runtime_error("Operator not defined: " + name);
        }
    } else if (auto st = dynamic_cast<TypeAST *>(obj.get())) {
        try {
            LLVMStruct s = struct_types.at(st->name);

            Value *tmp_struct = builder->CreateAlloca(s.type); // should be opimized out

            for (auto &field : st->fields) {
                string f_name = field.first;
                Value *f_val = genExpr(move(field.second));

                auto index_i = find_if(s.fields.begin(), s.fields.end(), [&](pair<string, Type *> val){
                                                                          if (val.first == f_name)
                                                                              return true;
                                                                          else
                                                                              return false;
                                                                      });

                if (index_i == s.fields.end())
                    throw runtime_error("Unknown field: " + f_name);

                unsigned int index = distance(s.fields.begin(), index_i);

                Value * this_field = builder->CreateStructGEP(s.type, tmp_struct, index);
                builder->CreateStore(f_val, this_field);
            }
            return builder->CreateLoad(tmp_struct);

        } catch (out_of_range) {
            throw runtime_error("Unknown custom type: " + st->name);
        }
    } else if (auto st = dynamic_cast<TypeFieldStoreAST  *>(obj.get())) {
        Value *val;

        try {
            val = functions.at(curr_fn_name).variables.at(st->struct_name);
        } catch(out_of_range) {
            throw runtime_error(string("Undefined varible: ") + st->struct_name);
        }

        while (val->getType()->getContainedType(0)->isPointerTy())
            val = builder->CreateLoad(val);
        string type_name = val->getType()->getContainedType(0)->getStructName();

        LLVMStruct s;

        try {
            s = struct_types.at(type_name);
        } catch (out_of_range) {
            throw runtime_error("Undefined custom type: " + type_name);
        }

        auto index_i = find_if(s.fields.begin(), s.fields.end(), [&](pair<string, Type *> v){
                                                                     if (v.first == st->field_name)
                                                                         return true;
                                                                     else
                                                                         return false;
                                                                 });
        if (index_i == s.fields.end())
            throw runtime_error("Unknown field: " + st->field_name);

        unsigned int index = distance(s.fields.begin(), index_i);

        Value *this_field = builder->CreateStructGEP(s.type, val, index);

        return builder->CreateStore(genExpr(move(st->value)), this_field);
    } else if (auto st = dynamic_cast<TypeFieldLoadAST  *>(obj.get())) {
        try {
            auto val = functions.at(curr_fn_name).variables.at(st->struct_name);

            Type *tmpt = val->getType();
            while (tmpt->isPointerTy())
                tmpt = tmpt->getContainedType(0);
            string type_name = tmpt->getStructName();

            LLVMStruct s = struct_types.at(type_name);

            auto index_i = find_if(s.fields.begin(), s.fields.end(), [&](pair<string, Type *> v){
                                                                         if (v.first == st->field_name)
                                                                             return true;
                                                                         else
                                                                             return false;
                                                                     });

                if (index_i == s.fields.end())
                    throw runtime_error("Unknown field: " + st->field_name);

                unsigned int index = distance(s.fields.begin(), index_i);

                Value *this_field = builder->CreateStructGEP(s.type, val, index);
                return builder->CreateLoad(this_field);

        } catch(out_of_range) {
            throw runtime_error(string("Undefined varible: ") + st->struct_name);
        }
    } else if (auto o = dynamic_cast<IntAST *>(obj.get())) {
        return ConstantInt::get(getLLVMType(_TType::Int), o->value);
    } else if (auto f = dynamic_cast<FloatAST *>(obj.get())) {
        return ConstantFP::get(getLLVMType(_TType::Float), f->value);
    } else if (auto b = dynamic_cast<BoolAST *>(obj.get())) {
        return ConstantInt::get(getLLVMType(_TType::Bool), b->value);
    } else if (auto s = dynamic_cast<StrAST *>(obj.get())) {
        return builder->CreateGlobalStringPtr(s->value);
    } else if (auto v = dynamic_cast<IdentAST *>(obj.get())) {
        try {
            auto val = functions.at(curr_fn_name).variables.at(v->value);
            return noload ? val : builder->CreateLoad(val);
        } catch(out_of_range) {
            throw runtime_error(string("Undefined varible: ") + v->value);
        }
    } else if (auto wh = dynamic_cast<WhileAST *>(obj.get())) {
        BasicBlock *l_cond = BasicBlock::Create(context, "loop.cond");
        BasicBlock *l_while  = BasicBlock::Create(context, "loop.while");
        BasicBlock *l_end = BasicBlock::Create(context, "loop.end");

        builder->CreateBr(l_cond);

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(l_cond);

        builder->SetInsertPoint(l_cond);
        Value *cond = genExpr(move(wh->cond));
        builder->CreateCondBr(cond, l_while, l_end);

        builder->SetInsertPoint(l_while);
        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(l_while);
        for (auto &el : wh->body) {
            genExpr(move(el));
        }
        builder->CreateBr(l_cond);

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(l_end);
        builder->SetInsertPoint(l_end);

        return cond;

    } else if (auto ifb = dynamic_cast<IfAST *>(obj.get())) {
        BasicBlock *then = BasicBlock::Create(context, "if.then");
        BasicBlock *els = BasicBlock::Create(context, "if.else");
        BasicBlock *ifend = BasicBlock::Create(context, "if.end");

        Value *cond = genExpr(move(ifb->cond));

        Value *c = builder->CreateCondBr(cond, then, els);

        Value *if_val = nullptr, *else_val = nullptr;

        // then

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(then);
        builder->SetInsertPoint(then);
        for (auto &v : ifb->body)
            genExpr(move(v));

        if (ifb->value != nullptr) {
            if_val = genExpr(move(ifb->value));
        }

        then = builder->GetInsertBlock(); // update for phi

        builder->CreateBr(ifend);

        // else

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(els);

        builder->SetInsertPoint(els);
        for (auto &v : ifb->else_body)
            genExpr(move(v));

        if (ifb->else_value != nullptr) {
            else_val = genExpr(move(ifb->else_value));
        }

        els = builder->GetInsertBlock(); // update for phi

        builder->CreateBr(ifend);

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(ifend);
        builder->SetInsertPoint(ifend);

        if (if_val != nullptr && else_val != nullptr) {
            assert(if_val->getType() == else_val->getType());

            PHINode *if_res = builder->CreatePHI(if_val->getType(), 2, "if.expr.res");
            if_res->addIncoming(if_val, then);
            if_res->addIncoming(else_val, els);

            return if_res;
        }

        return cond;
    }

    throw runtime_error("strange expr");
}

/** Generate a function from an AST node.
 *
 * Inserts exiting and entering points, exiting value, mangles function's name & adds
 * it to global function list.
 */
void CodeGen::genFnc(FncDefAST &fn, string type = "") {
        if (find(real_names.begin(), real_names.end(), fn.name) == real_names.end())
            real_names.push_back(fn.name);
        else
            throw runtime_error("Redifinition of a function: " + fn.name);

        if (fn.name != "main") {
            fn.name = mangle(&fn, type);
        }

        curr_fn_name = fn.name;

        vector<Type *> fn_args;
        map<string, Value *> fn_vars;

        for (auto &ar : fn.args)
            fn_args.push_back(getLLVMType(ar.second));

        FunctionType *tp = FunctionType::get(getLLVMType(fn.ret_type), fn_args, false);
        Function *fnc = Function::Create(tp, Function::ExternalLinkage, fn.name, module.get());

        BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
        BasicBlock *exit = BasicBlock::Create(context, "exit", fnc);

        builder->SetInsertPoint(enter);

        Value *exit_value = nullptr;
        if (fn.ret_type != _TType::Void) {
            exit_value = builder->CreateAlloca(getLLVMType(fn.ret_type), nullptr, "ret_value");
        };

        auto arg = fnc->arg_begin();
        auto fnc_arg = fn.args.begin();
        for (; arg != fnc->arg_end(); arg++, fnc_arg++) {
            Value *alloc = builder->CreateAlloca(getLLVMType(fnc_arg->second), nullptr, fnc_arg->first);
            builder->CreateStore(&*arg, alloc);
            fn_vars.emplace(fnc_arg->first, alloc);
        }

        functions.emplace(fn.name, LLVMFn(fnc, fn_vars, getLLVMType(fn.ret_type)));
        functions.at(curr_fn_name).exit_block = exit;
        functions.at(curr_fn_name).exit_value = exit_value;

        for (auto &expr : fn.body) {
            genExpr(move(expr));
        }

        // Run destructors

        BasicBlock *curr = builder->GetInsertBlock();

        builder->SetInsertPoint(exit);
        for (auto &v : functions.at(curr_fn_name).variables) {
            Value *val = v.second;

            if (val != exit_value) { // Do not delete returned value
                Type *tp = val->getType();

                while (tp->isPointerTy())
                    tp = tp->getContainedType(0);

                if (tp->isStructTy()) {
                    if (type != "") { // do not run destructor in yourself's destructor
                        if (fn_vars.at("self")->getType()->getContainedType(0) == tp)
                            break;
                    }

                    string st_name = tp->getStructName();

                    try {
                        Type *selftp = struct_types.at(st_name).type;
                        string mname = mangle(LLVMCall("destructor", {selftp}, 'F'), selftp);
                        auto callee = functions.at(mname);

                        builder->CreateCall(callee.fn, builder->CreateLoad(val));
                    } catch (out_of_range e) { /* No destructor */ }
                }
            }
        }
        if (fn.ret_type != _TType::Void) {
            builder->CreateRet(builder->CreateLoad(exit_value));
        } else {
            builder->CreateRetVoid();
        }

        builder->SetInsertPoint(curr);
}

void CodeGen::genCompiledIn() { // Always inline compiled in & optimize out unused
    auto defOp = [&](tuple<string, TType, TType, TType, function<Value *(Value *, Value *)>> fncdef) {
        string op = get<0>(fncdef);
        Type *t1 = getLLVMType(get<1>(fncdef));
        Type *t2 = getLLVMType(get<2>(fncdef));
        Type *ret = getLLVMType(get<3>(fncdef));

        string name = mangle(LLVMCall(op, deque<Type *>{t1,t2}, 'O'));

        Function *fnc = Function::Create(FunctionType::get(ret, vector<Type *>{t1,t2}, false), Function::LinkOnceAnyLinkage, name, module.get());
        fnc->addFnAttr(Attribute::AlwaysInline);

        BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
        builder->SetInsertPoint(enter);

        auto &arg_list = fnc->getArgumentList();

        builder->CreateRet(get<4>(fncdef)(&*arg_list.begin(), &*--arg_list.end()));
        functions.emplace(name, LLVMFn(fnc, map<string, Value *>(), fnc->getReturnType()));
    };

    #define genTuple(c, t1, t2, ret, lmd) {c, _TType::t1, _TType::t2, _TType::ret, [&](Value *v1, Value *v2){ return builder->lmd(v1,v2);}}

    vector<tuple<string, TType, TType, TType, function<Value *(Value*, Value*)>>> ops{
        // Basic integer operators
        genTuple("+", Int, Int, Int, CreateAdd),
        genTuple("-", Int, Int, Int, CreateSub),
        genTuple("*", Int, Int, Int, CreateMul),
        genTuple("/", Int, Int, Int, CreateSDiv),
        // Integer compare
        genTuple("==", Int, Int, Bool, CreateICmpEQ),
        genTuple("!=", Int, Int, Bool, CreateICmpNE),
        genTuple(">", Int, Int, Bool, CreateICmpSGT),
        genTuple("<", Int, Int, Bool, CreateICmpSLT),
        // Basic float operators
        genTuple("+", Float, Float, Float, CreateFAdd),
        genTuple("-", Float, Float, Float, CreateFSub),
        genTuple("*", Float, Float, Float, CreateFMul),
        genTuple("/", Float, Float, Float, CreateFDiv),
        // Float compare
        genTuple("==", Float, Float, Bool, CreateFCmpOEQ),
        genTuple("!=", Float, Float, Bool, CreateFCmpONE),
        genTuple(">", Float, Float, Bool, CreateFCmpOGT),
        genTuple("<", Float, Float, Bool, CreateFCmpOLT),
    };

    for (auto &o : ops) {
        defOp(o);
    }

    /*FunctionType *ftoi_t = FunctionType::get(getLLVMType(_TType::Int), getLLVMType(_TType::Float), false);
    Function *ftoi = Function::Create(ftoi_t, Function::LinkOnceAnyLinkage, "floatToInt", module.get());
    BasicBlock *enter = BasicBlock::Create(context, "entry", ftoi);
    builder->SetInsertPoint(enter);
    builder->CreateRet(builder->CreateFPToSI(&*ftoi->args().begin(), getLLVMType(_TType::Int)));
    functions.emplace("floatToInt", LLVMFn(ftoi, map<string, Value *>(), ftoi_t->getReturnType()));*/

    compiledInEmited = true;
}

void CodeGen::AST2IR() {
    for (auto &in : this->incls) {
        for (string mod : in->modules) {
            CodeGen m(mod + ".ff");
            for (auto &o_fn : m.module->getFunctionList()) {
                Function *ef = cast<Function>(module->getOrInsertFunction(o_fn.getName(), o_fn.getFunctionType()));
                functions.emplace(o_fn.getName(), LLVMFn(ef, map<string, Value*>(), o_fn.getReturnType()));
            }
            includes.push_back(move(m.module));
        }
    }

    // Only emit compied-in functions once
    if (!compiledInEmited) genCompiledIn();

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
        genFnc(*op);
    }

    for (auto &st : this->typedefs) {
        auto strct = st.second;

        vector<pair<string, Type *>> els;
        transform(strct->fields.begin(), strct->fields.end(), back_inserter(els),
                  [&](pair<string, TType> t) -> pair<string, Type *> { return {t.first, getLLVMType(t.second)}; });
        vector<Type *> decl_els;
        transform(els.begin(), els.end(), back_inserter(decl_els),
                  [&](pair<string, Type *> t) { return t.second; });

        StructType *type = StructType::create(decl_els, st.first);

        struct_types.emplace(st.first, LLVMStruct(els, type));
    }

    for (auto &i : this->impls) {
        for (auto &fn : i->fncs) {
            genFnc(*fn, i->type);
        }
    }

    for (auto &fn : this->ast) {
        genFnc(*fn);
    }
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        cout << "No filename" << endl;
        return 1;
    }

    CodeGen codegen(argv[1]);

    Linker l(*codegen.module);
    for (auto &in : codegen.includes) {
        l.linkInModule(move(in));
    }

    PassManager<Module> pm;
    AnalysisManager<Module> am;
    pm.addPass(AlwaysInlinerPass());

    pm.run(*codegen.module, am);

    string fname = string(argv[1]);
    fname = fname.substr(0, fname.length() - 3);

    ofstream file(fname + ".ll");
    raw_os_ostream outfile(file);

    codegen.module->print(outfile, nullptr);

    return 0;
}
