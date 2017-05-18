#include "CodeGen.hpp"

using namespace std;
using namespace llvm;
using mpark::variant;

bool compiledInEmited = false;
LLVMContext context;

/** Name mangling implementation
 *
 * To provide generics ans operator overloading (and also name clashes), there is
 * function name mangling. The name begins with `_FF`, then there is one character,
 * `F` or `O`, for Function and Operator respectivly. Optionally, there might be a type
 * function belongs to, `T(length of type name)(type name)`, for example `T7test_ty`.
 * Next symbol is `N` followed by an integer, it says how long the name is,
 * for example `N4test`. Then, for every argument there goes `A(length or argument type)(argument type)`,
 * for example `A3i32A4float` for `f(int, float)`. The last one is `R(length of return type)(return type)`,
 * if function does not return anything the return type is `void`. The complete example would look like this:
 * `_FFFT7test_tyN4testAi32AfloatRint` for `implement for test_ty { fnc test(int x, float y) int }`
 *
 * | Symbol | Meaning                        |
 * | :----: | :----------------------------: |
 * | _FF    | Beggining of function name     |
 * | T      | Type to which function belongs |
 * | N      | Name of the function           |
 * | A      | Type of function argument      |
 * | R      | Function's return type         |
 */
string CodeGen::mangle(FncDefAST *f, optional<string> tp = nullopt) {
   string res_name = "_FF";

    if (dynamic_cast<OperatorDefAST *>(f)) {
        res_name += "O";
    } else {
        res_name += "F";
    }

    if (tp) {
        res_name += "T" + to_string(tp.value().length()) + tp.value();
    }

    res_name += "N" + to_string(f->name.length()) + f->name;

    for (auto &arg : f->args) {
            res_name += "A";
            string arg_name;
            if (getLLVMType(arg.second)->isStructTy()) {
                arg_name = getLLVMType(arg.second)->getStructName();
            } else {
                string useless;
                raw_string_ostream name_s(useless);
                Type *tt = getLLVMType(arg.second);

                tt->print(name_s);

                arg_name = name_s.str();
            }

            res_name += to_string(arg_name.length()) + arg_name;
    }

    Type *rtype = getLLVMType(f->ret_type);
    string useless;
    raw_string_ostream name_s(useless);
    rtype->print(name_s);
    string rs = name_s.str();

    res_name += "R" + to_string(rs.length()) + rs;

    return res_name;
}

string CodeGen::mangle(LLVMFn f, optional<Type *> t = nullopt) {
    string res_name("_FF" + string(1, f.type));

    if (t) {
        auto tp = t.value();
        while (tp->isPointerTy())
            tp = tp->getContainedType(0);
        string st_name = tp->getStructName();

        res_name += "T" + to_string(st_name.length()) + st_name;
    }

    res_name += "N" + to_string(f.fn->getName().str().length()) + f.fn->getName().str();

    for (auto &arg : f.fn->getArgumentList()) {
        res_name += "A";
        string arg_name;
        if (arg.getType()->isStructTy()) {
            arg_name = arg.getType()->getStructName();
        } else {
            string useless;
            raw_string_ostream name_s(useless);

            arg.getType()->print(name_s);

            arg_name = name_s.str();
        }
        res_name += to_string(arg_name.length()) + arg_name;
    }

    string useless;
    raw_string_ostream name_s(useless);
    f.fn->getReturnType()->print(name_s);
    res_name += "R" + to_string(name_s.str().length()) + name_s.str();

    return res_name;
}

string CodeGen::mangle(FncCallAST *f, optional<string> tp = nullopt) {
    string res_name = "_FF" + string(1, f->f_or_op);

    if (tp) {
        res_name += "T" + to_string(tp.value().length()) + tp.value();
    }

    res_name += "N" + to_string(f->name.length()) + f->name;

    for (auto &arg : f->args) {
        res_name += "A";
        string arg_name;

        auto at = dynamic_cast<Expression *>(arg.get())->expression_type;

        if (getLLVMType(at)->isStructTy()) {
            arg_name = getLLVMType(at)->getStructName();
        } else {
            string useless;
            raw_string_ostream name_s(useless);
            Type *tt = getLLVMType(at);

            tt->print(name_s);

            arg_name = name_s.str();
        }

        res_name += to_string(arg_name.length()) + arg_name;
    }


    Type *rtype = getLLVMType(dynamic_cast<Expression *>(f)->expression_type);
    string useless;
    raw_string_ostream name_s(useless);
    rtype->print(name_s);
    string rs = name_s.str();

    res_name += "R" + to_string(rs.length()) + rs;

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
              if constexpr (is_same<_TType, T>::value) {
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
              } else if constexpr (is_same<string, T>::value) {
                  try {
                      return struct_types.at(arg).type;
                  } catch (out_of_range) {
                      throw runtime_error("Unknown type: " + arg);
                  }
              } else if constexpr (is_same<GenericType, T>::value) {
                  return getLLVMType(curr_generics_types.at(t.to_string()));
              } else {
                  throw runtime_error("Bug");
              }
          }, t.inner);
}

Value *CodeGen::genFncCall(FncCallAST *ca) {
    LLVMFn callee;
    vector<Value *> argms;
    optional<string> tp;

    if (ca->type) {
        tp = functions.at(curr_fn_name).variables.at(ca->type.value())->getType()->getStructName().str();
    }

    deque<Type *> mangle_args;
    for (auto arg : ca->args) {
        auto ar = genExpr(arg);

        mangle_args.push_back(ar->getType());

        argms.push_back(ar);
    }

    string name = ca->name;
    if (find_if(exts.begin(), exts.end(), [&](ExternFncAST e){ return e.name == ca->name; }) == exts.end()) {
        name = mangle(ca);
    }

    try {
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
}

Value *CodeGen::genIf(IfAST *ifb) {
    BasicBlock *then = BasicBlock::Create(context, "if.then");
    BasicBlock *els = BasicBlock::Create(context, "if.else");
    BasicBlock *ifend = BasicBlock::Create(context, "if.end");

    Value *cond = genExpr(ifb->cond);

    Value *c = builder->CreateCondBr(cond, then, els);

    Value *if_val = nullptr, *else_val = nullptr;

    // then

    functions.at(curr_fn_name).fn->getBasicBlockList().push_back(then);
    builder->SetInsertPoint(then);
    for (auto v : ifb->body)
        genStmt(v);

    if (ifb->value != nullptr) {
        if_val = genExpr(ifb->value);
    }

    then = builder->GetInsertBlock(); // update for phi

    builder->CreateBr(ifend);

    // else

    functions.at(curr_fn_name).fn->getBasicBlockList().push_back(els);

    builder->SetInsertPoint(els);
    for (auto v : ifb->else_body)
        genStmt(v);

    if (ifb->else_value != nullptr) {
        else_val = genExpr(ifb->else_value);
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

Value *CodeGen::genType(TypeAST *st) {
    try {
        auto e = static_cast<Expression>(*st);

        LLVMStruct s;
        if (e.expression_type.isGeneric()) {
            s = struct_types.at(curr_generics_types.at(e.expression_type.to_string()));
        } else {
            s = struct_types.at(e.expression_type.to_string());
        }

        Value *tmp_struct = builder->CreateAlloca(s.type); // should be opimized out if unused

        for (auto field : st->fields) {
            string f_name = field.first;
            Value *f_val = genExpr(field.second);

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
}

Value *CodeGen::genExpr(shared_ptr<BaseAST> obj, bool noload) {
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

            Value *value = genExpr(in->value);

            Value *pointee = builder->CreateLoad(var);
            builder->CreateStore(value, pointee);

            return value;
        } else {
            return builder->CreateLoad(genExpr(v->value));
        }
    } else if (auto r = dynamic_cast<RefToValAST *>(obj.get())) {
        return genExpr(r->value, true);
    } else if (auto ca = dynamic_cast<FncCallAST *>(obj.get())) {
        return genFncCall(ca);
    } else if (auto st = dynamic_cast<TypeAST *>(obj.get())) {
        genType(st);
    } else if (auto st = dynamic_cast<TypeFieldLoadAST  *>(obj.get())) {
        auto val = genExpr(st->from);
        auto tmpv = builder->CreateAlloca(val->getType());
        builder->CreateStore(val, tmpv);

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

        Value *this_field = builder->CreateStructGEP(s.type, tmpv, index);
        return builder->CreateLoad(this_field);
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
    } else if (auto ifb = dynamic_cast<IfAST *>(obj.get())) {
        genIf(ifb);
    }

    throw runtime_error("strange expr");
}

void CodeGen::genStmt(shared_ptr<BaseAST> obj, bool noload) {
    if (auto decl = dynamic_cast<DeclAST *>(obj.get())) {
        auto t = getLLVMType(decl->type);

        Value *alloc = builder->CreateAlloca(t, nullptr, decl->name);

        functions.at(curr_fn_name).variables.emplace(decl->name, alloc);

        if (decl->value != nullptr) {
            Value *v = genExpr(decl->value);

            if (t != v->getType())
                throw runtime_error(string("Invalid type assigned to ") + decl->name);

            builder->CreateStore(v, alloc, false);
        }
    } else if (auto ass = dynamic_cast<AssAST *>(obj.get())) {
        try {
            auto var = functions.at(curr_fn_name).variables.at(ass->name);

            Value *v = genExpr(ass->value);

            if (v->getType() != var->getType()->getContainedType(0)) // var is always <type>* (alloca type), so we check the type it points to
                throw runtime_error(string("Invalid type assigned to ") + ass->name);

            builder->CreateStore(v, var, false);
        } catch (out_of_range) {
            throw runtime_error(string("Undefined variable: ") + ass->name);
        }
    } else if (auto ret = dynamic_cast<RetAST *>(obj.get())) {
        if (ret->value != nullptr) {
            Value *retxpr = genExpr(ret->value);

            if (retxpr->getType() != functions.at(curr_fn_name).ret_type)
                throw runtime_error("Invalid return type");

            functions.at(curr_fn_name).exit_value = retxpr;
        } else {
            if (functions.at(curr_fn_name).ret_type != builder->getVoidTy())
                throw runtime_error("Can't return nothing from a non-void function");
        }

        builder->CreateBr(functions.at(curr_fn_name).exit_block);
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

        builder->CreateStore(genExpr(st->value), this_field);
    } else if (auto wh = dynamic_cast<WhileAST *>(obj.get())) {
        BasicBlock *l_cond = BasicBlock::Create(context, "loop.cond");
        BasicBlock *l_while  = BasicBlock::Create(context, "loop.while");
        BasicBlock *l_end = BasicBlock::Create(context, "loop.end");

        builder->CreateBr(l_cond);

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(l_cond);

        builder->SetInsertPoint(l_cond);
        Value *cond = genExpr(wh->cond);
        builder->CreateCondBr(cond, l_while, l_end);

        builder->SetInsertPoint(l_while);
        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(l_while);
        for (auto el : wh->body) {
            genStmt(el);
        }
        builder->CreateBr(l_cond);

        functions.at(curr_fn_name).fn->getBasicBlockList().push_back(l_end);
        builder->SetInsertPoint(l_end);
    } else {
        genStmt(obj, noload);
    }
}

/** Generate a function from an AST node.
 *
 * Inserts exiting and entering points, exiting value, mangles function's name & adds
 * it to global function list.
 */
void CodeGen::genFnc(FncDefAST fn, optional<string> type = nullopt, bool skipcheck = false) {
    if (fn.name != "main") {
        fn.name = mangle(&fn, type);
    }

    if (!skipcheck) {
        if (find(real_names.begin(), real_names.end(), fn.name) == real_names.end()) {
            real_names.push_back(fn.name);
        } else {
            throw runtime_error("Redefinition of a function: " + fn.name);
        }
    }

    curr_fn_name = fn.name;

    vector<Type *> fn_args;
    map<string, Value *> fn_vars;

    for (auto ar : fn.args)
        fn_args.push_back(getLLVMType(ar.second));

    FunctionType *tp = FunctionType::get(getLLVMType(fn.ret_type), fn_args, false);
    Function *fnc = Function::Create(tp, Function::ExternalLinkage, fn.name, module.get());

    BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
    BasicBlock *exit = BasicBlock::Create(context, "exit", fnc);

    builder->SetInsertPoint(enter);

    Value *exit_value = nullptr;
    if (getLLVMType(fn.ret_type) != getLLVMType(_TType::Void)) {
        exit_value = builder->CreateAlloca(getLLVMType(fn.ret_type), nullptr, "ret_value");
    };

    auto arg = fnc->arg_begin();
    auto fnc_arg = fn.args.begin();
    for (; arg != fnc->arg_end(); arg++, fnc_arg++) {
        Value *alloc = builder->CreateAlloca(getLLVMType(fnc_arg->second), nullptr, fnc_arg->first);
        builder->CreateStore(&*arg, alloc);
        fn_vars.emplace(fnc_arg->first, alloc);
    }

    char f_or_op = dynamic_cast<OperatorDefAST *>(&fn) ? 'O' : 'F';

    functions.emplace(fn.name, LLVMFn(fnc, fn_vars, getLLVMType(fn.ret_type), f_or_op));
    functions.at(curr_fn_name).exit_block = exit;
    functions.at(curr_fn_name).exit_value = exit_value;

    for (auto expr : fn.body) {
        genStmt(expr);
    }

    // Run destructors

    BasicBlock *curr = builder->GetInsertBlock();

    builder->SetInsertPoint(exit);
    for (auto v : functions.at(curr_fn_name).variables) {
        Value *val = v.second;

        if (val != exit_value) { // Do not delete returned value
            Type *tp = val->getType();

            while (tp->isPointerTy())
                tp = tp->getContainedType(0);

            if (tp->isStructTy()) {
                if (type) { // do not run destructor in yourself's destructor
                    if (fn_vars.at("self")->getType()->getContainedType(0) == getLLVMType(type.value()))
                        break;
                }

                string st_name = tp->getStructName();

                try {
                    Type *selftp = struct_types.at(st_name).type;
                    FncDefAST fff("destructor", deque<pair<string, TType>>{{"self", st_name}},
                                  _TType::Void, vector<shared_ptr<BaseAST>>{}, map<string, TypedName>{});
                    string mname = mangle(&fff, st_name);
                    auto callee = functions.at(mname);

                    builder->CreateCall(callee.fn, builder->CreateLoad(val));
                } catch (out_of_range e) { /* No destructor */ }
            }
        }
    }

    if (getLLVMType(fn.ret_type) != getLLVMType(_TType::Void)) {
        builder->CreateRet(functions.at(curr_fn_name).exit_value);
    } else {
        builder->CreateRetVoid();
    }

    builder->SetInsertPoint(curr);
}

void CodeGen::genCompiledIn() { // Always inline compiled in & optimize out unused
    auto defOp = [&](tuple<string, TType, TType, TType, function<Value *(Value *, Value *)>> fncdef) {
        string op = get<0>(fncdef);
        TType tt1 = get<1>(fncdef);
        TType tt2 = get<2>(fncdef);
        TType rret = get<3>(fncdef);

        OperatorDefAST o(tt1.to_string() + op + tt2.to_string(),
                         op,
                         deque<pair<string, TType>>{{"x", tt1}, {"y", {tt2}}},
                         rret,
                         vector<shared_ptr<BaseAST>>{},
                         map<string, TypedName>{});
        string name = mangle(&o);

        Type *t1 = getLLVMType(get<1>(fncdef));
        Type *t2 = getLLVMType(get<2>(fncdef));
        Type *ret = getLLVMType(get<3>(fncdef));

        Function *fnc = Function::Create(FunctionType::get(ret, vector<Type *>{t1,t2}, false), Function::LinkOnceAnyLinkage, name, module.get());
        fnc->addFnAttr(Attribute::AlwaysInline);

        BasicBlock *enter = BasicBlock::Create(context, "entry", fnc);
        builder->SetInsertPoint(enter);

        auto &arg_list = fnc->getArgumentList();

        builder->CreateRet(get<4>(fncdef)(&*arg_list.begin(), &*--arg_list.end()));
        functions.emplace(name, LLVMFn(fnc, map<string, Value *>(), fnc->getReturnType(), 'O'));
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

    for (auto o : ops) {
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
        for (string mod : in.modules) {
            CodeGen m(mod + ".ff");
            for (auto &o_fn : m.module->getFunctionList()) {
                Function *ef = cast<Function>(module->getOrInsertFunction(o_fn.getName(), o_fn.getFunctionType()));
                functions.emplace(o_fn.getName(), LLVMFn(ef, map<string, Value*>(), o_fn.getReturnType(), 'F')); // FIXME
            }
            includes.push_back(move(m.module));
        }
    }

    // Only emit compied-in functions once
    if (!compiledInEmited) genCompiledIn();

    for (auto ex : this->exts) {
        vector<Type *> ext_args;
        for (auto &ar : ex.args) {
            ext_args.push_back(getLLVMType(ar));
        }

        FunctionType *extTy = FunctionType::get(getLLVMType(ex.ret_type), ext_args, false);

        Function *ext = cast<Function>(module->getOrInsertFunction(ex.name, extTy));
        functions.emplace(ex.name, LLVMFn(ext, map<string, Value *>(), getLLVMType(ex.ret_type), 'F'));
    }

    for (auto st : this->typedefs) {
        auto strct = st.second;

        vector<pair<string, Type *>> els;
        transform(strct.fields.begin(), strct.fields.end(), back_inserter(els),
                  [&](pair<string, TType> t) -> pair<string, Type *> { return {t.first, getLLVMType(t.second)}; });
        vector<Type *> decl_els;
        transform(els.begin(), els.end(), back_inserter(decl_els),
                  [&](pair<string, Type *> t) { return t.second; });

        StructType *type = StructType::create(decl_els, st.first);

        struct_types.emplace(st.first, LLVMStruct(els, type));
    }

    for (auto u : this->generic_uses) {
        auto fn = generic_fncs.at(u.first);
        auto use = u.second;

        deque<pair<string, TType>>::iterator ar;
        deque<shared_ptr<BaseAST>>::iterator ar_u;

        if (fn.function.args.size() != use.args.size())
            throw runtime_error("Invalid number of arguments for " + fn.function.name +
                                ", expected " + to_string(fn.function.args.size()) + ", but found " + to_string(use.args.size()));

        for (pair<decltype(ar), decltype(ar_u)> i{fn.function.args.begin(), use.args.begin()}; i.first != fn.function.args.end(); ++i.first,
                 ++get<1>(i)) {
            auto arg_g = i.first;
            auto arg = i.second;

            auto uu = dynamic_cast<Expression *>(arg->get());
            if (arg_g->second.isGeneric()) {
                if (find_if(curr_generics_types.begin(),
                            curr_generics_types.end(),
                            [arg_g](pair<string, TType> gen) { return gen.first == arg_g->second.to_string(); }) == curr_generics_types.end()) {
                    auto uu = dynamic_cast<Expression *>(arg->get());
                    curr_generics_types.emplace(arg_g->second.to_string(), uu->expression_type);
                    arg_g->second = uu->expression_type;
                } else {
                    auto targ = curr_generics_types.at(arg_g->second.to_string());

                    if (targ != uu->expression_type) {
                        throw runtime_error("Generic type " +
                                            arg_g->second.to_string() +
                                            " is not " + uu->expression_type.to_string() + " as previous, it's " + targ.to_string());
                    } else {
                        arg_g->second = uu->expression_type;
                    }
                }
            } else {
                assert(arg_g->second == uu->expression_type && "Wrong type in function call");
            }
        }

        if (fn.function.ret_type.isGeneric()) {
             if (find_if(curr_generics_types.begin(),
                         curr_generics_types.end(),
                         [&](pair<string, TType> gen) { return gen.first == fn.function.ret_type.to_string(); }) != curr_generics_types.end()) {
                 fn.function.ret_type = curr_generics_types.at(fn.function.ret_type.to_string());
             } else {
                 throw runtime_error("Unimplemented? Can't deduce return type of " + fn.function.name);
             }
        }

        genFnc(fn.function, nullopt, true);
        curr_generics_types.clear();
    }

    for (auto op : this->ops) {
        genFnc(op);
    }

    for (auto i : this->impls) {
        for (auto fn : i.fncs) {
            genFnc(fn);
        }
    }

    for (auto fn : this->ast) {
        genFnc(fn);
    }
}
