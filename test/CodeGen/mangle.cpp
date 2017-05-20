#include "optional.hpp"

#include "gtest/gtest.h"
#include "CodeGen.hpp"

class MangleTests : public ::testing::Test, protected CodeGen {
    protected:
        MangleTests() : CodeGen("type test_ty { int x }", true) {}
};

TEST_F(MangleTests, Function) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{});

    string r = mangle(f, nullopt);
    ASSERT_EQ(r, "_FFFN4testA3i32R3i32");
}

TEST_F(MangleTests, TypeFunction) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{{"self", TypedName("self","test_ty")}});

    string r = mangle(f, string("test_ty"));
    ASSERT_EQ(r, "_FFFT7test_tyN4testA3i32R3i32");
}

TEST_F(MangleTests, Operator) {
    OperatorDefAST f = OperatorDefAST("i32+float",
                                 "+",
                                 deque<pair<string, TType>>{{"x", _TType::Int}, {"y", _TType::Float}},
                                 _TType::Int,
                                 vector<shared_ptr<BaseAST>>{},
                                 map<string, TypedName>{});

    string r = mangle(f, nullopt);
    ASSERT_EQ(r, "_FFON1+A3i32A5floatR3i32");
}

TEST_F(MangleTests, LLVMFnAndGenFnc) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{});

    genFnc(f, string("test_ty"), false);

    string r = mangle(f, string("test_ty"));
    ASSERT_NO_THROW(mangle(functions.at("_FFFT7test_tyN4testA3i32R3i32"), struct_types.at("test_ty").type));
}

TEST_F(MangleTests, FncCall) {
    shared_ptr<IntAST> in = make_shared<IntAST>(10);
    in->expression_type = _TType::Int;

    FncCallAST c("test_two", deque<shared_ptr<BaseAST>>{in}, 'F', nullopt);
    c.expression_type = _TType::Int;
    string asumed_name = mangle(c, nullopt);

    ASSERT_EQ(asumed_name, "_FFFN8test_twoA3i32R3i32");
}
