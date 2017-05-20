#include "gtest/gtest.h"
#include "CodeGen.hpp"
#include "optional.hpp"

class GenFncTests : public ::testing::Test, protected CodeGen {
    protected:
        GenFncTests() : CodeGen("", true) {}
};

TEST_F(GenFncTests, Simple) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{{"var", TypedName("var", _TType::Int)}});
    genFnc(f, nullopt, false);

    string n = mangle(f, nullopt);

    LLVMFn l_testf;
    ASSERT_NO_THROW(l_testf = functions.at(n));

    ASSERT_EQ(l_testf.variables.size(), 1);
    ASSERT_EQ(l_testf.fn->getName(), n);
}

TEST_F(GenFncTests, Operator) {
    OperatorDefAST f = OperatorDefAST("i32+float",
                                 "+",
                                 deque<pair<string, TType>>{{"x", _TType::Int}},
                                 _TType::Int,
                                 vector<shared_ptr<BaseAST>>{},
                                 map<string, TypedName>{{"var", TypedName("var", _TType::Int)}});
    genFnc(f, nullopt, false);

    string n = mangle(f, nullopt);

    LLVMFn l_testf;
    ASSERT_NO_THROW(l_testf = functions.at(n));

    ASSERT_EQ(l_testf.variables.size(), 1);
    ASSERT_EQ(l_testf.fn->getName(), n);
}


TEST_F(GenFncTests, TypeFnc) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{{"var", TypedName("var", _TType::Int)}});
    genFnc(f, string("test_ty"), false);

    string n = mangle(f, string("test_ty"));

    LLVMFn l_testf;
    ASSERT_NO_THROW(l_testf = functions.at(n));

    ASSERT_EQ(l_testf.variables.size(), 1);
    ASSERT_EQ(l_testf.fn->getName(), n);
}
