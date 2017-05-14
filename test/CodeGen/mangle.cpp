#include <optional>

#include "gtest/gtest.h"
#include "CodeGen.hpp"

class MangleTests : public ::testing::Test, protected CodeGen {
    protected:
        MangleTests() : CodeGen("", true) {}
};

TEST_F(MangleTests, Function) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{});

    string r = mangle(&f, nullopt);
    ASSERT_EQ(r, "_FFFN4testA3i32");
}

TEST_F(MangleTests, TypeFunction) {
    FncDefAST f = FncDefAST("test",
                            deque<pair<string, TType>>{{"x", _TType::Int}},
                            _TType::Int,
                            vector<shared_ptr<BaseAST>>{},
                            map<string, TypedName>{{"self", TypedName("self","test_ty")}});

    string r = mangle(&f, "test_ty");
    ASSERT_EQ(r, "_FFFT7test_tyN4testA3i32");
}

TEST_F(MangleTests, Operator) {
    OperatorDefAST f = OperatorDefAST("i32+float",
                                 "+",
                                 deque<pair<string, TType>>{{"x", _TType::Int}, {"y", _TType::Float}},
                                 _TType::Int,
                                 vector<shared_ptr<BaseAST>>{},
                                 map<string, TypedName>{});

    string r = mangle(&f, nullopt);
    ASSERT_EQ(r, "_FFON9i32+floatA3i325float");
}
