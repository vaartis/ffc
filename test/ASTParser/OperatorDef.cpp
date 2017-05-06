#include "gtest/gtest.h"
#include "ASTParser.hpp"
#include "ParserShared.hpp"

TEST(OperatorDef, IntPlusStr) {
    ASTParser par("operator +(int x, str y) int { ret x; }");

    vector<OperatorDefAST> ops = par.operators;

    ASSERT_EQ(ops.size(), 1);

    OperatorDefAST op = ops[0];

    ASSERT_EQ(op.ret_type, _TType::Int);
    ASSERT_EQ(op.args[0].second, _TType::Int);
    ASSERT_EQ(op.args[1].second, _TType::Str);

    ASSERT_EQ(op.body.size(), 1);
}

TEST(OperatorDef, RefIntPlusRefStr) {
    ASTParser par("operator +(ref int x, ref str y) ref int { ret x; }");

    vector<OperatorDefAST> ops = par.operators;

    ASSERT_EQ(ops.size(), 1);

    OperatorDefAST op = ops[0];

    ASSERT_TRUE(op.ret_type.isRef());
    ASSERT_EQ(*op.ret_type.referenceTo, _TType::Int);

    ASSERT_EQ(*op.args[0].second.referenceTo, _TType::Int);
    ASSERT_EQ(*op.args[1].second.referenceTo, _TType::Str);

    ASSERT_EQ(op.body.size(), 1);
}
