#include "gtest/gtest.h"
#include "ASTParser.hpp"
#include "ParserShared.hpp"

TEST(OperatorDef, IntPlusStr) {
    ASTParser par("operator +(int x, str y) int { }");

    vector<unique_ptr<OperatorDefAST>> ops = move(par.operators);

    ASSERT_EQ(ops.size(), 1);

    unique_ptr<OperatorDefAST> op = move(ops[0]);

    ASSERT_EQ(op->ret_type, _TType::Int);
    ASSERT_EQ(op->lhs.second, _TType::Int);
    ASSERT_EQ(op->rhs.second, _TType::Str);

    ASSERT_EQ(op->body.size(), 0);
}

TEST(OperatorDef, RefIntPlusRefStr) {
    ASTParser par("operator +(ref int x, ref str y) ref int { ret x; }");

    vector<unique_ptr<OperatorDefAST>> ops = move(par.operators);

    ASSERT_EQ(ops.size(), 1);

    unique_ptr<OperatorDefAST> op = move(ops[0]);

    ASSERT_TRUE(op->ret_type.isRef());
    ASSERT_EQ(*op->ret_type.referenceTo, _TType::Int);

    ASSERT_EQ(*op->lhs.second.referenceTo, _TType::Int);
    ASSERT_EQ(*op->rhs.second.referenceTo, _TType::Str);

    ASSERT_EQ(op->body.size(), 1);
}
