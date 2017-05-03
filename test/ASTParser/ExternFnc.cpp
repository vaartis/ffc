#include "ASTParser.hpp"
#include "ParserShared.hpp"
#include "gtest/gtest.h"

TEST(ExternFnc, NoParams) {
    ASTParser par("extern test();");

    vector<unique_ptr<ExternFncAST>> incls = par.ext_functions;

    ASSERT_EQ(incls.size(), 1);

    unique_ptr<ExternFncAST> ext = incls[0];

    ASSERT_EQ(ext->name, "test");
    ASSERT_EQ(ext->args.size(), 0);
    ASSERT_EQ(ext->ret_type, _TType::Void);
}

TEST(ExternFnc, WithParamsAndRet) {
    ASTParser par("extern test(int, float) bool;");

    vector<unique_ptr<ExternFncAST>> incls = par.ext_functions;

    ASSERT_EQ(incls.size(), 1);

    unique_ptr<ExternFncAST> ext = incls[0];

    ASSERT_EQ(ext->name, "test");

    ASSERT_EQ(ext->args.size(), 2);

    ASSERT_EQ(ext->args[0], _TType::Int);
    ASSERT_EQ(ext->args[1], _TType::Float);

    ASSERT_EQ(ext->ret_type, _TType::Bool);
}

TEST(ExternFnc, ForgotSemicolon) {
    ASSERT_DEATH(ASTParser par("extern test()"), "Assertion .* failed");
}
