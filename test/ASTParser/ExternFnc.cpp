#include "ASTParser.hpp"
#include "ParserShared.hpp"
#include "gtest/gtest.h"

TEST(ExternFnc, NoParams) {
    ASTParser par("extern test();");

    vector<unique_ptr<ExternFncAST>> incls = par.get_ext_functions();

    ASSERT_EQ(incls.size(), 1);

    unique_ptr<ExternFncAST> ext = move(incls[0]);

    ASSERT_EQ(ext->name, "test");
    ASSERT_EQ(ext->args.size(), 0);
    ASSERT_EQ(ext->ret_type, _TType::Void);
}

TEST(ExternFnc, WithParamsAndRet) {
    ASTParser par("extern test(int, float) bool;");

    vector<unique_ptr<ExternFncAST>> incls = par.get_ext_functions();

    ASSERT_EQ(incls.size(), 1);

    unique_ptr<ExternFncAST> ext = move(incls[0]);

    ASSERT_EQ(ext->name, "test");

    ASSERT_EQ(ext->args.size(), 2);

    ASSERT_EQ(ext->args[0], _TType::Int);
    ASSERT_EQ(ext->args[1], _TType::Float);

    ASSERT_EQ(ext->ret_type, _TType::Bool);
}

TEST(ExternFnc, ForgotSemicolon) {
    ASTParser par("extern test()");

    vector<unique_ptr<ExternFncAST>> incls = par.get_ext_functions();

    ASSERT_EQ(incls.size(), 0);
}
