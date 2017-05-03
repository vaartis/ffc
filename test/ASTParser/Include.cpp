#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(Include, OneModule) {
    ASTParser par("include \"test\";");

    vector<unique_ptr<IncludeAST>> incls = par.includes;
    unique_ptr<IncludeAST> incl = incls[0];

    ASSERT_EQ(incl->modules.front(), "test");
}

TEST(Include, MultipleModules) {
    ASTParser par("include \"test\" \"test_second\";");

    vector<unique_ptr<IncludeAST>> incls = par.includes;
    unique_ptr<IncludeAST> incl = incls[0];

    ASSERT_EQ(incl->modules, (vector<string>{"test", "test_second"}));
}

TEST(Include, MultipleIncludes) {
    ASTParser par("include \"test\" \"test_second\"; include \"test_third\" \"test_fourth\";");

    vector<unique_ptr<IncludeAST>> incls = par.includes;
    unique_ptr<IncludeAST> incl_one = incls[0];
    unique_ptr<IncludeAST> incl_two = incls[1];

    ASSERT_EQ(incl_one->modules, (vector<string>{"test", "test_second"}));
    ASSERT_EQ(incl_two->modules, (vector<string>{"test_third", "test_fourth"}));
}
