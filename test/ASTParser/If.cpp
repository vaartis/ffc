#include "gtest/gtest.h"
#include "ASTParser.hpp"

TEST(If, Empty) {
    ASTParser par("fnc main() { if true {} }");

    vector<unique_ptr<FncDefAST>> fns = par.get_functions();
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    unique_ptr<BaseAST> ii = move(mainf->body[0]);

    IfAST *i = dynamic_cast<IfAST *>(ii.get());

    ASSERT_NE(i, nullptr);

    BoolAST *b = dynamic_cast<BoolAST *>(i->cond.get());

    ASSERT_NE(b, nullptr);

    ASSERT_EQ(b->value, true);

    ASSERT_EQ(i->body.size(), 0);
}

TEST(If, ReturnValue) {
    ASTParser par("fnc main() { if true { 1; 2; 3 } else { 12 } }");

    vector<unique_ptr<FncDefAST>> fns = par.get_functions();
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    ASSERT_EQ(mainf->body.size(), 1);

    unique_ptr<BaseAST> ii = move(mainf->body[0]);

    IfAST *i = dynamic_cast<IfAST *>(ii.get());

    ASSERT_NE(i, nullptr);

    BoolAST *b = dynamic_cast<BoolAST *>(i->cond.get());

    ASSERT_NE(b, nullptr);

    ASSERT_EQ(b->value, true);

    IntAST *first_res = dynamic_cast<IntAST *>(i->value.get());
    IntAST *second_res = dynamic_cast<IntAST *>(i->else_value.get());

    ASSERT_NE(first_res, nullptr);
    ASSERT_NE(second_res, nullptr);
}

TEST(If, ValueNoElse) {
    ASSERT_THROW(ASTParser par("fnc main() { if true { 1; 2; 3 } }"), std::runtime_error);
}
