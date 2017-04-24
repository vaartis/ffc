#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(Ret, Empty) {
    ASTParser par("fnc main() { ret; }");

    vector<unique_ptr<FncDefAST>> fns = par.get_functions();
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    unique_ptr<BaseAST> r = move(mainf->body[0]);

    RetAST *re = dynamic_cast<RetAST *>(r.get());

    ASSERT_NE(re, nullptr);

    ASSERT_EQ(re->value, nullptr);
}

TEST(Ret, Value) {
    ASTParser par("fnc main() int { ret 0; }");

    vector<unique_ptr<FncDefAST>> fns = par.get_functions();
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    unique_ptr<BaseAST> r = move(mainf->body[0]);

    RetAST *re = dynamic_cast<RetAST *>(r.get());

    ASSERT_NE(re, nullptr);

    IntAST *ret_r = dynamic_cast<IntAST *>(re->value.get());

    ASSERT_NE(ret_r, nullptr);
    ASSERT_EQ(ret_r->value, 0);
}
