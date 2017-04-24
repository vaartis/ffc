#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(Assign, Simple){
    ASTParser par("fnc main() { x = 1; }");

    vector<unique_ptr<FncDefAST>> fns = par.get_functions();
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    ASSERT_EQ(mainf->body.size(), 1);

    unique_ptr<BaseAST> as = move(mainf->body[0]);

    AssAST *assign = dynamic_cast<AssAST *>(as.get());

    ASSERT_NE(assign, nullptr);

    ASSERT_EQ(assign->name, "x");

    IntAST *i = dynamic_cast<IntAST *>(assign->value.get());

    ASSERT_NE(i, nullptr);
    ASSERT_EQ(i->value, 1);
}

TEST(Assign, Variable) {
    ASTParser par("fnc main() { y = x; }");

    vector<unique_ptr<FncDefAST>> fns = par.get_functions();
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    ASSERT_EQ(mainf->body.size(), 1);

    unique_ptr<BaseAST> as = move(mainf->body[0]);

    AssAST *assign = dynamic_cast<AssAST *>(as.get());

    ASSERT_NE(assign, nullptr);

    ASSERT_EQ(assign->name, "y");

    IdentAST *i = dynamic_cast<IdentAST *>(assign->value.get());

    ASSERT_NE(i, nullptr);
    ASSERT_EQ(i->value, "x");
}
