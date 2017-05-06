#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(Assign, Simple){
    ASTParser par("fnc main() { x = 1; }");

    vector<FncDefAST> fns = par.functions;
    FncDefAST mainf = fns[0];

    ASSERT_EQ(mainf.body.size(), 1);

    shared_ptr<BaseAST> as = mainf.body[0];

    AssAST *assign = dynamic_cast<AssAST *>(as.get());

    ASSERT_NE(assign, nullptr);

    ASSERT_EQ(assign->name, "x");

    IntAST *i = dynamic_cast<IntAST *>(assign->value.get());

    ASSERT_NE(i, nullptr);
    ASSERT_EQ(i->value, 1);
}

TEST(Assign, Variable) {
    ASTParser par("fnc main() { int x = 10; int y; y = x; }");

    vector<FncDefAST> fns = par.functions;
    FncDefAST mainf = fns[0];

    ASSERT_EQ(mainf.body.size(), 3);

    shared_ptr<BaseAST> as = mainf.body[2];

    AssAST *assign = dynamic_cast<AssAST *>(as.get());

    ASSERT_NE(assign, nullptr);

    ASSERT_EQ(assign->name, "y");

    IdentAST *i = dynamic_cast<IdentAST *>(assign->value.get());

    ASSERT_NE(i, nullptr);
    ASSERT_EQ(i->value, "x");
}
