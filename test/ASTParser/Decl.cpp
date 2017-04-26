#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(Declaration, Empty){
    ASTParser par("fnc main() { str x; }");

    vector<unique_ptr<FncDefAST>> fns = move(par.functions);
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    ASSERT_EQ(mainf->body.size(), 1);

    unique_ptr<BaseAST> d = move(mainf->body[0]);

    DeclAST *decl = dynamic_cast<DeclAST *>(d.get());

    ASSERT_EQ(decl->name, "x");
    ASSERT_EQ(decl->value, nullptr);
}

TEST(Declaration, Simple) {
    ASTParser par("fnc main() { str x = 10; }");

    vector<unique_ptr<FncDefAST>> fns = move(par.functions);
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    ASSERT_EQ(mainf->body.size(), 1);

    unique_ptr<BaseAST> d = move(mainf->body[0]);

    DeclAST *decl = dynamic_cast<DeclAST *>(d.get());

    ASSERT_EQ(decl->name, "x");

    IntAST *i = dynamic_cast<IntAST *>(decl->value.get());

    ASSERT_NE(i, nullptr);
    ASSERT_EQ(i->value, 10);
}

TEST(Declaration, MultipleVariables) {
    ASTParser par("fnc main() { str x = y; str y = x; }");

    vector<unique_ptr<FncDefAST>> fns = move(par.functions);
    unique_ptr<FncDefAST> mainf = move(fns[0]);

    ASSERT_EQ(mainf->body.size(), 2);

    unique_ptr<BaseAST> first = move(mainf->body[0]);
    unique_ptr<BaseAST> second = move(mainf->body[1]);

    DeclAST *dfirst = dynamic_cast<DeclAST *>(first.get());
    DeclAST *dsecond = dynamic_cast<DeclAST *>(second.get());

    ASSERT_EQ(dfirst->name, "x");
    ASSERT_EQ(dsecond->name, "y");

    IdentAST *v1 = dynamic_cast<IdentAST *>(dfirst->value.get());
    IdentAST *v2 = dynamic_cast<IdentAST *>(dsecond->value.get());

    ASSERT_NE(v1, nullptr);
    ASSERT_NE(v2, nullptr);

    ASSERT_EQ(v1->value, "y");
    ASSERT_EQ(v2->value, "x");
}
