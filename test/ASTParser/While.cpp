#include "gtest/gtest.h"
#include "ASTParser.hpp"
#include "ParserShared.hpp"

TEST(While, Empty) {
    ASTParser par("fnc main() { while true { } }");

    vector<FncDefAST> fns = par.functions;
    FncDefAST mainf = fns[0];

    shared_ptr<BaseAST> wl = mainf.body[0];

    WhileAST *w = dynamic_cast<WhileAST *>(wl.get());

    ASSERT_NE(w, nullptr);

    BoolAST *b = dynamic_cast<BoolAST *>(w->cond.get());

    ASSERT_NE(w, nullptr);

    ASSERT_EQ(b->value, true);

    ASSERT_EQ(w->body.size(), 0);
}

TEST(While, BasicBody) {
    ASTParser par("fnc main() { while true { int x = 1; } }");

    vector<FncDefAST> fns = par.functions;
    FncDefAST mainf = fns[0];

    shared_ptr<BaseAST> wl = mainf.body[0];

    WhileAST *w = dynamic_cast<WhileAST *>(wl.get());

    ASSERT_NE(w, nullptr);

    BoolAST *b = dynamic_cast<BoolAST *>(w->cond.get());

    ASSERT_NE(w, nullptr);

    ASSERT_EQ(b->value, true);

    ASSERT_EQ(w->body.size(), 1);

    DeclAST *d = dynamic_cast<DeclAST *>(w->body[0].get());

    ASSERT_NE(d, nullptr);
}
