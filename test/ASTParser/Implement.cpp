#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(Implement, Empty) {
    ASTParser par("type T { int x } implement for T { }");

    vector<ImplementAST> impls = par.impls;

    ASSERT_EQ(impls.size(), 1);

    ImplementAST impl = impls[0];

    ASSERT_EQ(impl.type, "T");
    ASSERT_EQ(impl.fncs.size(), 0);
}

TEST(Implement, Simple) {
    ASTParser par("type T { int x } implement for T { fnc f() int { ret self.x; } }");

    vector<ImplementAST> impls = par.impls;

    ASSERT_EQ(impls.size(), 1);

    ImplementAST impl = impls[0];

    ASSERT_EQ(impl.type, "T");
    ASSERT_EQ(impl.fncs.size(), 1);

    FncDefAST f = impl.fncs[0];
    ASSERT_EQ(f.body.size(), 1);
}

TEST(Implement, NonexistantType) {
    ASSERT_DEATH(ASTParser par("implement for T { fnc f() int { ret self.x; }"), "ImplementAST.*Assertion.*failed");
}

TEST(Implement, Destructor) {
    ASTParser par("type T { int x } implement for T { destructor { ret; } }");

    vector<ImplementAST> impls = par.impls;

    ASSERT_EQ(impls.size(), 1);

    ImplementAST impl = impls[0];

    ASSERT_EQ(impl.type, "T");
    ASSERT_EQ(impl.fncs.size(), 1);

    FncDefAST f = impl.fncs[0];
    ASSERT_EQ(f.name, "destructor");
    ASSERT_EQ(f.body.size(), 1);
}

TEST(Implement, InvalidToken) {
    ASSERT_THROW(ASTParser par("type T { int x } implement for T { T }"), std::runtime_error);
}
