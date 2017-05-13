#include "ASTParser.hpp"
#include "gtest/gtest.h"

TEST(FncCall, NonExistant) {
    ASSERT_THROW(ASTParser par("fnc main() { nonexistant(); }"), std::runtime_error);
}

TEST(FncCall, Simple) {
    ASTParser par("fnc test() {} fnc main() { test(); }");

    vector<FncDefAST> fns = par.functions;
    ASSERT_EQ(fns.size(), 2);

    FncDefAST mainf = fns[1];

    ASSERT_EQ(mainf.body.size(), 1);

    shared_ptr<BaseAST> ca = mainf.body[0];

    FncCallAST *call = dynamic_cast<FncCallAST *>(ca.get());

    ASSERT_NE(call, nullptr);

    ASSERT_EQ(call->name, "test");
}
