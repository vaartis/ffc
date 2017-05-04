#include "gtest/gtest.h"
#include "ASTParser.hpp"
#include "ParserShared.hpp"

TEST(TypeDef, Empty) {
    ASTParser par("type T { }");

    map<string, TypeDefAST> types = par.typedefs;

    ASSERT_EQ(types.size(), 1);

    TypeDefAST type = types.at("T");

    ASSERT_EQ(type.name, "T");
    ASSERT_EQ(type.fields.size(), 0);
}

TEST(TypeDef, WithFields) {
    ASTParser par("type T { int x, str y }");

    map<string, TypeDefAST> types = par.typedefs;

    ASSERT_EQ(types.size(), 1);

    TypeDefAST type = types.at("T");

    ASSERT_EQ(type.name, "T");
    ASSERT_EQ(type.fields.size(), 2);

    ASSERT_EQ(_TType::Int, type.fields[0].second);
    ASSERT_EQ("y", type.fields[1].first);
}

TEST(TypeDef, WithRef) {
    ASTParser par("type T { ref int x, ref str y }");

    map<string, TypeDefAST> types = par.typedefs;

    TypeDefAST type = types.at("T");

    ASSERT_EQ(type.name, "T");
    ASSERT_EQ(type.fields.size(), 2);

    ASSERT_TRUE(type.fields[0].second.isRef());
    ASSERT_EQ(type.fields[0].first, "x");

    ASSERT_TRUE(type.fields[1].second.isRef());
    ASSERT_EQ(type.fields[1].first, "y");
}
