#include "gtest/gtest.h"
#include "CodeGen.hpp"

using namespace llvm;

class GenFncCallTests : public ::testing::Test, protected CodeGen {
    protected:
        GenFncCallTests() : CodeGen("fnc test_one() { ret; } fnc tt(int x) int { ret x; }", true) {}
};

TEST_F(GenFncCallTests, NoParams) {
    FncCallAST c("test_one", deque<shared_ptr<BaseAST>>{}, 'F', nullopt);
    Value *res = genFncCall(c);

    CallInst *call = dynamic_cast<CallInst *>(res);
    ASSERT_NE(call, nullptr);

    string asumed_name = mangle(c, nullopt);

    ASSERT_EQ(call->getNumArgOperands(), 0);

    ASSERT_EQ(call->getCalledFunction()->getName(), asumed_name);
    ASSERT_EQ(call->getCalledFunction()->getReturnType(), getLLVMType(_TType::Void));

    // Functions end with a non-conditional branch to exit, so we check if this branch points to block with return inst
    ASSERT_TRUE(dynamic_cast<ReturnInst *>(call->getCalledFunction()->getEntryBlock().getSingleSuccessor()->getTerminator()));
}

TEST_F(GenFncCallTests, Simple) {
    shared_ptr<IntAST> in = make_shared<IntAST>(10);
    in->expression_type = _TType::Int;

    FncCallAST c("tt", deque<shared_ptr<BaseAST>>{in}, 'F', nullopt);
    c.expression_type = _TType::Int;
    string asumed_name = mangle(c, nullopt);

    CallInst *call = dynamic_cast<CallInst *>(genFncCall(c));
    ASSERT_NE(call, nullptr);

    call->moveBefore(&call->getCalledFunction()->getEntryBlock().front());

    ASSERT_EQ(call->getNumArgOperands(), 1);

    ASSERT_EQ(call->getCalledFunction()->getName(), asumed_name);
    ASSERT_EQ(call->getCalledFunction()->getReturnType(), getLLVMType(_TType::Int));

    // Check return value

    Value *retres = dynamic_cast<ReturnInst *>(call->getCalledFunction()->getEntryBlock().getSingleSuccessor()->getTerminator())->getReturnValue();
    ASSERT_TRUE(retres->getType()->isIntegerTy());
}
