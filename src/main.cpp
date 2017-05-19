#include <iostream>
#include <fstream>

#include "llvm/Linker/Linker.h"

#include "llvm/IR/PassManager.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"

#include "CodeGen.hpp"

using namespace std;
using namespace llvm;

int main(int argc, char *argv[]) {
    if (argc < 2) {
        cout << "No filename" << endl;
        return 1;
    }

    CodeGen codegen(argv[1]);

    for (auto &in : codegen.includes) {
        Linker::linkModules(*codegen.module, move(in));
    }

    PassManager<Module> pm;
    AnalysisManager<Module> am;
    pm.addPass(AlwaysInlinerPass());

    pm.run(*codegen.module, am);

    string fname = string(argv[1]);
    fname = fname.substr(0, fname.rfind("."));

    ofstream file(fname + ".ll");
    raw_os_ostream outfile(file);

    codegen.module->print(outfile, nullptr);

    return 0;
}
