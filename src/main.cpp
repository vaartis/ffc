#include <iostream>
#include <fstream>

#include "CodeGen.hpp"

using namespace std;
using namespace llvm;

int main(int argc, char *argv[]) {
    if (argc < 2) {
        cout << "No filename" << endl;
        return 1;
    }

    CodeGen codegen(argv[1]);

    Linker l(*codegen.module);
    for (auto &in : codegen.includes) {
        l.linkInModule(move(in));
    }

    PassManager<Module> pm;
    AnalysisManager<Module> am;
    pm.addPass(AlwaysInlinerPass());

    pm.run(*codegen.module, am);

    string fname = string(argv[1]);
    fname = fname.substr(0, fname.length() - 3);

    ofstream file(fname + ".ll");
    raw_os_ostream outfile(file);

    codegen.module->print(outfile, nullptr);

    return 0;
}
