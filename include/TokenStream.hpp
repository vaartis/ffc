#pragma once

#include <string>
#include <sstream>
#include <vector>

#include "ParserShared.hpp"

using std::string;
using std::exception;
using std::pair;
using std::vector;
using std::stringstream;
using std::unique_ptr;

class TokenStream {
    public:
        TokenStream(string s);

        struct EOFException : exception {};

        long length();
        pair<Token, string> get();
        pair<Token, string> peek();
        vector<string> getTypes();
    private:
        long index = 0;
        unique_ptr<stringstream> text;
        vector< pair<Token, string> > vec;
        char lastchr = ' ';
        vector<string> types;

        pair<Token, string> getTok();
};
