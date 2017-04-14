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

struct TokenInfo {
    public:
        TokenInfo(Token t, string i, long x, long y) : tok(t), IdentStr(i), symbol(x), line(y) {}
        Token tok;
        string IdentStr;
        struct { long symbol; long line; };
};

class TokenStream {
    public:
        TokenStream(string s);

        struct EOFException : exception {};

        char getChar();
        long length();
        TokenInfo get();
        TokenInfo peek();
        vector<string> getTypes();
    private:
        long line = 1, symbol = 1;
        long index = 0;
        unique_ptr<stringstream> text;
        vector<TokenInfo> vec;
        char lastchr = ' ';
        vector<string> types;

        TokenInfo getTok();
};
