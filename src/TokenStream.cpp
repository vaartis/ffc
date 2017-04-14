#include "TokenStream.hpp"

using namespace std;

long TokenStream::length() {
    return vec.size();
}

TokenInfo TokenStream::get() {
    if (index++ >= vec.size())
        throw TokenStream::EOFException();
    return vec[index - 1];
}

TokenInfo TokenStream::peek() {
    return vec[index];
}

vector<string> TokenStream::getTypes() {
    return types;
}


TokenStream::TokenStream(string s) {
    text = make_unique<stringstream>(s);

    types = { "int", "float", "bool", "str" };

    while (!text->eof()) {
        vec.push_back(getTok());
    }
}

char TokenStream::getChar() {
    char res = text->get();
    symbol++;
    if (res == '\n') {
        line++;
        symbol = 0;
    }
    return res;
}

TokenInfo TokenStream::getTok() {
    string IdentStr;

    while (isspace(lastchr))
        lastchr = getChar();

    if (lastchr == '"') {
        lastchr = getChar();
        while (lastchr != '"') {
            IdentStr += lastchr;
            lastchr = getChar();
        }
        lastchr = getChar();

        return TokenInfo(Token::StrLit, IdentStr, symbol, line);
    }

    static vector<char> op_chars = {'!','~','@','#','$','%','^','&','*','-','+','\\','/','<','>','='};

    if (any_of(begin(op_chars), --end(op_chars), [&](char c) { return lastchr == c; })) {
        IdentStr = lastchr;
        lastchr = getChar();
        while (any_of(begin(op_chars), end(op_chars), [&](char c) { return lastchr == c; })) {
            IdentStr += lastchr;
            lastchr = getChar();
        }
        return TokenInfo(Token::Operator, IdentStr, symbol, line);
    }

    bool f = false;
    if (isalpha(lastchr) || lastchr == '_') {
        f = true;
        IdentStr = lastchr;
        while (isalnum((lastchr = getChar())) || lastchr == '_')
            IdentStr += lastchr;
    }

    if (lastchr == '=') {
        IdentStr = lastchr;
        lastchr = getChar();
        if (lastchr == '=') {
            IdentStr += lastchr;
            lastchr = getChar();
            return TokenInfo(Token::Operator, IdentStr, symbol, line);
        } else {
            lastchr = getChar();
            return TokenInfo(Token::Eq, IdentStr, symbol, line);
        }
    }

    if (isdigit(lastchr)) {
        string numstr;
        bool f = false;

        do {
            if (lastchr == '.')
                f = true;
            numstr += lastchr;
            lastchr = getChar();
        } while (isdigit(lastchr) || lastchr == '.');

        if (!f)
            return TokenInfo(Token::IntLit, numstr, symbol, line);
        else
            return TokenInfo(Token::FloatLit, numstr, symbol, line);
    }

    #define match(wh, to, type) if (wh == to)\
            return TokenInfo(type, IdentStr, symbol, line);

    #define match_char(to, type) if (lastchr == to) {\
        lastchr = getChar();\
        IdentStr = to;\
        return TokenInfo(type, IdentStr, symbol, line);\
    }

    match(IdentStr, "==", Token::Operator);
    match(IdentStr, "fnc", Token::Fnc);
    match(IdentStr, "extern", Token::Extern);
    match(IdentStr, "operator", Token::OperatorDef);
    match(IdentStr, "include", Token::Include);
    match(IdentStr, "type", Token::TypeDef);
    match(IdentStr, "ref", Token::Ref);

    if (any_of(begin(types), end(types), [&](string s) { return s == IdentStr; })) {
        return TokenInfo(Token::Type, IdentStr, symbol, line);
    }

    match(IdentStr, "true", Token::BoolLit);
    match(IdentStr, "false", Token::BoolLit);

    match(IdentStr, "if", Token::If);
    match(IdentStr, "while", Token::While);
    match(IdentStr, "else", Token::Else);
    match(IdentStr, "ret", Token::Ret);

    if (!f) {
        match_char('(', Token::OpP);
        match_char(')', Token::ClP);
        match_char('{', Token::OpCB);
        match_char('}', Token::ClCB);
        match_char('=', Token::Eq);
        match_char(';', Token::Semicolon);
        match_char('.', Token::Dot);
        match_char(',', Token::Comma);
    }

    return TokenInfo(Token::Ident, IdentStr, symbol, line);
}
