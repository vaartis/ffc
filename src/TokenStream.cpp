#include "TokenStream.hpp"

using namespace std;

long TokenStream::length() {
    return vec.size();
}

pair<Token, string> TokenStream::get() {
    if (index++ >= vec.size())
        throw TokenStream::EOFException();
    return vec[index - 1];
}

pair<Token, string> TokenStream::peek() {
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

pair<Token, string> TokenStream::getTok() {
    string IdentStr;

    while (isspace(lastchr))
        lastchr = text->get();

    if (lastchr == '"') {
        lastchr = text->get();
        while (lastchr != '"') {
            IdentStr += lastchr;
            lastchr = text->get();
        }
        lastchr = text->get();
        return {Token::StrLit, IdentStr};
    }

    static vector<char> op_chars = {'!','~','@','#','$','%','^','&','*','-','+','\\','/','<','>','='};

    if (any_of(begin(op_chars), --end(op_chars), [&](char c) { return lastchr == c; })) {
        IdentStr = lastchr;
        lastchr = text->get();
        while (any_of(begin(op_chars), end(op_chars), [&](char c) { return lastchr == c; })) {
            IdentStr += lastchr;
            lastchr = text->get();
        }
        return {Token::Operator, IdentStr};
    }

    bool f = false;
    if (isalpha(lastchr) || lastchr == '_') {
        f = true;
        IdentStr = lastchr;
        while (isalnum((lastchr = text->get())) || lastchr == '_')
            IdentStr += lastchr;
    }

    if (lastchr == '=') {
        IdentStr = lastchr;
        lastchr = text->get();
        if (lastchr == '=') {
            IdentStr += lastchr;
            lastchr = text->get();
            return {Token::Operator, IdentStr};
        } else {
            lastchr = text->get();
            return {Token::Eq, IdentStr};
        }
    }

    if (isdigit(lastchr)) {
        string numstr;
        bool f = false;

        do {
            if (lastchr == '.')
                f = true;
            numstr += lastchr;
            lastchr = text->get();
        } while (isdigit(lastchr) || lastchr == '.');

        if (!f)
            return {Token::IntLit, numstr};
        else
            return {Token::FloatLit, numstr};
    }

    #define match(wh, to, type) if (wh == to)\
            return {type, IdentStr};

    #define match_char(to, type) if (lastchr == to) {\
        lastchr = text->get();\
        IdentStr = to;\
        return {type, IdentStr};\
    }

    match(IdentStr, "==", Token::Operator);
    match(IdentStr, "fnc", Token::Fnc);
    match(IdentStr, "extern", Token::Extern);
    match(IdentStr, "operator", Token::OperatorDef);
    match(IdentStr, "include", Token::Include);
    match(IdentStr, "type", Token::TypeDef);

    if (any_of(begin(types), end(types), [&](string s) { return s == IdentStr; })) {
        return {Token::Type, IdentStr};
    }

    match(IdentStr, "true", Token::BoolLit);
    match(IdentStr, "false", Token::BoolLit);

    match(IdentStr, "if", Token::If);
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
    }

    return {Token::Ident, IdentStr};
}

