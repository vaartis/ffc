CC = g++
LIBS = -lfl
CFLAGS = -std=c++11

FLEX_SRC = ffc.flex
FLEX_OUT = ffc.flex.cpp

LEXER = lexer

$(LEXER): $(FLEX_OUT)
	$(CC) -o $@ $< $(CFLAGS) $(LIBS)

$(FLEX_OUT): $(FLEX_SRC)
	flex -o $@ $<

debug: CFLAGS += -ggdb
debug: clean $(LEXER)

clean:
	rm -f $(FLEX_OUT) $(LEXER)
