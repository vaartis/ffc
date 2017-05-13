CC = g++
LIBS = -lfl
CFLAGS = -std=c++11

FLEX_SRC = ffc.flex
FLEX_OUT = ffc.flex.cpp

SRC = $(FLEX_OUT) output.cpp
HPP = tokens.hpp
OBJS = $(SRC:.cpp=.o)

LEXER = lexer

all: $(HPP) Makefile $(LEXER)

$(LEXER): $(OBJS)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

.cpp.o:
	$(CC) $(CFLAGS) -c $<


$(FLEX_OUT): $(FLEX_SRC)
	flex -o $@ $<

debug: CFLAGS += -ggdb
debug: clean $(LEXER)

clean:
	rm -f $(FLEX_OUT) $(LEXER) $(OBJS)
