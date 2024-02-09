# Compiler
CC = g++

# Compiler flags
CFLAGS = -Wall -Wextra -std=c++23 -g

# Source files
SRCS = main.cpp token.cpp lexer.cpp

# Object files
OBJS = $(SRCS:.cpp=.o)

# Executable name
TARGET = lexer

# Default target
all: $(TARGET)

# Linking object files to generate the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

# Compiling C++ source files
%.o: %.cpp
	$(CC) $(CFLAGS) -c -o $@ $<

# Clean the generated files
clean:
	rm main.o lexer.o token.o 
