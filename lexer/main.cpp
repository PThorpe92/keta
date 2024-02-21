#include "lexer.hpp"
#include <fstream>
#include <iostream>
#include <sstream>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cout << "Usage: ./lexer <input_filename> <output_filename> "
              << std::endl;
    return 1;
  }
  std::string filename = argv[1];
  std::ifstream file(filename);
  if (!file) {
    std::cout << "Failed to open file: " << filename << std::endl;
    return 1;
  }
  std::ostringstream oss;
  oss << file.rdbuf();
  std::string input = oss.str();
  file.close();

  Lexer new_lexer(input);
  new_lexer.parse_tokens();
  new_lexer.print_tokens();
  return 0;
}
