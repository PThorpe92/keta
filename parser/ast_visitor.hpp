#include "../lexer/token.hpp"
#include <memory>
#include <sys/types.h>
#include <vector>

class AstVisitor {

public:
  Token *current_token;
  uint token_index;
  std::vector<TokenSpan> tokens;
  std::vector<std::unique_ptr<AstNode>> nodes;
  AstVisitor() = default;
  AstVisitor(std::vector<TokenSpan> tokens) : tokens(tokens) {}
  ~AstVisitor() = default;
};
