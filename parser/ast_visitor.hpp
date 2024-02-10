#include "ast_node.hpp"
#include <memory>
#include <sys/types.h>
#include <vector>

class AstVisitor {

public:
  std::vector<TokenSpan> tokens;
  std::vector<std::unique_ptr<AstNode>> nodes;
  AstNode *root;
  virtual void visit(AstNode *node) = 0;
  AstVisitor() = default;
  AstVisitor(std::vector<TokenSpan> tokens) : tokens(tokens) {}
  ~AstVisitor() = default;
};
