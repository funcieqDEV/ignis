//
// Created by Funcieq on 27.11.2025.
//

#ifndef IGNIS_PARSER_H
#define IGNIS_PARSER_H
#include <vector>
#include "../token/token.h"
#include "../ast/node.h"

class Parser {
    public:
    std::vector<Token> tokens;
    std::vector<Node> Parse(std::vector<Token> tokens);
};

#endif //IGNIS_PARSER_H