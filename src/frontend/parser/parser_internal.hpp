#pragma once

// Result-propagation macros shared by the parser translation units.
// (Split out of parser.cpp when it was divided by responsibility.)

#define PHOS_PARSER_CONCAT_IMPL(x, y) x##y
#define PHOS_PARSER_CONCAT(x, y) PHOS_PARSER_CONCAT_IMPL(x, y)

#define ASSIGN_OR_RETURN(var, expr)                                                                                                                  \
    auto PHOS_PARSER_CONCAT(_res_, __LINE__) = (expr);                                                                                               \
    if (!PHOS_PARSER_CONCAT(_res_, __LINE__))                                                                                                        \
        return std::unexpected(PHOS_PARSER_CONCAT(_res_, __LINE__).error());                                                                         \
    var = std::move(PHOS_PARSER_CONCAT(_res_, __LINE__).value())

#define DECL_OR_RETURN(var, expr)                                                                                                                    \
    auto PHOS_PARSER_CONCAT(_res_, __LINE__) = (expr);                                                                                               \
    if (!PHOS_PARSER_CONCAT(_res_, __LINE__))                                                                                                        \
        return std::unexpected(PHOS_PARSER_CONCAT(_res_, __LINE__).error());                                                                         \
    auto var = std::move(PHOS_PARSER_CONCAT(_res_, __LINE__).value())

#define TRY_IGNORE(expr)                                                                                                                             \
    do {                                                                                                                                             \
        auto PHOS_PARSER_CONCAT(_res_, __LINE__) = (expr);                                                                                           \
        if (!PHOS_PARSER_CONCAT(_res_, __LINE__))                                                                                                    \
            return std::unexpected(PHOS_PARSER_CONCAT(_res_, __LINE__).error());                                                                     \
    } while (0)
