#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>

#include "../value/value.hpp"

namespace phos::lex
{

// Token types

enum class TokenType : uint8_t
{
    // Literals
    Integer64, Float64, String, Bool, Nil,

    //  Identifiers
    Identifier,

    // Primitive type keywords
    // Signed integers
    TInt64, TInt32, TInt16, TInt8,
    // Unsigned integers
    TUInt64, TUInt32, TUInt16, TUInt8,
    // Floats
    TFloat64, TFloat32,
    // Other primitives
    TBool, TString, TVoid, TAny,

    //  Binding keywords
    Let, Mut, Const,

    //  Control flow
    If, Else, While, For, In, Return, Match,

    //  Declarations
    Fn, Static, Model, Union, Bind,

    //  Concurrency
    Spawn, Await, Yield,

    //  Built-ins
    Print, PrintErr, As, This,

    //  Arithmetic
    Plus, Minus, Star, Slash, Percent,

    //  Compound assignment
    PlusEqual, MinusEqual, StarEqual, SlashEqual,

    //  Comparison
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,

    //  Logical
    LogicalAnd, LogicalOr, LogicalNot,

    //  Bitwise
    BitAnd, Pipe, BitXor, BitNot, BitLShift, BitRshift,

    //  Assignment / arrows
    Assign,       // =
    Arrow,        // ->
    FatArrow,     // =>

    //  Ranges
    DotDot,       // ..
    DotDotEq,     // ..=

    //  Punctuation
    Dot, Comma, Colon, ColonColon, Semicolon, Question,

    //  Delimiters
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    LeftBracket, RightBracket,

    //  Special patterns
    Underscore,   // _   wildcard

    //  String interpolation
    Fstring,      // f"..."  raw template stored as literal

    //  Meta
    Newline, Eof, Invalid,
};

//  Token

struct Token
{
    TokenType   type;
    std::string lexeme;
    Value       literal;
    size_t      line;
    size_t      column;

    Token(TokenType type, std::string lexeme, Value literal, size_t line, size_t column)
        : type(type)
        , lexeme(std::move(lexeme))
        , literal(std::move(literal))
        , line(line)
        , column(column)
    {}
};

//  Keyword map

static const std::unordered_map<std::string_view, TokenType> token_keywords =
{
    // binding
    { "let",       TokenType::Let       },
    { "mut",       TokenType::Mut       },
    { "const",     TokenType::Const     },

    // control flow
    { "if",        TokenType::If        },
    { "else",      TokenType::Else      },
    { "while",     TokenType::While     },
    { "for",       TokenType::For       },
    { "in",        TokenType::In        },
    { "return",    TokenType::Return    },
    { "match",     TokenType::Match     },

    // declarations
    { "fn",        TokenType::Fn        },
    { "static",    TokenType::Static    },
    { "model",     TokenType::Model     },
    { "union",     TokenType::Union     },
    { "bind",      TokenType::Bind      },

    // concurrency
    { "spawn",     TokenType::Spawn     },
    { "await",     TokenType::Await     },
    { "yield",     TokenType::Yield     },

    // built-ins
    { "print",     TokenType::Print     },
    { "print_err", TokenType::PrintErr  },
    { "as",        TokenType::As        },
    { "this",      TokenType::This      },
    { "nil",       TokenType::Nil       },

    // boolean literals
    { "true",      TokenType::Bool      },
    { "false",     TokenType::Bool      },

    // wildcard
    { "_",         TokenType::Underscore },

    // type keywords
    { "i64",       TokenType::TInt64    },
    { "i32",       TokenType::TInt32    },
    { "i16",       TokenType::TInt16    },
    { "i8",        TokenType::TInt8     },
    { "u64",       TokenType::TUInt64   },
    { "u32",       TokenType::TUInt32   },
    { "u16",       TokenType::TUInt16   },
    { "u8",        TokenType::TUInt8    },
    { "f64",       TokenType::TFloat64  },
    { "f32",       TokenType::TFloat32  },
    { "bool",      TokenType::TBool     },
    { "string",    TokenType::TString   },
    { "void",      TokenType::TVoid     },
    { "any",       TokenType::TAny      },
};

//  Utilities

std::string token_to_string(TokenType t);

} // namespace phos::lex
