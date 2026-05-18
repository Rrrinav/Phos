#include "../src/core/instruction/instruction.hpp"
#include "../src/frontend/lexer/token.hpp"

#include <filesystem>
#include <fstream>
#include <meta>
#include <print>
#include <sstream>

constexpr auto gen_instruction_cpp(std::filesystem::path out_file)
{
    std::println("[PHOS: META]: Building '{}' file", out_file.string());

    std::ofstream outfile(out_file, std::ios::out);

    constexpr static auto instruction_enumerators = std::define_static_array(std::meta::enumerators_of(^^phos::vm::Opcode));

    std::println(outfile, "#include \"instruction.hpp\"");
    std::println(outfile);
    std::println(outfile, "#include <format>");
    std::println(outfile);

    std::stringstream opcode_to_string_fn{};
    std::stringstream string_to_opcode_fn{};

    std::println(opcode_to_string_fn, "std::string phos::vm::opcode_to_string(Opcode code)");

    std::println(opcode_to_string_fn, "{{");
    std::println(opcode_to_string_fn, "    switch(code) {{");

    std::println(string_to_opcode_fn, "phos::vm::Opcode phos::vm::string_to_opcode(std::string code)");

    std::println(string_to_opcode_fn, "{{");

    template for (constexpr auto e : instruction_enumerators) {
        std::string to_print = std::format("case Opcode::{}:", std::meta::identifier_of(e));

        std::println(opcode_to_string_fn, "    {:<40} return \"{}\";", to_print, std::meta::identifier_of(e));
        std::println(string_to_opcode_fn, "    if (code == \"{}\") {{", std::meta::identifier_of(e));
        std::println(string_to_opcode_fn, "        return Opcode::{};", std::meta::identifier_of(e));
        std::println(string_to_opcode_fn, "    }}");
    }

    std::println(opcode_to_string_fn, "    }}");
    std::println(opcode_to_string_fn, "    return std::format(\"UNKNOWN_{{}}\", static_cast<uint8_t>(code));");
    std::println(opcode_to_string_fn, "}}");
    std::println(string_to_opcode_fn, "    return Opcode::Return;");
    std::println(string_to_opcode_fn, "}}");

    std::print(outfile, "{}", opcode_to_string_fn.str());
    std::println(outfile);
    std::print(outfile, "{}", string_to_opcode_fn.str());

    std::println("[PHOS: META]: Built '{}' file", out_file.string());
}


constexpr auto gen_token_cpp(std::filesystem::path out_file)
{
    std::println("[PHOS: META]: Building '{}' file", out_file.string());

    std::ofstream outfile(out_file, std::ios::out);

    constexpr static auto instruction_enumerators = std::define_static_array(std::meta::enumerators_of(^^phos::lex::TokenType));

    std::println(outfile, "#include \"token.hpp\"");
    std::println(outfile);
    std::println(outfile, "#include <string>\n#include <format>");
    std::println(outfile);

    std::stringstream token_to_string_fn{};

    std::println(token_to_string_fn, "std::string phos::lex::token_to_string(phos::lex::TokenType t)");

    std::println(token_to_string_fn, "{{");
    std::println(token_to_string_fn, "    switch(t) {{");

    template for (constexpr auto e : instruction_enumerators) {
        std::string to_print = std::format("case TokenType::{}:", std::meta::identifier_of(e));
        std::println(token_to_string_fn, "    {:<40} return \"{}\";", to_print, std::meta::identifier_of(e));
    }

    std::println(token_to_string_fn, "    }}");
    std::println(token_to_string_fn, "    return std::format(\"UNKNOWN_{{}}\", static_cast<uint8_t>(t));");
    std::println(token_to_string_fn, "}}");

    std::print(outfile, "{}", token_to_string_fn.str());
    std::println(outfile);

    auto operator_token_to_string_fn = R"(
std::string phos::lex::operator_token_to_string(phos::lex::TokenType type)
{
    static const std::unordered_map<phos::lex::TokenType, std::string> token_names = {
        {phos::lex::TokenType::Plus, "+"},
        {phos::lex::TokenType::Minus, "-"},
        {phos::lex::TokenType::Star, "*"},
        {phos::lex::TokenType::Slash, "/"},
        {phos::lex::TokenType::Percent, "%"},
        {phos::lex::TokenType::Equal, "=="},
        {phos::lex::TokenType::NotEqual, "!="},
        {phos::lex::TokenType::Less, "<"},
        {phos::lex::TokenType::Greater, ">"},
        {phos::lex::TokenType::LessEqual, "<="},
        {phos::lex::TokenType::GreaterEqual, ">="},
        {phos::lex::TokenType::LogicalAnd, "&&"},
        {phos::lex::TokenType::LogicalOr, "||"},
        {phos::lex::TokenType::LogicalNot, "!"}
    };
    auto it = token_names.find(type);
    return it != token_names.end() ? it->second : "unknown_operator";
}
    )";

    std::println(outfile, "{}", operator_token_to_string_fn);

    std::println("[PHOS: META]: Built '{}' file", out_file.string());
}

int main()
{
    gen_instruction_cpp("./src/core/instruction/instruction.cpp");
    gen_token_cpp("./src/frontend/lexer/token.cpp");
}
