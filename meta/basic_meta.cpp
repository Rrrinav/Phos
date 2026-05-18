#include "../src/core/instruction/instruction.hpp"

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

    std::println("[PHOS: META]: Building '{}' file", out_file.string());
}

int main()
{
    gen_instruction_cpp("./src/core/instruction/instruction.cpp");
}
