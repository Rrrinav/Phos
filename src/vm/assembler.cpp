#include "assembler.hpp"

#include <algorithm>
#include <cctype>
#include <sstream>
#include <vector>

namespace phos::vm {

// Helper to strip whitespace
static std::string trim(std::string s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) { return !std::isspace(ch); }));
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) { return !std::isspace(ch); }).base(), s.end());
    return s;
}

std::string Assembler::disassemble_instruction(Instruction inst, const Closure_data *closure)
{
    Opcode op = inst.rrr.op;
    std::string name = opcode_to_string(op);

    std::string asm_str;
    std::string comment;

    // Decode using LLVM-style Sigils: % (Register) and $ (Constant/Immediate)
    switch (op) {
    case Opcode::Load_const:
        asm_str = std::format("{:<14} %r{}, $K{:03}", name, inst.ri.dst, inst.ri.imm);
        if (closure && inst.ri.imm < closure->constant_count) {
            Value val = closure->constants[inst.ri.imm];
            if (val.is_integer()) {
                comment = std::format("%r{} = {}", inst.ri.dst, val.as_int());
            } else if (val.is_float()) {
                comment = std::format("%r{} = {}", inst.ri.dst, val.as_float());
            } else if (val.is_string()) {
                comment = std::format("%r{} = \"{}\"", inst.ri.dst, val.as_string());
            }
        }
        break;

    case Opcode::Load_nil:
    case Opcode::Print:
    case Opcode::Return:
        asm_str = std::format("{:<14} %r{}", name, inst.rrr.dst);
        break;

    case Opcode::Move:
        asm_str = std::format("{:<14} %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a);
        comment = std::format("%r{} = %r{}", inst.rrr.dst, inst.rrr.src_a);
        break;

    case Opcode::Add_i64:
    case Opcode::Add_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{} + %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Sub_i64:
    case Opcode::Sub_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{} - %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Mul_i64:
    case Opcode::Mul_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{} * %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Div_i64:
    case Opcode::Div_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{} / %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Mod_i64:
    case Opcode::Mod_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{} % %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    default:
        asm_str = std::format("{:<14} ???", name);
        break;
    }

    // If we generated a comment, align it neatly to column 32 (using standard ';' for comments)
    if (!comment.empty()) {
        return std::format("{:<32} ; {}", asm_str, comment);
    }
    return asm_str;
}

std::string Assembler::serialize(const Closure_data &closure)
{
    std::stringstream ss;

    // Resolve Function Name safely
    std::string func_name = "main";
    if (closure.name && closure.name->length > 0) {
        func_name = std::string(closure.name->chars, closure.name->length);
    }

    // 1. Semantic Function Definition Block
    ss << std::format("@fn {}(arity: {}) {{\n", func_name, closure.arity);

    // 2. Constants Block
    ss << "  .constants:\n";
    for (size_t i = 0; i < closure.constant_count; i++) {
        Value val = closure.constants[i];
        if (val.is_float()) {
            ss << std::format("    $K{:03} = f64 {}\n", i, val.as_float());
        } else if (val.is_integer()) {
            ss << std::format("    $K{:03} = i64 {}\n", i, val.as_int());
        } else if (val.is_string()) {
            ss << std::format("    $K{:03} = str \"{}\"\n", i, val.as_string());
        } else {
            ss << std::format("    $K{:03} = unknown\n", i);
        }
    }

    if (closure.constant_count > 0) {
        ss << "\n";
    }

    // 3. Executable Code Block
    ss << "  .code:\n";
    for (size_t i = 0; i < closure.code_count; i++) {
        // No line numbers. Just pure, parsable instruction text.
        std::string inst_str = disassemble_instruction(closure.code[i], &closure);
        ss << std::format("    {}\n", inst_str);
    }

    // Close the function scope
    ss << "}\n\n";

    return ss.str();
}

// Inverse mapping for our opcodes
Closure_data Assembler::deserialize(const std::string &ir_source, mem::Arena &arena)
{
    Closure_data closure{};
    std::vector<Value> temp_constants;
    std::vector<Instruction> temp_code;

    std::istringstream ss(ir_source);
    std::string line;

    bool in_constants = false;
    bool in_code = false;

    while (std::getline(ss, line)) {
        // Strip inline comments starting with ';'
        size_t comment_pos = line.find(';');
        if (comment_pos != std::string::npos) {
            line = line.substr(0, comment_pos);
        }

        line = trim(line);
        if (line.empty()) {
            continue;
        }

        // --- 1. Parse Header ---
        if (line.starts_with("@fn")) {
            // Very naive extraction: @fn main(arity: 0) {
            size_t name_start = 4;
            size_t name_end = line.find('(');
            std::string name = line.substr(name_start, name_end - name_start);
            closure.name = static_cast<String_data *>(arena.allocate_bytes(sizeof(String_data) + name.length() + 1));
            closure.name->length = name.length();
            std::copy(name.begin(), name.end(), closure.name->chars);
            closure.name->chars[name.length()] = '\0';

            // Extract Arity
            size_t arity_pos = line.find("arity:");
            closure.arity = std::stoi(line.substr(arity_pos + 6));
        } else if (line == ".constants:") {
            in_constants = true;
            in_code = false;
        } else if (line == ".code:") {
            in_constants = false;
            in_code = true;
        } else if (line == "}") {
            break; // End of function block
        }
        // --- 2. Parse Constants ---
        else if (in_constants) {
            // Expected format: $K000 = i64 100
            size_t eq_pos = line.find('=');
            if (eq_pos == std::string::npos) {
                continue;
            }

            std::string type_val = trim(line.substr(eq_pos + 1));

            if (type_val.starts_with("i64")) {
                temp_constants.push_back(Value(static_cast<int64_t>(std::stoll(type_val.substr(4)))));
            } else if (type_val.starts_with("f64")) {
                temp_constants.push_back(Value(static_cast<double>(std::stod(type_val.substr(4)))));
            } else if (type_val.starts_with("str")) {
                // Extract string between quotes
                size_t first_quote = type_val.find('"');
                size_t last_quote = type_val.rfind('"');
                std::string str_content = type_val.substr(first_quote + 1, last_quote - first_quote - 1);
                temp_constants.push_back(Value::make_string(arena, str_content));
            }
        }
        // --- 3. Parse Code ---
        else if (in_code) {
            // Expected format: Add_i64 %r2, %r0, %r1
            std::istringstream token_stream(line);
            std::string op_str;
            token_stream >> op_str;

            Opcode op = string_to_opcode(op_str);

            // Extract the comma separated arguments safely
            std::string args_str;
            std::getline(token_stream, args_str);

            std::vector<std::string> args;
            std::stringstream arg_ss(args_str);
            std::string item;
            while (std::getline(arg_ss, item, ',')) {
                args.push_back(trim(item));
            }

            // Helper to pull out the integer from %rX or $KX
            auto parse_arg = [](const std::string &s) -> uint16_t {
                if (s.empty()) {
                    return 0;
                }
                // Skips the '%' or '$' and the 'r' or 'K'
                return static_cast<uint16_t>(std::stoi(s.substr(2)));
            };

            // Build Instruction based on Opcode format
            if (op == Opcode::Load_const) {
                temp_code.push_back(Instruction::make_ri(op, parse_arg(args[0]), parse_arg(args[1])));
            } else if (op == Opcode::Return || op == Opcode::Print) {
                temp_code.push_back(Instruction::make_rrr(op, parse_arg(args[0]), 0, 0));
            } else if (op == Opcode::Move) {
                temp_code.push_back(Instruction::make_rrr(op, parse_arg(args[0]), parse_arg(args[1]), 0));
            } else {
                // RRR Math Opcodes
                temp_code.push_back(Instruction::make_rrr(op, parse_arg(args[0]), parse_arg(args[1]), parse_arg(args[2])));
            }
        }
    }

    // --- 4. Permanently Allocate to Arena ---
    // This guarantees the VM can execute it safely without vector reallocation deleting the pointers.
    closure.constant_count = temp_constants.size();
    if (closure.constant_count > 0) {
        closure.constants = arena.allocate<Value>(closure.constant_count);
        std::copy(temp_constants.begin(), temp_constants.end(), closure.constants);
    }

    closure.code_count = temp_code.size();
    if (closure.code_count > 0) {
        closure.code = arena.allocate<Instruction>(closure.code_count);
        std::copy(temp_code.begin(), temp_code.end(), closure.code);
    }

    return closure;
}

} // namespace phos::vm
