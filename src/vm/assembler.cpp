#include "assembler.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <sstream>
#include <vector>

namespace phos::vm {

static std::string trim(std::string s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) { return !std::isspace(ch); }));
    s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) { return !std::isspace(ch); }).base(), s.end());
    return s;
}

static std::string escape_string(std::string_view input)
{
    std::string out;
    out.reserve(input.length() + 2);
    for (char c : input) {
        switch (c) {
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\t':
            out += "\\t";
            break;
        case '\\':
            out += "\\\\";
            break;
        case '\"':
            out += "\\\"";
            break;
        default:
            out += c;
            break;
        }
    }
    return out;
}

static std::string unescape_string(std::string_view input)
{
    std::string out;
    out.reserve(input.length());
    for (size_t i = 0; i < input.length(); ++i) {
        if (input[i] == '\\' && i + 1 < input.length()) {
            char next = input[++i];
            switch (next) {
            case 'n':
                out += '\n';
                break;
            case 'r':
                out += '\r';
                break;
            case 't':
                out += '\t';
                break;
            case '\\':
                out += '\\';
                break;
            case '"':
                out += '"';
                break;
            default:
                out += '\\';
                out += next;
                break;
            }
        } else {
            out += input[i];
        }
    }
    return out;
}

std::string Assembler::disassemble_instruction(Instruction inst, const Closure_data *closure)
{
    Opcode op = inst.rrr.op;
    std::string name = opcode_to_string(op);

    std::string asm_str;
    std::string comment;

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
                comment = std::format("%r{} = \"{}\"", inst.ri.dst, escape_string(val.as_string()));
            }
        }
        break;

    case Opcode::Jump:
        asm_str = std::format("{:<14} .L{:04}", name, static_cast<uint32_t>(inst.i.imm));
        break;

    case Opcode::Jump_if_false:
        asm_str = std::format("{:<14} %r{}, .L{:04}", name, inst.ri.dst, inst.ri.imm);
        break;

    case Opcode::Load_nil:
    case Opcode::Load_true:
    case Opcode::Load_false:
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
    case Opcode::Sub_i64:
    case Opcode::Sub_f64:
    case Opcode::Mul_i64:
    case Opcode::Mul_f64:
    case Opcode::Div_i64:
    case Opcode::Div_f64:
    case Opcode::Mod_i64:
    case Opcode::Mod_f64:
    case Opcode::Eq_i64:
    case Opcode::Neq_i64:
    case Opcode::Lt_i64:
    case Opcode::Lte_i64:
    case Opcode::Gt_i64:
    case Opcode::Gte_i64:
    case Opcode::Eq_f64:
    case Opcode::Neq_f64:
    case Opcode::Lt_f64:
    case Opcode::Lte_f64:
    case Opcode::Gt_f64:
    case Opcode::Gte_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    default:
        asm_str = std::format("{:<14} ???", name);
        break;
    }

    if (!comment.empty()) {
        return std::format("{:<32} ; {}", asm_str, comment);
    }
    return asm_str;
}

std::string Assembler::serialize(const Closure_data &closure)
{
    std::stringstream ss;

    std::string func_name = "main";
    if (closure.name && closure.name->length > 0) {
        func_name = std::string(closure.name->chars, closure.name->length);
    }

    ss << std::format("@fn {}(arity: {}) {{\n", func_name, closure.arity);

    ss << "  .constants:\n";
    for (size_t i = 0; i < closure.constant_count; i++) {
        Value val = closure.constants[i];
        if (val.is_float()) {
            ss << std::format("    $K{:03} = f64 {}\n", i, val.as_float());
        } else if (val.is_integer()) {
            ss << std::format("    $K{:03} = i64 {}\n", i, val.as_int());
        } else if (val.is_string()) {
            ss << std::format("    $K{:03} = str \"{}\"\n", i, escape_string(val.as_string()));
        } else if (val.is_nil()) {
            ss << std::format("    $K{:03} = nil\n", i);
        } else {
            ss << std::format("    $K{:03} = unknown\n", i);
        }
    }

    if (closure.constant_count > 0) {
        ss << "\n";
    }

    ss << "  .code:\n";

    // First pass: find all instructions that are targets of a jump
    std::vector<bool> is_target(closure.code_count, false);
    for (size_t i = 0; i < closure.code_count; i++) {
        Opcode op = closure.code[i].rrr.op;
        if (op == Opcode::Jump) {
            is_target[closure.code[i].i.imm] = true;
        } else if (op == Opcode::Jump_if_false) {
            is_target[closure.code[i].ri.imm] = true;
        }
    }

    // Second pass: write the instructions, injecting labels where needed
    for (size_t i = 0; i < closure.code_count; i++) {
        if (is_target[i]) {
            ss << std::format("  .L{:04}:\n", i);
        }
        std::string inst_str = disassemble_instruction(closure.code[i], &closure);
        ss << std::format("    {}\n", inst_str);
    }

    ss << "}\n\n";

    return ss.str();
}

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
        size_t comment_pos = line.find(';');
        if (comment_pos != std::string::npos) {
            line = line.substr(0, comment_pos);
        }

        line = trim(line);
        if (line.empty()) {
            continue;
        }

        if (line.starts_with("@fn")) {
            size_t name_start = 4;
            size_t name_end = line.find('(');
            std::string name = line.substr(name_start, name_end - name_start);
            closure.name = static_cast<String_data *>(arena.allocate_bytes(sizeof(String_data) + name.length() + 1));
            closure.name->length = name.length();
            std::copy(name.begin(), name.end(), closure.name->chars);
            closure.name->chars[name.length()] = '\0';

            size_t arity_pos = line.find("arity:");
            closure.arity = std::stoi(line.substr(arity_pos + 6));
        } else if (line == ".constants:") {
            in_constants = true;
            in_code = false;
        } else if (line == ".code:") {
            in_constants = false;
            in_code = true;
        } else if (line == "}") {
            break;
        } else if (in_constants) {
            size_t eq_pos = line.find('=');
            if (eq_pos == std::string::npos) {
                continue;
            }

            std::string type_val = trim(line.substr(eq_pos + 1));

            if (type_val.starts_with("i64")) {
                temp_constants.push_back(Value(static_cast<int64_t>(std::stoll(type_val.substr(4)))));
            } else if (type_val.starts_with("f64")) {
                temp_constants.push_back(Value(static_cast<double>(std::stod(type_val.substr(4)))));
            } else if (type_val.starts_with("nil")) {
                temp_constants.push_back(Value(nullptr));
            } else if (type_val.starts_with("str")) {
                size_t first_quote = type_val.find('"');
                if (first_quote != std::string::npos) {
                    std::string escaped_content;
                    bool in_escape = false;

                    for (size_t i = first_quote + 1; i < type_val.length(); ++i) {
                        char c = type_val[i];
                        if (in_escape) {
                            escaped_content += c;
                            in_escape = false;
                        } else if (c == '\\') {
                            escaped_content += c;
                            in_escape = true;
                        } else if (c == '"') {
                            break;
                        } else {
                            escaped_content += c;
                        }
                    }
                    temp_constants.push_back(Value::make_string(arena, unescape_string(escaped_content)));
                }
            }
        } else if (in_code) {
            // Ignore label definitions completely during deserialization
            if (line.starts_with(".L") && line.ends_with(":")) {
                continue;
            }

            std::istringstream token_stream(line);
            std::string op_str;
            token_stream >> op_str;

            Opcode op = string_to_opcode(op_str);

            std::string args_str;
            std::getline(token_stream, args_str);

            std::vector<std::string> args;
            std::stringstream arg_ss(args_str);
            std::string item;
            while (std::getline(arg_ss, item, ',')) {
                args.push_back(trim(item));
            }

            auto parse_arg = [](const std::string &s) -> uint16_t {
                if (s.empty()) {
                    return 0;
                }
                return static_cast<uint16_t>(std::stoi(s.substr(2)));
            };

            auto parse_jump = [](const std::string &s) -> uint32_t {
                if (s.empty()) {
                    return 0;
                }
                if (s.starts_with(".L")) {
                    return static_cast<uint32_t>(std::stoul(s.substr(2)));
                }
                if (s.starts_with("@")) {
                    return static_cast<uint32_t>(std::stoul(s.substr(1)));
                }
                return 0;
            };

            if (op == Opcode::Load_const) {
                temp_code.push_back(Instruction::make_ri(op, parse_arg(args[0]), parse_arg(args[1])));
            } else if (op == Opcode::Jump) {
                temp_code.push_back(Instruction::make_i(op, parse_jump(args[0])));
            } else if (op == Opcode::Jump_if_false) {
                temp_code.push_back(Instruction::make_ri(op, parse_arg(args[0]), parse_jump(args[1])));
            } else if (op == Opcode::Return || op == Opcode::Print) {
                temp_code.push_back(Instruction::make_rrr(op, parse_arg(args[0]), 0, 0));
            } else if (op == Opcode::Move) {
                temp_code.push_back(Instruction::make_rrr(op, parse_arg(args[0]), parse_arg(args[1]), 0));
            } else {
                temp_code.push_back(Instruction::make_rrr(op, parse_arg(args[0]), parse_arg(args[1]), parse_arg(args[2])));
            }
        }
    }

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
