#include "assembler.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <sstream>
#include <unordered_map>
#include <vector>

namespace phos::vm {

static std::string primitive_to_string(types::Primitive_kind kind)
{
    switch (kind) {
    case types::Primitive_kind::I8:
        return "i8";
    case types::Primitive_kind::I16:
        return "i16";
    case types::Primitive_kind::I32:
        return "i32";
    case types::Primitive_kind::I64:
        return "i64";
    case types::Primitive_kind::U8:
        return "u8";
    case types::Primitive_kind::U16:
        return "u16";
    case types::Primitive_kind::U32:
        return "u32";
    case types::Primitive_kind::U64:
        return "u64";
    case types::Primitive_kind::F16:
        return "f16";
    case types::Primitive_kind::F32:
        return "f32";
    case types::Primitive_kind::F64:
        return "f64";
    case types::Primitive_kind::Bool:
        return "bool";
    default:
        return "unknown";
    }
}

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
            } else if (val.is_closure()) {
                std::string cname = "anon";
                if (val.as_closure() && val.as_closure()->name && val.as_closure()->name->length > 0) {
                    cname = std::string(val.as_closure()->name->chars, val.as_closure()->name->length);
                }
                comment = std::format("%r{} = <closure: {}>", inst.ri.dst, cname);
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

    case Opcode::Get_upvalue:
    case Opcode::Set_upvalue:
        asm_str = std::format("{:<14} %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a);
        comment = std::format("upval[{}]", inst.rrr.src_a);
        break;

    case Opcode::Make_closure:
        asm_str = std::format("{:<14} %r{}, $K{:03}", name, inst.ri.dst, inst.ri.imm);
        if (closure && inst.ri.imm < closure->constant_count) {
            Value val = closure->constants[inst.ri.imm];
            if (val.is_closure()) {
                std::string cname = "anon";
                if (val.as_closure() && val.as_closure()->name && val.as_closure()->name->length > 0) {
                    cname = std::string(val.as_closure()->name->chars, val.as_closure()->name->length);
                }
                comment = std::format("blueprint: <closure: {}>", cname);
            }
        }
        break;

    case Opcode::Call:
        asm_str = std::format("{:<14} %r{}, %r{}, argc: {}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Make_array:
        asm_str = std::format("{:<14} %r{}, %r{}, count: {}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = [%r{} ... %r{}]", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_a + inst.rrr.src_b - 1);
        break;

    case Opcode::Load_index:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{}[%r{}]", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Store_index:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{}[%r{}] = %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Make_model:
        asm_str = std::format("{:<14} %r{}, %r{}, fields: {}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = Model(%r{} ... %r{})", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_a + inst.rrr.src_b - 1);
        break;

    case Opcode::Load_field:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{} = %r{}.field[%r{}]", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Store_field:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        comment = std::format("%r{}.field[%r{}] = %r{}", inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;

    case Opcode::Add_i64:
    case Opcode::Add_u64:
    case Opcode::Add_f64:
    case Opcode::Sub_i64:
    case Opcode::Sub_u64:
    case Opcode::Sub_f64:
    case Opcode::Mul_i64:
    case Opcode::Mul_u64:
    case Opcode::Mul_f64:
    case Opcode::Div_i64:
    case Opcode::Div_u64:
    case Opcode::Div_f64:
    case Opcode::Mod_i64:
    case Opcode::Mod_u64:
    case Opcode::Mod_f64:
    case Opcode::Eq_i64:
    case Opcode::Neq_i64:
    case Opcode::Lt_i64:
    case Opcode::Lte_i64:
    case Opcode::Gt_i64:
    case Opcode::Gte_i64:
    case Opcode::Eq_u64:
    case Opcode::Neq_u64:
    case Opcode::Lt_u64:
    case Opcode::Lte_u64:
    case Opcode::Gt_u64:
    case Opcode::Gte_u64:
    case Opcode::Eq_f64:
    case Opcode::Neq_f64:
    case Opcode::Lt_f64:
    case Opcode::Lte_f64:
    case Opcode::Gt_f64:
    case Opcode::Gte_f64:
        asm_str = std::format("{:<14} %r{}, %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a, inst.rrr.src_b);
        break;
    case Opcode::Neg_i64:
    case Opcode::Neg_f64:
    case Opcode::Not:
    case Opcode::BitNot_i64:
    case Opcode::BitNot_u64:
        asm_str = std::format("{:<14} %r{}, %r{}", name, inst.rrr.dst, inst.rrr.src_a);
        break;
    case Opcode::Cast_i8:
    case Opcode::Cast_i16:
    case Opcode::Cast_i32:
    case Opcode::Cast_i64:
    case Opcode::Cast_u8:
    case Opcode::Cast_u16:
    case Opcode::Cast_u32:
    case Opcode::Cast_u64:
    case Opcode::Cast_f16:
    case Opcode::Cast_f32:
    case Opcode::Cast_f64:
        asm_str = std::format("{:<14} %r{}", name, inst.rrr.dst);
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

    // 1. Recursively find and print nested closures FIRST
    if (closure.constants != nullptr) {
        for (size_t i = 0; i < closure.constant_count; ++i) {
            if (closure.constants[i].is_closure() && closure.constants[i].as_closure() != nullptr) {
                Closure_data *nested = closure.constants[i].as_closure();
                // Skip native functions! They don't have bytecode to serialize.
                if (!nested->native_func.has_value()) {
                    ss << serialize(*nested);
                }
            }
        }
    }

    // 2. Print current closure
    std::string func_name = "main";
    if (closure.name && closure.name->length > 0) {
        func_name = std::string(closure.name->chars, closure.name->length);
    }

    ss << std::format("@fn {}(arity: {}) {{\n", func_name, closure.arity);

    ss << "  .constants:\n";
    for (size_t i = 0; i < closure.constant_count; i++) {
        Value val = closure.constants[i];
        if (val.is_bool()) {
            ss << std::format("    $K{:03} = bool {}\n", i, val.as_bool() ? "true" : "false");
        } else if (val.is_float()) {
            ss << std::format("    $K{:03} = {} {}\n", i, primitive_to_string(val.numeric_type()), val.to_string());
        } else if (val.is_integer()) {
            if (val.is_u_integer()) {
                ss << std::format("    $K{:03} = {} {}\n", i, primitive_to_string(val.numeric_type()), val.as_uint());
            } else {
                ss << std::format("    $K{:03} = {} {}\n", i, primitive_to_string(val.numeric_type()), val.as_int());
            }
        } else if (val.is_string()) {
            ss << std::format("    $K{:03} = str \"{}\"\n", i, escape_string(val.as_string()));
        } else if (val.is_nil()) {
            ss << std::format("    $K{:03} = nil\n", i);
        } else if (val.is_closure()) {
            std::string cname = "anon";
            if (val.as_closure() && val.as_closure()->name && val.as_closure()->name->length > 0) {
                cname = std::string(val.as_closure()->name->chars, val.as_closure()->name->length);
            }
            ss << std::format("    $K{:03} = <closure: {}>\n", i, cname);
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
        Instruction inst = closure.code[i];
        std::string inst_str = disassemble_instruction(inst, &closure);
        ss << std::format("    {}\n", inst_str);

        // Extract and format routing bytes instead of treating them as instructions
        if (inst.rrr.op == Opcode::Make_closure) {
            uint16_t const_idx = inst.ri.imm;
            if (const_idx < closure.constant_count && closure.constants[const_idx].is_closure()) {
                Closure_data *blueprint = closure.constants[const_idx].as_closure();
                for (size_t u = 0; u < blueprint->upvalue_count; u++) {
                    i++; // Consume the routing byte
                    if (i < closure.code_count) {
                        Instruction route = closure.code[i];
                        bool is_local = (route.rrr.src_a == 1);
                        uint8_t index = route.rrr.src_b;
                        ss << std::format("      .route is_local: {}, index: {}\n", is_local ? "true" : "false", index);
                    }
                }
            }
        }
    }

    ss << "}\n\n";

    return ss.str();
}

Closure_data Assembler::deserialize(const std::string &ir_source, mem::Arena &arena)
{
    std::istringstream ss(ir_source);
    std::string line;

    bool in_constants = false;
    bool in_code = false;

    // We hold all parsed closures until the end to resolve them.
    std::unordered_map<std::string, Closure_data *> parsed_closures;
    Closure_data *main_closure = nullptr;

    Closure_data *current_closure = nullptr;
    std::vector<Value> temp_constants;
    std::vector<Instruction> temp_code;

    // Deferred linking for hoisted closures in constants.
    struct DeferredLink
    {
        Closure_data *owner;
        size_t const_index;
        std::string target_name;
    };
    std::vector<DeferredLink> deferred_links;

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
            current_closure = arena.allocate<Closure_data>();
            new (current_closure) Closure_data();

            temp_constants.clear();
            temp_code.clear();

            size_t name_start = 4;
            size_t name_end = line.find('(');
            std::string name = line.substr(name_start, name_end - name_start);
            current_closure->name = static_cast<String_data *>(arena.allocate_bytes(sizeof(String_data) + name.length() + 1));
            current_closure->name->length = name.length();
            std::copy(name.begin(), name.end(), current_closure->name->chars);
            current_closure->name->chars[name.length()] = '\0';

            size_t arity_pos = line.find("arity:");
            current_closure->arity = std::stoi(line.substr(arity_pos + 6));

            in_constants = false;
            in_code = false;
        } else if (line == ".constants:") {
            in_constants = true;
            in_code = false;
        } else if (line == ".code:") {
            in_constants = false;
            in_code = true;
        } else if (line == "}") {
            // Finalize the current closure
            if (current_closure) {
                current_closure->constant_count = temp_constants.size();
                if (current_closure->constant_count > 0) {
                    current_closure->constants = arena.allocate<Value>(current_closure->constant_count);
                    std::copy(temp_constants.begin(), temp_constants.end(), current_closure->constants);
                }

                current_closure->code_count = temp_code.size();
                if (current_closure->code_count > 0) {
                    current_closure->code = arena.allocate<Instruction>(current_closure->code_count);
                    std::copy(temp_code.begin(), temp_code.end(), current_closure->code);
                }

                std::string c_name(current_closure->name->chars, current_closure->name->length);
                parsed_closures[c_name] = current_closure;

                if (c_name == "main") {
                    main_closure = current_closure;
                }

                current_closure = nullptr;
            }
            in_constants = false;
            in_code = false;
        } else if (in_constants && current_closure) {
            size_t eq_pos = line.find('=');
            if (eq_pos == std::string::npos) {
                continue;
            }

            std::string type_val = trim(line.substr(eq_pos + 1));

            if (type_val.starts_with("i8")) {
                temp_constants.push_back(Value(static_cast<int8_t>(std::stoi(type_val.substr(3)))));
            } else if (type_val.starts_with("i16")) {
                temp_constants.push_back(Value(static_cast<int16_t>(std::stoi(type_val.substr(4)))));
            } else if (type_val.starts_with("i32")) {
                temp_constants.push_back(Value(static_cast<int32_t>(std::stol(type_val.substr(4)))));
            } else if (type_val.starts_with("i64")) {
                temp_constants.push_back(Value(static_cast<int64_t>(std::stoll(type_val.substr(4)))));
            } else if (type_val.starts_with("u8")) {
                temp_constants.push_back(Value(static_cast<uint8_t>(std::stoul(type_val.substr(3)))));
            } else if (type_val.starts_with("u16")) {
                temp_constants.push_back(Value(static_cast<uint16_t>(std::stoul(type_val.substr(4)))));
            } else if (type_val.starts_with("u32")) {
                temp_constants.push_back(Value(static_cast<uint32_t>(std::stoul(type_val.substr(4)))));
            } else if (type_val.starts_with("u64")) {
                temp_constants.push_back(Value(static_cast<uint64_t>(std::stoull(type_val.substr(4)))));
            } else if (type_val.starts_with("f16")) {
                temp_constants.push_back(Value(static_cast<numeric::float16_t>(std::stof(type_val.substr(4)))));
            } else if (type_val.starts_with("f32")) {
                temp_constants.push_back(Value(static_cast<float>(std::stof(type_val.substr(4)))));
            } else if (type_val.starts_with("f64")) {
                temp_constants.push_back(Value(static_cast<double>(std::stod(type_val.substr(4)))));
            } else if (type_val.starts_with("bool")) {
                temp_constants.push_back(Value(trim(type_val.substr(5)) == "true"));
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
            } else if (type_val.starts_with("<closure:")) {
                std::string target = type_val.substr(10, type_val.length() - 11);
                temp_constants.push_back(Value(nullptr)); // Put a placeholder, we link it later.
                deferred_links.push_back({current_closure, temp_constants.size() - 1, target});
            } else {
                temp_constants.push_back(Value(nullptr));
            }
        } else if (in_code && current_closure) {
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

            // SAFE vector access fix: Check array length before access!
            auto get_arg = [&](size_t idx) -> uint16_t {
                if (idx < args.size()) {
                    std::string s = args[idx];
                    if (s.empty()) {
                        return 0;
                    }
                    if (s.starts_with("argc:")) {
                        return static_cast<uint16_t>(std::stoi(trim(s.substr(5))));
                    }
                    if (s.starts_with("%r") || s.starts_with("$K")) {
                        return static_cast<uint16_t>(std::stoi(s.substr(2)));
                    }
                    return static_cast<uint16_t>(std::stoi(s));
                }
                return 0; // Return 0 if the argument was omitted (like for Return or Load_nil)
            };

            auto get_jump = [&](size_t idx) -> uint32_t {
                if (idx < args.size()) {
                    std::string s = args[idx];
                    if (s.empty()) {
                        return 0;
                    }
                    if (s.starts_with(".L")) {
                        return static_cast<uint32_t>(std::stoul(s.substr(2)));
                    }
                    if (s.starts_with("@")) {
                        return static_cast<uint32_t>(std::stoul(s.substr(1)));
                    }
                }
                return 0;
            };

            if (op == Opcode::Load_const) {
                temp_code.push_back(Instruction::make_ri(op, get_arg(0), get_arg(1)));
            } else if (op == Opcode::Jump) {
                temp_code.push_back(Instruction::make_i(op, get_jump(0)));
            } else if (op == Opcode::Jump_if_false) {
                temp_code.push_back(Instruction::make_ri(op, get_arg(0), get_jump(1)));
            } else {
                // Safely grabs up to 3 arguments. Missing args perfectly default to 0.
                temp_code.push_back(Instruction::make_rrr(op, get_arg(0), get_arg(1), get_arg(2)));
            }
        }
    }

    // Map deferred closure pointers
    for (const auto &link : deferred_links) {
        if (parsed_closures.contains(link.target_name)) {
            link.owner->constants[link.const_index] = Value::make_closure(parsed_closures[link.target_name]);
        }
    }

    // Return the 'main' block
    if (main_closure) {
        return *main_closure;
    }

    // Fallback: If no "main" was explicitly found, just return whatever the last parsed block was.
    if (!parsed_closures.empty()) {
        return *parsed_closures.begin()->second;
    }

    return Closure_data{};
}
} // namespace phos::vm
