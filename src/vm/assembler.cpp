#include "assembler.hpp"

#include "chunk.hpp"

#include <algorithm>
#include <cctype>
#include <format>
#include <iostream>
#include <map>
#include <sstream>

namespace phos::vm {

std::unordered_map<std::string, Op_code> Assembler::get_opcode_map()
{
    std::unordered_map<std::string, Op_code> map;
    for (int i = 0; i <= static_cast<int>(Op_code::Halt); ++i) {
        Op_code op = static_cast<Op_code>(i);
        std::string mnemonic = op_code_to_string(op);
        std::transform(mnemonic.begin(), mnemonic.end(), mnemonic.begin(), ::toupper);
        map[mnemonic] = op;
    }
    return map;
}

std::unordered_map<Op_code, std::string> Assembler::get_mnemonic_map()
{
    std::unordered_map<Op_code, std::string> map;
    for (int i = 0; i <= static_cast<int>(Op_code::Halt); ++i) {
        Op_code op = static_cast<Op_code>(i);
        std::string mnemonic = op_code_to_string(op);
        std::transform(mnemonic.begin(), mnemonic.end(), mnemonic.begin(), ::toupper);
        map[op] = mnemonic;
    }
    return map;
}

// =========================================================================
// SERIALIZER (Bytecode -> PhosAsm)
// =========================================================================
std::string Assembler::serialize(mem::rc_ptr<Closure_value> main_closure)
{
    std::stringstream ss;
    auto mnemonics = get_mnemonic_map();
    std::unordered_map<Closure_value *, int> closure_ids;
    std::vector<mem::rc_ptr<Closure_value>> closures;

    closures.push_back(main_closure);
    closure_ids[main_closure.get()] = 0;

    for (size_t i = 0; i < closures.size(); ++i) {
        auto closure = closures[i];
        int id = closure_ids[closure.get()];
        std::string name = closure->name.empty() ? "anon" : closure->name;

        ss << "; ==========================================================\n";
        ss << "; Function: " << name << "\n";
        ss << "; ID: " << id << " | Arity: " << closure->arity << " | Upvalues: " << closure->upvalue_count << "\n";
        ss << "; ==========================================================\n";
        ss << ".fn " << id << " " << name << " " << closure->arity << " " << closure->upvalue_count << "\n\n";

        auto chunk = closure->chunk;

        ss << "  .constants:\n";
        for (size_t j = 0; j < chunk->constants.size(); ++j) {
            const auto &val = chunk->constants[j];
            ss << "      @" << j << " = ";
            if (is_signed_integer(val))
                ss << "int " << get_int(val) << "\n";
            else if (is_unsigned_integer(val))
                ss << "uint " << get_uint(val) << "\n";
            else if (is_float(val))
                ss << "float " << get_float(val) << "\n";
            else if (is_bool(val))
                ss << "bool " << (get_bool(val) ? "true" : "false") << "\n";
            else if (is_string(val))
                ss << "string \"" << get_string(val) << "\"\n";
            else if (is_closure(val)) {
                auto child = get_closure(val);
                if (!closure_ids.count(child.get())) {
                    closure_ids[child.get()] = closures.size();
                    closures.push_back(child);
                }
                ss << "fn " << closure_ids[child.get()] << "\n";
            }
        }

        // PRE-PASS: Find all jump targets and assign labels
        std::map<size_t, std::string> labels;
        int label_counter = 0;
        for (size_t offset = 0; offset < chunk->code.size();) {
            Op_code op = static_cast<Op_code>(chunk->code[offset]);
            if (op == Op_code::Jump || op == Op_code::Jump_if_false || op == Op_code::Jump_if_true || op == Op_code::Loop) {
                uint16_t jump = static_cast<uint16_t>(chunk->code[offset + 1] << 8) | chunk->code[offset + 2];
                size_t target = (op == Op_code::Loop) ? (offset + 3 - jump) : (offset + 3 + jump);
                if (!labels.count(target))
                    labels[target] = ".L" + std::to_string(label_counter++);
                offset += 3;
            } else if (
                op == Op_code::Constant || op == Op_code::Define_global || op == Op_code::Get_global || op == Op_code::Set_global
                || op == Op_code::Match_variant || op == Op_code::Get_local || op == Op_code::Set_local || op == Op_code::Get_upvalue
                || op == Op_code::Set_upvalue || op == Op_code::Call || op == Op_code::Create_array || op == Op_code::Get_field
                || op == Op_code::Set_field || op == Op_code::Cast) {
                offset += 2;
            } else if (op == Op_code::Construct_union || op == Op_code::Construct_model) {
                offset += 3;
            } else if (op == Op_code::Make_closure) {
                uint8_t const_idx = chunk->code[offset + 1];
                auto child = get_closure(chunk->constants[const_idx]);
                offset += 2 + (child->upvalue_count * 2);
            } else {
                offset += 1;
            }
        }

        // MAIN PASS: Generate Code
        ss << "\n  .code:\n";
        for (size_t offset = 0; offset < chunk->code.size();) {
            if (labels.count(offset))
                ss << "    " << labels[offset] << ":\n";

            Op_code op = static_cast<Op_code>(chunk->code[offset]);
            std::string mnemonic = mnemonics[op];
            std::string args = "";
            std::string comment = "";

            switch (op) {
            case Op_code::Constant:
            case Op_code::Define_global:
            case Op_code::Get_global:
            case Op_code::Set_global:
            case Op_code::Match_variant: {
                uint8_t idx = chunk->code[offset + 1];
                args = "@" + std::to_string(idx);
                comment = value_to_string(chunk->constants[idx]);
                offset += 2;
                break;
            }
            case Op_code::Get_local:
            case Op_code::Set_local:
            case Op_code::Get_field:
            case Op_code::Set_field: {
                args = "%" + std::to_string(chunk->code[offset + 1]);
                offset += 2;
                break;
            }
            case Op_code::Get_upvalue:
            case Op_code::Set_upvalue: {
                args = "^" + std::to_string(chunk->code[offset + 1]);
                offset += 2;
                break;
            }
            case Op_code::Call:
            case Op_code::Create_array: {
                args = "#" + std::to_string(chunk->code[offset + 1]);
                offset += 2;
                break;
            }
            case Op_code::Jump:
            case Op_code::Jump_if_false:
            case Op_code::Jump_if_true:
            case Op_code::Loop: {
                uint16_t jump = static_cast<uint16_t>(chunk->code[offset + 1] << 8) | chunk->code[offset + 2];
                size_t target = (op == Op_code::Loop) ? (offset + 3 - jump) : (offset + 3 + jump);
                args = labels[target];
                offset += 3;
                break;
            }
            case Op_code::Construct_model:
            case Op_code::Construct_union: {
                uint8_t arg1 = chunk->code[offset + 1];
                uint8_t arg2 = chunk->code[offset + 2];
                args = "@" + std::to_string(arg1) + " @" + std::to_string(arg2);
                comment = value_to_string(chunk->constants[arg1]) + "::" + value_to_string(chunk->constants[arg2]);
                offset += 3;
                break;
            }
            case Op_code::Make_closure: {
                uint8_t const_idx = chunk->code[offset + 1];
                args = "@" + std::to_string(const_idx);
                auto child_closure = get_closure(chunk->constants[const_idx]);
                comment = child_closure->name;
                offset += 2;
                ss << std::format("      {:<20} {:<15} ; {}\n", mnemonic, args, comment);
                for (size_t j = 0; j < child_closure->upvalue_count; j++) {
                    uint8_t is_local = chunk->code[offset];
                    uint8_t idx = chunk->code[offset + 1];
                    std::string route_arg = (is_local ? "%" : "^") + std::to_string(idx);
                    ss << std::format("        {:<18} {:<15} ; {}\n", ".route", route_arg, is_local ? "capture local" : "pass upvalue");
                    offset += 2;
                }
                continue;
            }
            default:
                offset += 1;
                break;
            }

            // Zero-Cost Formatting (No trailing whitespaces on 1-byte instructions)
            if (comment.empty() && args.empty())
                ss << "      " << mnemonic << "\n";
            else if (comment.empty())
                ss << std::format("      {:<20} {}\n", mnemonic, args);
            else if (args.empty())
                ss << std::format("      {:<36} ; {}\n", mnemonic, comment);
            else
                ss << std::format("      {:<20} {:<15} ; {}\n", mnemonic, args, comment);
        }

        if (labels.count(chunk->code.size()))
            ss << "    " << labels[chunk->code.size()] << ":\n";
        ss << ".endfn\n\n";
    }
    return ss.str();
}

// =========================================================================
// ASSEMBLER (PhosAsm -> Bytecode)
// =========================================================================
mem::rc_ptr<Closure_value> Assembler::assemble(const std::string &ir_source)
{
    std::unordered_map<int, mem::rc_ptr<Closure_value>> closures;
    std::stringstream ss(ir_source);
    std::string line;
    auto op_map = get_opcode_map();

    mem::rc_ptr<Closure_value> current_closure = nullptr;
    bool in_code = false;

    // Backpatching state
    struct UnresolvedJump
    {
        size_t patch_offset;
        size_t instruction_offset;
        std::string label;
        bool is_loop;
    };
    std::unordered_map<std::string, size_t> current_labels;
    std::vector<UnresolvedJump> current_jumps;

    // Pass 1: Allocate closures
    while (std::getline(ss, line)) {
        std::stringstream ls(line);
        std::string tok;
        ls >> tok;
        if (tok == ".fn") {
            int id, arity, upvals;
            std::string name;
            ls >> id >> name >> arity >> upvals;
            types::Function_type dummy_sig;
            auto chunk = mem::make_rc<Chunk>();
            auto closure = mem::make_rc<Closure_value>(name, arity, dummy_sig, chunk);
            closure->upvalue_count = upvals;
            closures[id] = closure;
        }
    }

    // Pass 2: Populate chunks
    ss.clear();
    ss.seekg(0);
    while (std::getline(ss, line)) {
        line.erase(0, line.find_first_not_of(" \t"));
        if (line.empty() || line[0] == ';')
            continue;

        std::stringstream ls(line);
        std::string tok;
        ls >> tok;

        if (tok == ".fn") {
            int id;
            ls >> id;
            current_closure = closures[id];
            in_code = false;
        } else if (tok == ".constants:")
            continue;
        else if (tok == ".code:")
            in_code = true;
        else if (tok == ".endfn") {
            // Resolve all Jumps for this closure perfectly!
            for (const auto &jump : current_jumps) {
                if (!current_labels.count(jump.label)) {
                    std::cerr << "Assembler Error: Undefined label '" << jump.label << "'\n";
                    exit(1);
                }
                size_t target = current_labels[jump.label];
                size_t distance;
                if (jump.is_loop)
                    distance = (jump.instruction_offset + 3) - target;
                else
                    distance = target - (jump.instruction_offset + 3);

                current_closure->chunk->code[jump.patch_offset] = (distance >> 8) & 0xff;
                current_closure->chunk->code[jump.patch_offset + 1] = distance & 0xff;
            }
            current_labels.clear();
            current_jumps.clear();
            current_closure = nullptr;
        } else if (tok.starts_with("@") && tok.find('=') == std::string::npos) {
            std::string eq, type, val;
            ls >> eq >> type;
            std::getline(ls, val);
            val.erase(0, val.find_first_not_of(" \t"));

            if (type == "int")
                current_closure->chunk->add_constant(Value(static_cast<std::int64_t>(std::stoll(val))));
            else if (type == "uint")
                current_closure->chunk->add_constant(Value(static_cast<std::uint64_t>(std::stoull(val))));
            else if (type == "float")
                current_closure->chunk->add_constant(Value(std::stod(val)));
            else if (type == "bool")
                current_closure->chunk->add_constant(Value(val == "true"));
            else if (type == "fn")
                current_closure->chunk->add_constant(Value(closures[std::stoi(val)]));
            else if (type == "string") {
                val = val.substr(1, val.length() - 2);
                current_closure->chunk->add_constant(Value(val));
            }
        } else if (in_code) {
            if (tok.ends_with(":")) {
                std::string label_name = tok.substr(0, tok.length() - 1);
                current_labels[label_name] = current_closure->chunk->code.size();
                continue;
            }

            if (tok == ".route") {
                std::string arg;
                ls >> arg;
                bool is_local = arg.starts_with("%");
                uint8_t idx = static_cast<uint8_t>(std::stoi(arg.substr(1)));
                current_closure->chunk->write(static_cast<uint8_t>(is_local), {0, 0}); // 0 cost location tracking
                current_closure->chunk->write(idx, {0, 0});
                continue;
            }

            if (!op_map.count(tok))
                continue;

            Op_code op = op_map[tok];
            current_closure->chunk->write(op, {0, 0});

            std::string arg1, arg2;
            if (op == Op_code::Constant || op == Op_code::Define_global || op == Op_code::Get_global || op == Op_code::Match_variant
                || op == Op_code::Get_local || op == Op_code::Set_local || op == Op_code::Get_upvalue || op == Op_code::Set_upvalue
                || op == Op_code::Call || op == Op_code::Make_closure || op == Op_code::Get_field || op == Op_code::Set_field
                || op == Op_code::Create_array || op == Op_code::Cast) {
                ls >> arg1;
                current_closure->chunk->write(static_cast<uint8_t>(std::stoi(arg1.substr(1))), {0, 0});
            } else if (op == Op_code::Construct_union || op == Op_code::Construct_model) {
                ls >> arg1 >> arg2;
                current_closure->chunk->write(static_cast<uint8_t>(std::stoi(arg1.substr(1))), {0, 0});
                current_closure->chunk->write(static_cast<uint8_t>(std::stoi(arg2.substr(1))), {0, 0});
            } else if (op == Op_code::Jump || op == Op_code::Jump_if_false || op == Op_code::Loop || op == Op_code::Jump_if_true) {
                ls >> arg1;
                size_t inst_offset = current_closure->chunk->code.size() - 1;
                size_t patch_offset = current_closure->chunk->code.size();
                current_closure->chunk->write(0, {0, 0});
                current_closure->chunk->write(0, {0, 0});
                current_jumps.push_back({patch_offset, inst_offset, arg1, op == Op_code::Loop});
            }
        }
    }

    return closures[0];
}

} // namespace phos::vm
