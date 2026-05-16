#pragma once

#include "core/value/value.hpp"
#include "virtual_machine/vm_context.hpp"

#include <span>
#include <stdexcept>
#include <string_view>

namespace phos::ffi {

inline const Value *find_model_field(const Value &value, const types::Model_type &signature, std::string_view field_name)
{
    if (!value.is_model()) {
        return nullptr;
    }

    auto index = signature.field_indices.find(std::string(field_name));
    if (index == signature.field_indices.end() || index->second >= value.as_model()->field_count) {
        return nullptr;
    }

    return &value.as_model()->fields[index->second];
}

inline const Value *find_model_field(const Value &value, std::string_view field_name)
{
    return find_model_field(value, value.as_model()->signature, field_name);
}

inline Value *find_model_field_mut(Value &value, const types::Model_type &signature, std::string_view field_name)
{
    if (!value.is_model()) {
        return nullptr;
    }

    auto index = signature.field_indices.find(std::string(field_name));
    if (index == signature.field_indices.end() || index->second >= value.as_model()->field_count) {
        return nullptr;
    }

    return &value.as_model()->fields[index->second];
}

inline Value *find_model_field_mut(Value &value, std::string_view field_name)
{
    return find_model_field_mut(value, value.as_model()->signature, field_name);
}

inline const Value &require_model_field(const Value &value, const types::Model_type &signature, std::string_view field_name)
{
    if (const Value *field = find_model_field(value, signature, field_name)) {
        return *field;
    }

    throw std::runtime_error(std::string("Missing model field: ") + std::string(field_name));
}

inline const Value &require_model_field(const Value &value, std::string_view field_name)
{
    return require_model_field(value, value.as_model()->signature, field_name);
}

inline Value make_model(vm::Vm_context &ctx, const types::Model_type &signature, std::span<const Value> fields, uint8_t depth = 0)
{
    Value model = Value::make_model(ctx, signature, static_cast<uint32_t>(fields.size()), depth);
    for (size_t i = 0; i < fields.size(); ++i) {
        model.as_model()->fields[i] = fields[i];
    }
    return model;
}

inline Value make_model(vm::Vm_context &ctx, const types::Type_table &tt, types::Type_id model_type, std::span<const Value> fields, uint8_t depth = 0)
{
    if (!tt.is_model(model_type)) {
        throw std::runtime_error("Type id does not refer to a model.");
    }
    return make_model(ctx, tt.get(model_type).as<types::Model_type>(), fields, depth);
}

} // namespace phos::ffi
