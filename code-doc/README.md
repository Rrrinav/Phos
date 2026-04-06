# Phos Code Docs

This folder is a code-first tour of the language.

Each `.phos` file is written as commented examples instead of prose-heavy docs. Most files are meant to be runnable as-is. The last file calls out syntax that exists in the language surface but is still experimental.

Suggested reading order:

1. `01_basics.phos`
2. `02_numbers_and_operators.phos`
3. `03_control_flow.phos`
4. `04_functions_and_closures.phos`
5. `05_arrays_strings_and_optionals.phos`
6. `06_models_and_structural_types.phos`
7. `07_unions_enums_and_match.phos`
8. `08_ranges_iterators_and_core_helpers.phos`
9. `09_fstrings_and_experimental_surface.phos`

Coverage in this folder:

- Variables, mutability, constants, and type annotations
- Real numeric types: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f16`, `f32`, `f64`
- Operators, casts, arrays, strings, and optionals
- `if`, `while`, C-style `for`, `for in`, and `match`
- Functions, default arguments, named arguments, and closures
- Models, `bind`, methods, static methods, `this`, and anonymous structural model types
- Unions, enums, implicit enum members like `.Variant`, and contextual anonymous literals like `.{ ... }`
- Ranges, iterators, and core helpers such as `len`, `iter`, `clone`, `is_same`, `clock`, `sqrt`, `pow`, and `abs`
- `print`, `print_err`, string interpolation, and the current experimental concurrency surface
