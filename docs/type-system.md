# The Phos Type System

Phos is a modern, statically-typed language. This means every variable's type must be known at compile time, which helps prevent many common bugs before the program is ever run.

---
## Primitive Types

-   **`i64`**: A 64-bit signed integer for whole numbers (e.g., `-10`, `0`, `42`).
-   **`f64`**: A 64-bit floating-point number for decimals (e.g., `3.14`, `-0.5`).
-   **`bool`**: A boolean value, which can be `true` or `false`.
-   **`string`**: A sequence of characters, written in double quotes (e.g., `"hello"`).
-   **`void`**: Represents the absence of a value, used as the return type for functions that do not return anything.
-   **`any`**: A special type that can hold a value of any other type. It is primarily used for defining generic built-in functions.

---

## Types as Expressions

In Phos, types are not just simple labels; they are first-class expressions. This means you can build complex, nested types from simpler ones using postfix operators, just like you would build up a mathematical expression.

The primary postfix operators for types are:

- []: Creates an array of the preceding type.
- ? : Makes the preceding type optional (nullable).
- (): Groups a type to control the order of operations.

This system allows for the intuitive creation of sophisticated data structures.

Examples of Type Composition
- i64 -> An integer.
- i64[] -> An array of integers.
- i64[]? -> An optional array of integers (the array itself can be nil).
- (i64?)[] -> An array that can contain optional integers (the elements can be nil).
- (i64[]?)[] -> An array that contains optional arrays of integers.

---

## Composite Types

### Models
A model is a custom data structure that groups together named fields. It is similar to a `struct` in other languages.

- **Example**:
```js
  model Point {
      let x: i64;
      let y: i64;
  }
````

### Arrays

An array is an ordered, mutable list of elements of the same type.

  - **Syntax**: `T[]`, where `T` is any other type.
  - **Example**: `let my_numbers: i64[] = [10, 20, 30];`

### Closures (Function Types)

Variables can hold functions. The type of a function is defined by its parameters and return type.

  - **Syntax**: `|T1, T2| -> T_Return`
  - **Example**: `let my_adder: |i64, i64| -> i64 = |a: i64, b: i64| -> i64 { return a + b; };`

-----

### Safety Features: Optionals and `nil`

To prevent runtime errors, Phos requires you to be explicit about values that can be absent.

#### `nil`

`nil` is a special literal that represents the absence of a value. It can only be used with types that are explicitly marked as "optional".

#### Optional Types

An optional type is a wrapper that can hold either a value of its base type or `nil`. This is the only way for a variable to hold `nil`.

  - **Syntax**: `T?`, where `T` is any other type.
  - **Examples**:
    ```js
    // 'maybe_user' can hold a User model or nil.
    let maybe_user: User? = nil; 

    // 'name' cannot hold nil.
    let name: string = "Alex"; 
    // let name: string = nil; // ✅ COMPILE ERROR
    ```

-----

### Type Inference

Phos supports powerful type inference using the `:=` operator. The compiler will deduce the type of a variable from the expression it is initialized with, including complex nested and optional types.

  - `let x := 10;`                               // Inferred as `i64`
  - `let name := "Phos";`                        // Inferred as `string`
  - `let point := Point{ .x = 1, .y = 2 };`      // Inferred as `Point`
  - `let scores := [99, 85, 100];`               // Inferred as `i64[]`
  - `let data := [[1, 2], nil, [3]];`            // Inferred as `(i64[]?)[]`
  - `let user := find_user(1);`                // Inferred as `User?`

An explicit type annotation is only required when a variable is not initialized or when initializing an empty array (`let empty: i64[] = [];`).
