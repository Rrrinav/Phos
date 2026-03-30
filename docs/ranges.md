# Ranges And Iteration

Phos now treats iteration through a common iterator gateway:

- `iter(x)` converts a value into an iterator
- `for x in expr { ... }` implicitly uses that iterator protocol
- numeric intervals like `a..b` and `a..=b` produce lazy integer iterators

## What Is Iterable

In the current implementation, these values are iterable:

- integer intervals
- arrays
- strings
- optionals
- `nil`
- existing iterators
- any other single value as a singleton iterator

## Semantics

### Numeric intervals

- `a..b` is start-inclusive, end-exclusive
- `a..=b` is start-inclusive, end-inclusive
- descending intervals are supported

Examples:

```phos
for x in 0..5 { print(x); }   // 0 1 2 3 4
for x in 5..=2 { print(x); }  // 5 4 3 2
```

### Strings

Strings iterate as `string` values.

Each yielded item is a one-code-point string, not a byte and not a grapheme cluster. This keeps iteration Unicode-aware without introducing a separate `char` type.

### Optionals and nil

- `nil` iterates as empty
- `T?` iterates as empty when `nil`
- `T?` iterates as exactly one item when it contains a value

### Arrays

Arrays iterate by reference.

This is a live iterator:

- element mutations are visible during iteration
- structural mutations like `push`, `insert`, and `remove` are allowed
- later loop steps observe the current array state

If you want a detached snapshot, use `clone(...)` first.

## Iterator Methods

The iterator protocol currently supports:

- `next()`
- `prev()`
- `advance(i64)`
- `back(i64)`
- `retreat(i64)`
- `has_next()`
- `has_prev()`

Examples:

```phos
let it := iter(0..=3);
while it.has_next() {
    print(it.next());
}
```

## Clone

Phos now provides:

```phos
clone(value)
```

`clone(...)` performs a deep copy for arrays, models, unions, and iterators, while primitive values and strings are copied by value naturally.
