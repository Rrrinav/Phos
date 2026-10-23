# Phos

> An interpreted statically typed scripting language with proper type checking

I aim at adding a lot of features for learning purposes, they may not be perfect though.

## Example

```rust
model User {
    id: i64;
    name: string;
    is_active: bool;
}

fn find_first(users: User?[], predicate: ((fn(User) -> bool)?)) -> User? {

    let mut actual_p := fn(u: User) -> bool { return u.is_active; };

    if predicate != nil {
        actual_p = predicate as fn(User) -> bool;
    }

    for i in 0..(len(users) as i32) {
        if users[i] != nil {
            let user := users[i].get();
            if actual_p(user) {
                return user;
            }
        }
    }

    return nil;
}

fn foo() -> (fn() -> void) {
    let x := "x from function.";
    return fn() -> void { print("Hello from function, this is:", x); };
}

let user_list: User?[] = [
    .{ id: 1, name: "Alex", is_active: false },
    .{ id: 2, name: "Blake", is_active: true },
    nil,
    .{ id: 3, name: "Casey", is_active: true }
];

let first_active_user := find_first(user_list, nil);

if first_active_user != nil {
    let actual_user := first_active_user as User;
    print("Found active user: ", actual_user.name, end="!\n");
}

foo()();
```

## Piping

`|>` feeds the value on its left into the call on its right as the first
argument. Chains read top-to-bottom instead of inside-out:

```rust
fn double(x: i32) -> i32 { return x * 2; }
fn add(a: i32, b: i32) -> i32 { return a + b; }
fn clamp(x: i32, max: i32) -> i32 {
    if x > max { return max; }
    return x;
}

// f(g(h(x)))  ->  x |> h |> g |> f
let result := 5 |> double |> add(10);   // add(double(5), 10) == 20

// named arguments still work
let capped := 20 |> clamp(max = 10);    // clamp(20, max = 10)
```

Bare functions, closures and method calls are all valid targets, and the
pipeline is ordinary call syntax after parsing, so it costs nothing at runtime.

## Build

```sh
git clone https://github.com/rrrinav/phos
g++ ./bld.cpp -o ./bld
./bld -rel

./bin/phos ./examples/01_game_of_life.phos
```

## TODO

- [x] Add a saturation cast (limits to size, not truncate bits) — `expr sat i8`
- [ ] Concurrency (green threads)
- [ ] Proper I/O (reading input, file streams)
- [ ] Optimisations

- Concrete string input streams exist now; generic result/stream abstractions still need generics

## TODO 2

- [ ] Add inline code documentation
