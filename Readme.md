# Phos

> An interpreted statically typed scripting language with proper type checking

GC not implemented yet.

I aim at adding a lot of features for learning purposes, they may not be perfect though.

## Example

```rust
model User {
    id: i64;
    name: string;
    is_active: bool;
}

fn find_first(users: User?[], predicate: (|User| -> bool)?) -> User? {

    let mut actual_p: |User| -> bool = |u: User| -> bool { return u.is_active; };

    if predicate != nil {
        actual_p = predicate as (|User| -> bool);
    }

    for i in 0..len(users) {
        if users[i] != nil {
            let user := users[i].get();
            if actual_p(user) {
                return user;
            }
        }
    }

    return nil;
}

fn foo() -> (|| -> void) {
    let x := "x from function.";
    // Replaced string concatenation with variadic arguments
    return || -> void { print("Hello from function, this is:", x); };
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

## Build

```sh
git clone https://github.com/rrrinav/phos
g++ ./bld.cpp -o ./bld
./bld -rel

./bin/phos ./examples/01_game_of_life.phos
```

## TODO

- [ ] Add a saturation cast (limits to size, not truncate bits)
- [ ] Concurrency (green threads)
- [ ] Proper I/O (reading input, file streams)
- [ ] Have proper command line args for it
- [ ] Imports
- [ ] FFI (In langugae FFI)

## TODO 2

- [ ] Add inline code documentation
