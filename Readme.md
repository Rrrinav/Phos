# Phos

> An interpreted statically typed scripting language with proper type checking

## Example

```rust
model User {
    id: i64;
    name: string;
    is_active: bool;
};

fn find_first(users: User?[], predicate: (|User| -> bool)?) -> User? {

    let mut actual_p : |User| -> bool = |u: User| -> bool { return u.is_active; };

    if predicate != nil {
        actual_p = predicate as (|User| -> bool);
    }

    let mut i := 0;
    while i < len(users) {
        let maybe_user := users[i];
        if maybe_user != nil {
            let user := maybe_user as User;
            if actual_p(user) {
                return user;
            }
        }
        i = i + 1;
    }

    return nil;
}

fn foo() -> (|| -> void) {
    let x := "x from function.";
    return || -> void { print("Hello from function, this is: " + x); };
}

let user_list : User?[] = [
    User { id: 1, name: "Alex", is_active: false },
    nil,
    User { id: 2, name: "Blake", is_active: true },
    User { id: 3, name: "Casey", is_active: true }
];

let first_active_user := find_first(user_list, nil);

if first_active_user != nil {
    let actual_user := first_active_user as User;
    print("Found active user: " + actual_user.name);
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

- [ ] Proper I/O (reading input, file streams)
- [ ] Have proper command line args for it
- [ ] Imports
- [ ] C/C++ FFI

## TODO 2

- [ ] Fix calculation of same things twice ( like functions and models in both interpreter and type checker).
- [ ] PERF: Do overall optimizations and remove niavities.
