# Phos

> An interpreted statically typed scripting language with proper type checking

## Example

```js
model User {
    let name: string;
    let score: i64;
    let is_active: bool;
}

fn filter_users(users: User[], condition: |User| -> bool) -> User[] {
    let result: User[] = [];
    let i := 0;
    while (i < len(users)) {
        if (condition(users[i])) {
            result.push(users[i]);
        }
        i = i + 1;
    }
    return result;
}

let users: User[] = [
    User{ .name = "Alex", .score = 88, .is_active = true },
    User{ .name = "Blake", .score = 95, .is_active = false },
    User{ .name = "Casey", .score = 100, .is_active = true }
];

let high_scoring_and_active: |User| -> bool = |u: User| -> bool {
    return u.is_active && u.score > 90;
};

let top_users := filter_users(users, high_scoring_and_active);

print("--- Top Users Report ---");
print(top_users);
```

## TODO

- [ ] Implement const and 'nil'
- [ ] Proper I/O (reading input, file streams)
- [ ] Have proper command line args for it
- [ ] Imports
- [ ] Type Inference
- [ ] C/C++ FFI

## TODO 2

- [ ] Fix calculation of same things twice ( like functions and models in both interpreter and type checker).
- [ ] PERF: Do overall optimizations and remove niavities.
