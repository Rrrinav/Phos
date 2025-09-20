# Phos

> An interpreted statically typed scripting language with proper type checking

## Example

```js
model User {
  let id: i64;
  let name: string;
  let is_active: bool;
}

fn find_first(users: (User?)[], predicate: (| User | -> bool) ?) -> User ? {
  let const default_predicate: | User | -> bool = | u: User | -> bool { return u.is_active; };
  let p:= predicate.value_or(default_predicate);

  for (let i:= 0; i < len(users); i = i + 1) {
    let maybe_user:= users[i];
    // Type is narrowed to 'User' inside this block.
    if (maybe_user != nil) {
      let user:= maybe_user as User;
      if (p(user)) {
        return user; // Return the first user that matches
      }
    }
  }
  return nil; // Return nil if no user matches
}

let const user_list: (User?)[] = [
  User{ .id = 1, .name = "Alex", .is_active = false },
  nil, // A nil value in the list
  User{ .id = 2, .name = "Blake", .is_active = true },
  User{ .id = 3, .name = "Casey", .is_active = true }
];

let first_active_user:= find_first(user_list, nil);

if (first_active_user.exists()) {
  print("Found active user: " + first_active_user.value().name);
}
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
