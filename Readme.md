# Phos

> An interpreted statically typed scripting language with proper type checking

## Example

```js
fn fib(x: i64) -> i64 {
    if (x <= 1) return x;
    else return fib(x - 1) + fib(x - 2);
}

model Point {
    let x: i64;
    let y: i64;
    fn show() {
        print("{ x: " + this.x as string + ", y: " + this.y as string + " }");
    }
}

let p: Point = { .y = 10, . x = 10 };
p.show();
print(p.x);

fn make_adder(n: i64) -> |i64| -> i64 {
    let cls : |i64| -> i64 = |x: i64| -> i64 {
        return x + n;
    };
    return cls;
}

let add5  :|i64| -> i64 = make_adder(5);
let add10 :|i64| -> i64 = make_adder(10);

print(add5(3));
print(add10(3));

let y: i64;
let x:= 10;

print(fib(6));

while (x >= 1)
{
    print(x);
    x = x - 1;
}

let s := "Some error";
print_err(s);
```

## TODO

- [ ] User defined types
- [ ] Have proper command line args for it
- [ ] Arrays
- [ ] Proper I/O (reading input)
- [ ] Imports
- [ ] Type Inference

## TODO 2

- [ ] Fix calculation of same things twice ( like functions and models in both interpreter and type checker).
- [ ] PERF: Do overall optimizations and remove niavities.
