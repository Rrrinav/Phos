# Phos

> An interpreted statically typed scripting language with proper type checking

## Example

```js
fn fib(x: i64) -> i64 {
    if (x <= 1) return x;
    else return fib(x - 1) + fib(x - 2);
}

let y: i64;
let x:= 10;

print("hello: " + x as string + " " + y as string);

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
