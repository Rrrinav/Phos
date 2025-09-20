# Precedence


| Precedence Level | Operators              | Associativity | Parser Method   | Description                          |
|------------------|------------------------|---------------|-----------------|--------------------------------------|
| 1 (Lowest)       | =                      | Right         | assignment()    | Assignment                           |
| 2                | \|\|                   | Left          | logical_or()    | Logical OR                           |
| 3                | &&                     | Left          | logical_and()   | Logical AND                          |
| 4                | ==, !=                 | Left          | equality()      | Equality comparison                  |
| 5                | <, <=, >, >=           | Left          | comparison()    | Relational comparison                |
| 6                | +, -                   | Left          | term()          | Addition, Subtraction                |
| 7                | *, /, %                | Left          | factor()        | Multiplication, Division, Modulo     |
| 8                | as                     | Left          | cast()          | Type casting                         |
| 9                | !, - (unary)           | Right         | unary()         | Logical NOT, Unary minus             |
| 10               | (), .                  | Left          | call()          | Function calls, Field/method access  |
| 11 (Highest)     | Literals, Identifiers  | N/A           | primary()       | Primary expressions                  |

---

### Notes
1. Assignment is right-associative: `a = b = c` parses as `a = (b = c)`
2. All binary operators except assignment are left-associative: `a + b + c` parses as `(a + b) + c`
3. Unary operators are right-associative: `!!x` parses as `!(!x)`
4. Type casting has lower precedence than unary operators: `-x as int` parses as `(-x) as int`, not `-(x as int)`
5. Function calls and field access have the same precedence and are processed left-to-right: `obj.field()` parses as `(obj.field)()`
6. Grouping with parentheses has the highest effective precedence and can override any other precedence rules
---

### Expression Examples
Expr:
```
    a = b || c && d == e + f * g as h
```
Parses as:
```
    a = (b || (c && (d == (e + (f * (g as h))))))
```

Expr:
```
    !-x.method()
```
Parses as:
```
    !((-x).method())
```
