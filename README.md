# eAse 100
This is a small compiler for the toy assembly language ase100 for the E100 processor.
Design of the language is credited to the staff of the Microprocessors and Toys section of the ENGR 100 class at the University of Michigan.
This compiler add these features to the original ase100 tool:

- Compile-time constant expressions
- Constant-length padding
- Multiple literals on the same line
- String literals
- Define directives (akin to C's `#define`)
- Block comments (between `/*` and `*/`)

Planned features:

- Missing file errors
- Function-style macros

## Additional syntax
### Compile-time constant expressions
Expressions of the type `(value + value)` are evaluated by the compiler.
Values may be literals (numbers or chars) or labels (which represent addresses).
Subtraction is also supported, and nested expressions must be surrounded by parentheses as well.
For example:
```
  cpdata label1 (label2 - (label1 + 1))

label1 0 1 2 3
label2
```
will be processed to being equivalent to:
```
  cpdata 4 3
  0 1 2 3
```

### Constant-length padding
Padding can take the place of any literal (but not used in instructions). Padding is indicated by a `$` followed by some literal (not labels). It will be replaced by that number of `0`s.
For example:
```
  $8
```
is equivalent to:
```
  0 0 0 0 0 0 0 0
```

### String literals
String literals, like padding, can take the place of any literal. Surround a series of characters with `"` and it will be interpreted as a series of chars.
For example:
```
  "Hello, world!"
```
is equivalent to:
```
  'H' 'e' 'l' 'l' 'o' ',' ' ' 'w' 'o' 'r' 'l' 'd' '!'
```

### Define directives
Define directives, similarly to `include` directives, are denoted with `#define` followed by an unused label name and a value (literal, label, or expression). They can be used as if they were labels.
For example:
```
#define ORIGIN 0
  cpfa label2 ORIGIN label3

label1 label2
label2 0
label3 1
```
is equivalent to:
```
  cpfa label2 0 label3

label1 label2
label2 0
label3 1
```
