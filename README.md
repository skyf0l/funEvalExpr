# Epitech project - funEvalExpr

An expression evaluator written in [Haskell](https://www.haskell.org/).

## Goal

Implement a program which is able to parse a string given as argument, evaluate it and display the result.

## Must

- Handle float numbers ans parentheses.
- Handle addition, subtraction, multiplication, division and power operators (+ - \* / ^).
- The output must always have two digits after the decimal points (examples: 1.00, 3.14).
- Implement a recursive descent parser with combinators, following a Parsing Expression Grammar (PEG).

# Example

Build with make and stack:

```
$ make
or
$ stack build --copy-bins
```

And run with:

```bash
$ ./funEvalExpr "3 + 5.34"
8.34
$ ./funEvalExpr "(0.345+5)*(-2-1)/3"
-5.35
```

# Features

## Operators

### Binary Operators

| Operator | Aka | Precedence | Description               |
| -------- | --- | ---------- | ------------------------- |
| e        | Exp | 120        | Scientific exponentiation |
| ^        | Pow | 120        | Exponentiation            |
| \*       | Mul | 100        | Multiplication            |
| /        | Div | 100        | Division                  |
| %        | Mod | 100        | Modulo                    |
| +        | Add | 95         | Addition                  |
| -        | Sub | 95         | Subtraction               |
| >        | Gt  | 80         | Greater than              |
| <        | Lt  | 80         | Less than                 |
| >=       | Ge  | 80         | Greater than or equal to  |
| <=       | Le  | 80         | Less than or equal to     |
| ==       | Eq  | 80         | Equal to                  |
| !=       | Ne  | 80         | Not equal to              |
| &&       | And | 75         | Logical AND               |
| \|\|     | Or  | 70         | Logical OR                |

### Unary operators

| Operator | Aka | Description      |
| -------- | --- | ---------------- |
| +        | Pos | Identity         |
| -        | Neg | Negation         |
| !        | Neg | Logical negation |

## Debug

With `-d` option, the program will print the AST of the expression.

```
$ ./funEvalExpr -d "(0.345+5)*(-2-1)/3"
Just (
    Operator (
        BinaryOperator (
            Div (
                Operator (
                    BinaryOperator (
                        Mul (
                            Operator (
                                BinaryOperator (
                                    Add (
                                        Operand 0.345
                                    )
                                    (
                                        Operand 5.0
                                    )
                                )
                            )
                        )
                        (
                            Operator (
                                BinaryOperator (
                                    Sub (
                                        Operator (
                                            UnaryOperator (
                                                Neg (
                                                    Operand 2.0
                                                )
                                            )
                                        )
                                    )
                                    (
                                        Operand 1.0
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (
                Operand 3.0
            )
        )
    )
)
```
