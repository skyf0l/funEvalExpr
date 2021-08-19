#!/bin/bash

OK="\x1b[32;01m[✔]\x1b[0m"
KO="\x1b[31;01m[X]\x1b[0m"

BOLD="\033[1m"
UNBOLD="\033[0m"

if [ "$#" = 0 ]; then
    echo "Invalid usage: $0 ./binary"
    exit 1
fi
binary="$@"

function test_evalexpr() {
    result=$($binary "$1")
    exit_value=$?
    if [ $exit_value = 0 ]; then
        if [ "$result" = "$2" ]; then
            echo -e "$OK '$1' = '$2'"
        else
            echo -e "$KO '$1'"
            echo -e "\tgot '$result'"
            echo -e "\texpecetd '$2'"
        fi
    else
        echo -e "$KO '$1'"
        echo -e "\texit with '$exit_value'"
        echo -e "\texpected '0'"
    fi
}

function test_evalexpr_error_handling() {
    result=$($binary "$1")
    exit_value=$?
    if [ $exit_value = 84 ]; then
        echo -e "$OK '$1'"
    else
        echo -e "$KO '$1'"
        echo -e "\texit with '$exit_value'"
        echo -e "\texpected '84'"
    fi
}

# Error handling
echo -e "$BOLD""Test error handling""$UNBOLD"
test_evalexpr_error_handling ""
test_evalexpr_error_handling "+"
test_evalexpr_error_handling "2+"
test_evalexpr_error_handling "2**2"
test_evalexpr_error_handling "(2"
test_evalexpr_error_handling "2)"

# result
echo -e "\n$BOLD""Test result""$UNBOLD"

# Somes tests
test_evalexpr "3+5.34" 8.34
test_evalexpr "(0.345 + 5) * (- 2 -1) / 3" -5.35
test_evalexpr "(3+2)*5" 25.00

## Unary operators
echo -e "\n$BOLD""Test unary operators""$UNBOLD"

# Unary positive
echo "Test unary positive"
test_evalexpr "+0" 0.00
test_evalexpr "+1" 1.00
test_evalexpr "++++1" 1.00

# Unary negative
echo "Test unary negative"
test_evalexpr "-0" 0.00
test_evalexpr "-1" -1.00
test_evalexpr "--1" 1.00
test_evalexpr "---1" -1.00

test_evalexpr "1--1" 2.00
test_evalexpr "-1--1" 0.00
test_evalexpr "1---1" 0.00
test_evalexpr "-1--1--1" 1.00

# Logical not
echo "Test logical not"
test_evalexpr "!0" 1.00
test_evalexpr "!1" 0.00

# Unary operators priority
echo "Test unary operators priority"
test_evalexpr "-1^2" -1.00
test_evalexpr "(-1)^2" 1.00

# Unary operators series
echo "Test unary operators series"
test_evalexpr "--+-+--+-++---+-+-++--+-+-+--1" -1.00
test_evalexpr "--!+-!+-!-!+-+!!+--!-!!!+-+-!!+!!+--+-!+!-!+!--1" 1.00

## Tests from https://github.com/ISibboI/evalexpr/blob/main/tests/integration.rs
echo -e "\n$BOLD""Tests from https://github.com/ISibboI/evalexpr/blob/main/tests/integration.rs""$UNBOLD"

# Unary examples
echo "Unary examples"
test_evalexpr "3" 3.00
test_evalexpr "3.3" 3.30
test_evalexpr "-3" -3.00
test_evalexpr "-3.6" -3.60
test_evalexpr "----3" 3.00
test_evalexpr "1e0" 1.00
test_evalexpr "1e-0" 1.00
test_evalexpr "10e3" 10000.00
test_evalexpr "10e+3" 10000.00
test_evalexpr "10e-3" 0.01

# Binary examples
echo "Binary examples"
test_evalexpr "1+3" 4.00
test_evalexpr "3+1" 4.00
test_evalexpr "3-5" -2.00
test_evalexpr "5-3" 2.00
test_evalexpr "5 / 4" 1.25
test_evalexpr "5 *3" 15.00
test_evalexpr "1.0+3" 4.00
test_evalexpr "3.0+1" 4.00
test_evalexpr "3-5.0" -2.00
test_evalexpr "5-3.0" 2.00
test_evalexpr "5 / 4.0" 1.25
test_evalexpr "5.0 *3" 15.00
test_evalexpr "5.0 *-3" -15.00
test_evalexpr "5.0 *- 3" -15.00
test_evalexpr "5.0 * -3" -15.00
test_evalexpr "5.0 * - 3" -15.00
test_evalexpr "-5.0 *-3" 15.00
test_evalexpr "3+-1" 2.00
test_evalexpr "-3-5" -8.00
test_evalexpr "-5--3" -2.00
test_evalexpr "5e2--3" 503.00
test_evalexpr "-5e-2--3" 2.95

# Arithmetic precedence examples
echo "Arithmetic precedence examples"
test_evalexpr "1+3-2" 2.00
test_evalexpr "3+1*5" 8.00
test_evalexpr "2*3-5" 1.00
test_evalexpr "5-3/3" 4.00
test_evalexpr "5 / 4*2" 2.50
test_evalexpr "1-5 *3/15" 0.00
test_evalexpr "15/7/2.0" 1.07
test_evalexpr "15.0/7/2" 1.07
test_evalexpr "15.0/-7/2" -1.07
test_evalexpr "-15.0/7/2" -1.07
test_evalexpr "-15.0/7/-2" 1.07

# Braced examples
echo "Braced examples"
test_evalexpr "(1)" 1.00
test_evalexpr "( 1.0 )" 1.00
test_evalexpr "( -1 )" -1.00
test_evalexpr "-(1)" -1.00
test_evalexpr "-(1 + 3) * 7" -28.00
test_evalexpr "(1 * 1) - 3" -2.00
test_evalexpr "4 / (2 * 2)" 1.00
test_evalexpr "7/(7/(7/(7/(7/(7)))))" 1.00

# Mod examples
echo "Mod examples"
test_evalexpr "1 % 4" 1.00
test_evalexpr "6 % 4" 2.00
test_evalexpr "1 % 4 + 2" 3.00

# Pow examples
echo "Pow examples"
test_evalexpr "1 ^ 4" 1.00
test_evalexpr "6 ^ 4" 1296.00
test_evalexpr "1 ^ 4 + 2" 3.00
test_evalexpr "2 ^ (4 + 2)" 64.00

# Boolean examples
echo "Boolean examples"
test_evalexpr "1 && 0" 0.00
test_evalexpr "1 && 1 || 1 && 0" 1.00
test_evalexpr "5 > 4" 1.00
test_evalexpr "1 <= 1" 1.00
test_evalexpr "5 > 4 && 1 <= 1" 1.00
test_evalexpr "5.0 <= 4.9 || !(4 > 3.5)" 0.00