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

count_test_passed=0
count_test_failed=0

function test_evalexpr() {
    result=$($binary "$1")
    exit_value=$?
    if [ $exit_value = 0 ]; then
        if [ "$result" = "$2" ]; then
            echo -e "$OK '$1' = '$2'"
            count_test_passed=$((count_test_passed + 1))
        else
            echo -e "$KO '$1'"
            echo -e "\tgot '$result'"
            echo -e "\texpecetd '$2'"
            count_test_failed=$((count_test_failed + 1))
        fi
    else
        echo -e "$KO '$1'"
        echo -e "\texit with '$exit_value'"
        echo -e "\texpected '0'"
        count_test_failed=$((count_test_failed + 1))
    fi
}

function test_evalexpr_error_handling() {
    result=$($binary "$1")
    exit_value=$?
    if [ $exit_value = 84 ]; then
        echo -e "$OK '$1'"
        count_test_passed=$((count_test_passed + 1))
    else
        echo -e "$KO '$1'"
        echo -e "\texit with '$exit_value'"
        echo -e "\texpected '84'"
        count_test_failed=$((count_test_failed + 1))
    fi
}

# Error handling
echo -e "$BOLD""Test error handling""$UNBOLD"
test_evalexpr_error_handling ""
test_evalexpr_error_handling "+"
test_evalexpr_error_handling "2**2"
test_evalexpr_error_handling "(2"
test_evalexpr_error_handling "2)"
test_evalexpr_error_handling "2+"
test_evalexpr_error_handling "2*"
test_evalexpr_error_handling "2+*"
test_evalexpr_error_handling "2+*)"
test_evalexpr_error_handling "2*(2"
test_evalexpr_error_handling "2*(2+*)"
test_evalexpr_error_handling "+*/dqvsd5v9+/*ergaze-/f7-zef+sdv59qc+sd4vq"
test_evalexpr_error_handling ""
test_evalexpr_error_handling "dflkdlfkd"
test_evalexpr_error_handling "38.dsds"
test_evalexpr_error_handling "("
test_evalexpr_error_handling ")"
test_evalexpr_error_handling "42)"
test_evalexpr_error_handling "42("
test_evalexpr_error_handling "42t"
test_evalexpr_error_handling "42+"
test_evalexpr_error_handling "42 )"
test_evalexpr_error_handling "42 ("
test_evalexpr_error_handling "42 t"
test_evalexpr_error_handling "42 +"
test_evalexpr_error_handling "()"
test_evalexpr_error_handling ")("
test_evalexpr_error_handling ")78("
test_evalexpr_error_handling "1+1+"

# result
echo -e "\n$BOLD""Test evalexpr""$UNBOLD"

# Somes tests
echo "Test unary positive"
test_evalexpr "3+5.34" 8.34
test_evalexpr "(0.345 + 5) * (- 2 -1) / 3" -5.34
test_evalexpr "(3+2)*5" 25.00
test_evalexpr "3.+5." 8.00
test_evalexpr "    (    3 +  2  )    *   5     " 25.00

# Number format
echo -e "\n$BOLD""Test number format""$UNBOLD"
test_evalexpr "38." 38.00
test_evalexpr ".22" 0.22
test_evalexpr "." 0.00
test_evalexpr "42 .42" 42.42
test_evalexpr "42. 42" 42.42
test_evalexpr "42 . 42" 42.42
test_evalexpr " 42 . 42 " 42.42

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

# Exponent priority
echo "Test exponent priority"
test_evalexpr "2e3^2" 4000000.00
test_evalexpr "(2e3)^2" 4000000.00
test_evalexpr "2e(3^2)" 2000000000.00
test_evalexpr "1.01^(2e2)" 7.32

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

echo
echo -e "$BOLD""$count_test_passed tests passed""$UNBOLD"
echo -e "$BOLD""$count_test_failed tests failed""$UNBOLD"
