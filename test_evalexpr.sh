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

# error handling
echo -e "$BOLD""Test error handling""$UNBOLD"
test_evalexpr_error_handling ""
test_evalexpr_error_handling "+"
test_evalexpr_error_handling "2+"
test_evalexpr_error_handling "+2"
test_evalexpr_error_handling "2**2"

# result
echo
echo -e "$BOLD""Test result""$UNBOLD"
test_evalexpr "3+5.34" 8.34
test_evalexpr "(0.345+5)*(-2-1)/3" -5.35

test_evalexpr "(3+2)*5" 25.00
