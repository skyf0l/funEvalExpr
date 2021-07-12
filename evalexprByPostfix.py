#!/usr/bin/env python3

import re
import string
from sys import argv
from math import floor


def round_half_up(n, decimals=0):
    multiplier = 10 ** decimals
    return floor(n*multiplier + 0.5) / multiplier


def isOperand(x):
    return all([c in string.digits + '.' for c in x])


def isOperator(x):
    return not isOperand(x)


def isOpPriorited(top, op):
    precedence = {'+': 1, '-': 1, '*': 2, '/': 2, '%': 2, '^': 3}
    return precedence[op] <= precedence[top]


def infixToPostfix(infixExpression):
    postfixExpression = []
    operatorStack = []

    lastOp = ''
    while infixExpression:
        if not lastOp or lastOp == '(':
            m = re.search('^(-?[\d\.]+)', infixExpression)
        else:
            m = re.search('^([\d\.]+)', infixExpression)
        if m:
            op = m.group(1)
            infixExpression = infixExpression[len(op):]
            postfixExpression.append(round_half_up(float(op), 2))
        else:
            op = infixExpression[0]
            infixExpression = infixExpression[1:]
            if not operatorStack or operatorStack[-1] == '(':
                operatorStack.append(op)
            else:
                if op == '(':
                    operatorStack.append(op)
                elif op == ')':
                    while operatorStack[-1] != '(':
                        postfixExpression.append(operatorStack.pop(-1))
                    operatorStack.pop(-1)
                else:
                    while operatorStack and operatorStack[-1] != '(' and isOpPriorited(operatorStack[-1], op):
                        postfixExpression.append(operatorStack.pop(-1))
                    operatorStack.append(op)
        lastOp = op

    while operatorStack:
        postfixExpression.append(operatorStack.pop(-1))
    return postfixExpression


def evalPostfix(postfixExpression):
    stack = []

    for op in postfixExpression:
        if type(op) is float:
            stack.append(op)
        else:
            arg1 = stack.pop(-2)
            arg2 = stack.pop(-1)
            if op == '+':
                stack.append(arg1 + arg2)
            elif op == '-':
                stack.append(arg1 - arg2)
            elif op == '*':
                stack.append(arg1 * arg2)
            elif op == '/':
                stack.append(arg1 / arg2)
            elif op == '%':
                stack.append(arg1 % arg2)
            elif op == '^':
                stack.append(arg1 ** arg2)
            else:
                stack.append(op)

    return stack.pop()


if len(argv) == 2:
    try:
        expression = argv[1]
        postfixExpression = infixToPostfix(expression)
        result = evalPostfix(postfixExpression)
        print(f'{round_half_up(result, 2):.2f}')
    except:
        exit(84)
else:
    exit(84)
