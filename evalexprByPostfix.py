#!/usr/bin/env python3

import re
import string
from sys import argv
from math import floor

operatorPrecedences = {
    '^': 3,
    '*': 2,
    '/': 2,
    '%': 2,
    '+': 1,
    '-': 1,
    '>': 0,
    '<': 0,
    '>=': 0,
    '<=': 0,
    '==': 0,
    '!=': 0,
    '&&': -1,
    '||': -2,
}

operators = {
    '^': lambda arg1, arg2: arg1 ** arg2,
    '*': lambda arg1, arg2: arg1 * arg2,
    '/': lambda arg1, arg2: arg1 / arg2,
    '%': lambda arg1, arg2: arg1 % arg2,
    '+': lambda arg1, arg2: arg1 + arg2,
    '-': lambda arg1, arg2: arg1 - arg2,
    '>': lambda arg1, arg2: arg1 > arg2,
    '<': lambda arg1, arg2: arg1 < arg2,
    '>=': lambda arg1, arg2: arg1 >= arg2,
    '<=': lambda arg1, arg2: arg1 <= arg2,
    '==': lambda arg1, arg2: arg1 == arg2,
    '!=': lambda arg1, arg2: arg1 != arg2,
    '&&': lambda arg1, arg2: arg1 and arg2,
    '||': lambda arg1, arg2: arg1 or arg2,
}

unaryOperators = {
    '-': lambda x: -x,
    '!': lambda x: not x
}


def round_half_up(n, decimals=0):
    multiplier = 10 ** decimals
    return floor(n*multiplier + 0.5) / multiplier


def isOperand(x):
    return all([c in string.digits + '.' for c in x])


def isOperator(x):
    return not isOperand(x)


def isOpPriorited(top, op):
    return operatorPrecedences[op] <= operatorPrecedences[top]


def infixToPostfix(infixExpression):
    infixExpression = re.sub('\s', '', infixExpression)
    postfixExpression = []
    operatorStack = []

    lastOp = ''
    while infixExpression:
        if not lastOp or lastOp == '(' or lastOp in operatorPrecedences:
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
        elif op == '!':
            arg1 = stack.pop(-1)
            opRes = unaryOperators[op](arg1)
            stack.append(opRes)
        else:
            arg1 = stack.pop(-2)
            arg2 = stack.pop(-1)
            opRes = operators[op](arg1, arg2)
            stack.append(opRes)

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
