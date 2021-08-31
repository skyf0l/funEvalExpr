#!/usr/bin/env python3

import sys
import re
import string
from sys import argv
from math import floor

parenthesis = [
    '(',
    ')',
]

operators = [
    '>=',
    '<=',
    '==',
    '!=',
    '&&',
    '||',
    'e',
    '^',
    '*',
    '/',
    '%',
    '+',
    '-',
    '>',
    '<',
    '!',
]

operatorPrecedences = {
    'e': 120,
    '^': 120,
    'pos': 110,
    'neg': 110,
    'not': 110,
    '*': 100,
    '/': 100,
    '%': 100,
    '+': 95,
    '-': 95,
    '>': 80,
    '<': 80,
    '>=': 80,
    '<=': 80,
    '==': 80,
    '!=': 80,
    '&&': 75,
    '||': 70,
}

operatorsFunctions = {
    'e': lambda arg1, arg2: arg1 * 10 ** arg2,
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
    '+': 'pos',
    '-': 'neg',
    '!': 'not',
}

unaryOperatorsFunctions = {
    'pos': lambda x: x,
    'neg': lambda x: -x,
    'not': lambda x: not x,
}


def round_half_up(n, decimals=0):
    multiplier = 10 ** decimals
    return floor(n*multiplier + 0.5) / multiplier


def findNextOp(expression):
    for op in operators + parenthesis:
        if op == expression[:len(op)]:
            return op
    raise ValueError


def isOperand(x):
    return all([c in string.digits + '.' for c in x])


def isOperator(x):
    return x in operators


def isOpPriorited(top, op):
    return operatorPrecedences[op] <= operatorPrecedences[top]


def infixToPostfix(infixExpression):
    infixExpression = re.sub('\s', '', infixExpression)
    postfixExpression = []
    operatorStack = []

    lastOp = ''
    while infixExpression:
        m = re.search('^([\d\.]+)', infixExpression)
        if m:
            op = m.group(1)
            infixExpression = infixExpression[len(op):]
            postfixExpression.append(round_half_up(float(op), 2))
        else:
            op = findNextOp(infixExpression)
            infixExpression = infixExpression[len(op):]
            if (not lastOp or lastOp == '(' or isOperator(lastOp)) and op in unaryOperators:
                operatorStack.append(unaryOperators[op])
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
        elif op in unaryOperatorsFunctions:
            arg1 = stack.pop(-1)
            opRes = unaryOperatorsFunctions[op](arg1)
            stack.append(opRes)
        elif op in operatorsFunctions:
            arg1 = stack.pop(-2)
            arg2 = stack.pop(-1)
            opRes = operatorsFunctions[op](arg1, arg2)
            stack.append(opRes)
        else:
            raise ValueError

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
