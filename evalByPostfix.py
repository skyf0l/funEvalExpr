#!/usr/bin/env python3

import string

def isOperand(x):
    return all([c in string.digits for c in x])

def isOperator(x):
    return not isOperand(x)

def hasLessOrEqualPriority(top, op):
    precedence = {'+':1, '-':1, '*':2, '/':2, '%':2, '^':3}
    return precedence[op] <= precedence[top]

def infixToPostfix(infixExpression):
    postfixExpression = []
    operatorStack = []

    for op in infixExpression:
        if isOperand(op):
            postfixExpression.append(op)
        else:
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
                    while operatorStack and operatorStack[-1] != '(' and hasLessOrEqualPriority(operatorStack[-1], op):
                        postfixExpression.append(operatorStack.pop(-1))
                    operatorStack.append(op)

    while operatorStack:
        postfixExpression.append(operatorStack.pop(-1))
    return postfixExpression

def evalPostfix(postfixExpression):
    stack = []

    for op in postfixExpression:
        if op == '+':
            stack.append(float(stack.pop(-2)) + float(stack.pop(-1)))
        elif op == '-':
            stack.append(float(stack.pop(-2)) - float(stack.pop(-1)))
        elif op == '*':
            stack.append(float(stack.pop(-2)) * float(stack.pop(-1)))
        elif op == '/':
            stack.append(float(stack.pop(-2)) / float(stack.pop(-1)))
        elif op == '%':
            stack.append(float(stack.pop(-2)) % float(stack.pop(-1)))
        elif op == '^':
            stack.append(float(stack.pop(-2)) ** float(stack.pop(-1)))
        else:
            stack.append(op)

    return stack.pop()

expression = '5-8+2*(8+5*(9*5+5*5)/2-2)*2'
postfixExpression = infixToPostfix(expression)

print(eval(expression))
print(evalPostfix(postfixExpression))