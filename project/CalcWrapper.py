import subprocess, re, pathlib, os

operators = {"(": -1, "=": 0, "+" : 1, "-" : 1, "*" : 2, "/" : 2, "^": 3, "log" : 3}

def build_constraint_system(converted_eqn):
    filePath = os.path.join(pathlib.Path(__file__).parent.absolute(),'racket', 'ConstraintSystemBuilder.rkt')

    calcOut = str(subprocess.run(['racket', filePath, converted_eqn],
            stdout=subprocess.PIPE).stdout)
    return(re.search("\"(.*)\"", calcOut).group(1))

def calc(constraint_system):
    filePath = os.path.join(pathlib.Path(__file__).parent.absolute(),'racket', 'TempConstraintSystem.rkt')

    consysfile = open(filePath, 'w')
    consysfile.write("#lang racket\n(require \"ConstraintSystemBase.rkt\")\n" + constraint_system)
    consysfile.close()

    calcOut = str(subprocess.run(['racket', filePath],
            stdout=subprocess.PIPE).stdout)

    return(re.search("'(.*)\\\\n'", calcOut).group(1))

def replace_with_spaces(replace, str):
    for i in replace:
        str = str.replace(i, " " + i +" ")
    return str

def unary_to_binary_minus(charList):
    for i, c in enumerate(charList):
        if i == len(charList) - 1:
            continue
        elif c == "-" and (i == 0 or charList[i-1] in operators.keys()):
            charList.insert(i,"(")
            charList[i+1]="-1"
            charList.insert(i+2,"*")
            operandEndIndex = i+4
            if charList[i+3] in operators.keys():
                for c in charList[i+4:]:
                    operandEndIndex += 1
                    if c == ")":
                        break;
            charList.insert(operandEndIndex,")")
    return charList

def tokenize(eq):
    operators_and_paren = list(operators.keys())
    operators_and_paren.append(")")
    eq = replace_with_spaces(operators_and_paren, eq)
    return eq.split()

def rev(eq):
    rev_eq = []
    for i in eq[::-1]:
        if i == "(":
            rev_eq.append(")")
        elif i == ")":
            rev_eq.append("(")
        else:
            rev_eq.append(i)

    return rev_eq

def is_number(s):
    return s.isnumeric() or s[1:].isnumeric()

def in_to_pre(raw_eq):
    formula = rev(unary_to_binary_minus(tokenize(raw_eq)))

    op_stack = []
    eq = []

    for token in formula:
        if (token == "("):
            op_stack.append("(")
        elif (token == ")"):
            while(op_stack[-1] != "("):
                eq.append(op_stack.pop())
            op_stack.pop()
        elif token in operators.keys():
            if (len(op_stack) == 0 or operators[token] > operators[op_stack[-1]]):
                op_stack.append(token)
            else:
                while (len(op_stack) != 0 and operators[token] <= operators[op_stack[-1]]):
                    eq.append(op_stack.pop())
                op_stack.append(token)
        elif token.isalpha() or is_number(token):
            eq.append(token)

    while op_stack:
        eq.append(op_stack.pop())

    return rev(eq)

def parenthesize(formula):
    operator_amounts = {"=" : 2, "+" : 2, "-" : 2, "*" : 2, "/" : 2, "^": 2}

    eq = ""
    op_stack, operands = [], []
    occurences = [0]

    for char in formula:
        while op_stack and operator_amounts[op_stack[-1]] == occurences[-1]:
            eq = eq[:-1]
            eq += ") "
            occurences.pop()
            occurences[-1] += 1
            op_stack.pop()

        if char in operator_amounts.keys():
            eq += "(" + char + " "
            op_stack.append(char)
            occurences.append(0)
        elif char.isalpha() or is_number(char):
            eq += "(" + char + ")"  + " "
            occurences[-1] += 1


    while eq[-1] == " ":
        eq = eq[:-1]
    for i in op_stack:
        eq += ")"

    return eq

def convert_to_scheme(raw_eq):
    return(parenthesize(in_to_pre(raw_eq)))
