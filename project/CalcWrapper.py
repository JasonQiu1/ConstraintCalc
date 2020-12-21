import subprocess, re, pathlib, os

def build_constraint_system(raw_eqn):
    filePath = os.path.join(pathlib.Path(__file__).parent.absolute(),'racket', 'ConstraintSystemBuilder.rkt')

    calcOut = str(subprocess.run(['racket', filePath, str(convert_to_scheme(raw_eqn))],
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

def tokenize(eq):
    eq = replace_with_spaces(["+","-","*","/","(",")","=", "sin", "cos"], eq)
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

def in_to_pre(raw_eq):
    formula = rev(tokenize(raw_eq))

    op_stack = []
    eq = []

    operators = {"(": -1, "=": 0, "+" : 1, "-" : 1, "*" : 2, "/" : 2, "^": 3, "sin" : 3, "cos" : 3}

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
        elif token.isalpha() or token.isdigit():
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
        elif char.isalpha() or char.isdigit():
            eq += "(" + char + ")"  + " "
            occurences[-1] += 1


    while eq[-1] == " ":
        eq = eq[:-1]
    for i in op_stack:
        eq += ")"

    return eq

def convert_to_scheme(raw_eq):
    return(parenthesize(in_to_pre(raw_eq)))
