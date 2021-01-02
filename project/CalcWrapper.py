import subprocess, re, pathlib, os
from .DiagramGen import *

# all the operators and their precedence except ")"
operators = {"(": -1, "=": 0, "+" : 1, "-" : 1, "*" : 2, "/" : 2, "^": 3, "log" : 3}

# returns the answer to a substituted equation
def calc(raw_eqn):
    return exec_constraint_system(build_constraint_system(build_constraint_list(to_racket_lists(raw_eqn))))

# generates a dragram of the constraint system from the unsubstituted equation (with ans substitued)
def generate_diagram_from_eqn(raw_eqn_with_ans):
    return generate_diagram(build_constraint_list(to_racket_lists(raw_eqn_with_ans)))

# executes a racket program given the program's file path and arguments, and returns the part of the output captured by the regex
def exec_racket_prog(file_path, arg, extract_regex):
    out = str(subprocess.run(['racket', file_path, arg],
            stdout=subprocess.PIPE).stdout)
    return(re.search(extract_regex, out).group(1))

# returns the absolute filePath of a relative path beginning from the current directory (/project)
def rel_to_abs_path(relative_path):
    return(os.path.join(pathlib.Path(__file__).parent.absolute(),relative_path))

# returns the a list of constraints that make up the constraint system given a prefixed, parenthesized equation
def build_constraint_list(converted_eqn):
    return(exec_racket_prog(rel_to_abs_path("racket/ConstraintListBuilder.rkt"), converted_eqn, "\'(.*)\\\\n"))

# returns the full Racket-executable constraint system
def build_constraint_system(constraint_network):
    return(exec_racket_prog(rel_to_abs_path("racket/ConstraintSystemBuilder.rkt"), constraint_network, "\"(.*)\""))

# convert rational numbers into floating point numbers
def convert(s):
    try:
        return float(s)
    except ValueError:
        num, denom = s.split('/')
        return float(num) / float(denom)

# returns the value of the unknown in the constraint system
def exec_constraint_system(constraint_system):
    file_path = rel_to_abs_path("racket/TempConstraintSystem.rkt")
    con_sys_file = open(file_path, 'w')
    con_sys_file.write("#lang racket\n(require \"ConstraintSystemBase.rkt\")\n" + constraint_system)
    con_sys_file.close()
    return(exec_racket_prog(file_path, constraint_system, "'(.*)\\\\n'"))

# Puts spaces around given "replace" characters
def replace_with_spaces(replace, string):
    for i in replace:
        string = string.replace(i, " " + i +" ")
    return string

# changes unary minus (e.g. -5) to binary minus (e.g. (-1 * 5) )
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

# Splits input based on spaces
def tokenize(eq):
    operators_and_paren = list(operators.keys())
    operators_and_paren.append(")")
    eq = replace_with_spaces(operators_and_paren, eq)
    return eq.split()

# Reverses given equation (Also flips "(" for ")" and vice versa)
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

# Checks if s is a number including floating point and negatives
def is_number(s):
    return s.isnumeric() or s[1:].replace(".","").isnumeric() or s.replace(".","").isnumeric()

# First tokensizes, reverses, then uses Shunting Yard algorithm to convert from infix to postfix, and finally reverses to get prefix
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

#Inserts parenthesis into prefix formula
#Algorithm works by iterating through formula and counts how many arguments it has
#If it has enough arguments for the operator's operator_amounts it inserts the parenthesis
#Nested operator are handled by storing argument amounts in a list that appends/pops accordingly
def parenthesize(formula):
    operator_amounts = {"=" : 2, "+" : 2, "-" : 2, "*" : 2, "/" : 2, "^": 2, "log": 2}

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

# converts the raw equation into a representation racket can use (e.g. prefixed lists)
def to_racket_lists(raw_eq):
    try:
        return(parenthesize(in_to_pre(raw_eq)))
    except:
        return(f"Oops! (Probably) invalid Input: {raw_eq}")

# Gets variables from raw_eq
def get_vars(raw_eq):
    ops = list(operators.keys())
    ops.append(")")
    remove_tokens = ops
    tokens = tokenize(raw_eq)
    for i in tokens:
        if i in remove_tokens:
            tokens.remove(i)
        elif is_number(i):
            tokens.remove(i)
    return tokens

# Substitutes bindings into equations and changes unknown to 'ans'.
def substitute(listbindings, equation):
    bindings = {}
    for dict in listbindings:
        bindings[list(dict)[0]] = dict[list(dict)[0]]
    tokens = tokenize(equation)
    substituted_eqn = ""
    for t in tokens:
        if (t in operators.keys() or t == ")"):
            substituted_eqn += str(t)
            continue
        if (t in bindings.keys()):
            if (bindings[t] == ""):
                substituted_eqn += "ans"
            else:
                substituted_eqn += str(bindings[t])
    return substituted_eqn
