import subprocess, re, pathlib, os

def calc():
    filePath = os.path.join(pathlib.Path(__file__).parent.absolute(),'racket', 'constraintsys1.rkt')
    testFile = open(filePath,'w')

    # hardcoded string
    # will be built later through recursion (like derivative calc)
    codeOut = """
    ;; First two lines won't change
    #lang racket
    (require \"ConstraintLogic.rkt\")

    ;; (define (func...
    ;; include a letter for each unknown var
    (define (celsius-fahrenheit-converter c f)
    ;; connector for each layer of calc (parenthesis)
      (let ((u (make-connector))
            (v (make-connector))
            (w (make-connector))
            (x (make-connector))
            (y (make-connector)))
        ;; use operators to define constraints between layers
        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        'ok))

    ;; make connectors for unknown vars
    (define C (make-connector))
    (define F (make-connector))
    (celsius-fahrenheit-converter C F)
    ;; user must set values for n-1 vars
    (set-value! C 100 'user)

    ;; get value for unknown var (nth)
    ;; wrap answer in '[ ]'
    (display (string-append "["
                            (number->string (get-value F))
                            "]"))
    ;; TEST
    """

    testFile.write(codeOut)
    testFile.close()

    calcOut = str(subprocess.run(['racket', filePath],
            stdout=subprocess.PIPE).stdout)
    return(re.search('\[([^]]+)', calcOut).group(1))

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
            eq += char + " "
            occurences[-1] += 1


    while eq[-1] == " ":
        eq = eq[:-1]
    for i in op_stack:
        eq += ")"

    return eq

def convert_to_scheme(raw_eq):
    return(parenthesize(in_to_pre(raw_eq)))
