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

def rev(str):
    temp = str[::-1]
    for n, i in enumerate(temp):
        if i == "(":
            temp = temp[:n] + ")" + temp[n+1:]
        elif i == ")":
            temp = temp[:n] + "(" + temp[n+1:]
    return temp

def infix_to_prefix(formula):
    operators = {"(" : -1, "=": 0, "+" : 1, "-" : 1, "*" : 2, "/" : 2, "^": 3}
    formula = rev(formula.replace(" ", ""))
    op_stack = []
    eq = []

    for char in formula:
        #Implement sin/cos/tan later here by changing condition

        if char.isalpha() or char.isdigit():
            eq.append(char)
        elif (char == "("):
            op_stack.append("(")
        elif (char == ")"):
            while(op_stack[-1] != "("):
                eq.append(op_stack.pop())
            op_stack.pop()
        elif (char in operators.keys()):
            if (len(op_stack) == 0 or operators[str(char)] > operators[op_stack[-1]]):
                op_stack.append(char)
            else:
                while (len(op_stack) != 0 and operators[str(char)] <= operators[op_stack[-1]]):
                    eq.append(op_stack.pop())
                op_stack.append(char)

    #Dump operators left in stack into equation
    while op_stack:
        eq.append(op_stack.pop())

    eq_string = ""
    for i in eq:
        eq_string += i

    eq_string = rev(eq_string)
    return eq_string


def parenthesize(formula):
    formula = infix_to_prefix(formula)
    operator_amounts = {"=" : 2, "+" : 2, "-" : 2, "*" : 2, "/" : 2, "^": 2}
    eq = ""
    op_stack = []
    operands = []
    n = 0

    # while n < len(formula):
    #     if (formula[n] in operator_amounts.keys()):
    #         op_stack.append(formula[n])
    #     elif (formula[n].isalpha() or formula[n].isdigit()):
    #         operands.append(formula[n])
    #         if len(operands) == 2:
    #             eq += "(" + str(op_stack.pop()) + str(operands.pop()) + str(operands.pop()) + ")"
    #     n+=1


    return formula
