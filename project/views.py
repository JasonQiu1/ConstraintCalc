from . import app
from .CalcWrapper import *
from .forms import *
from flask import render_template, request, session, flash
import os

app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 1

#Home Page of the Website
#GET method is default when loading a page
#Once submitted, "POST" method is used
@app.route("/", methods = ["POST", "GET"])
def home():
    # Create equation and variable forms
    print("***Creating VariableForm***")
    var_form = VariableForm()
    var_list_of_dicts = None

    var_bindings = None

    print("***Creating EquationForm***")
    equation_form = EquationForm()

    equation = None

    if equation_form.data and equation_form.validate():
        # BRANCH for valid equation form submission
        print("***Running equation_form submission!***")
        
        # Store the raw and processed equations in local session
        raw_equation = equation_form.equation.data
        session["raw_equation"] = raw_equation
        equation = to_racket_lists(raw_equation)
        session["equation"] = equation

        # Get variables from the equation
        var_list_of_dicts = [{i:f"Enter value for {i}"} for i in get_vars(raw_equation)]
        session["var_list_of_dicts"] = var_list_of_dicts

        # Populate var_form with variables and set their labels
        print("*** Populating var_form with variables")
        for i in var_list_of_dicts:
            var_form.vars.append_entry(i)

        # Return variable form for user to fill 
        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]))

    elif var_form.vars.data and var_form.validate():
        # BRANCH for valid variable form submission
        print("***Running var_form submission!***")

        # Grab session data to preserve the equation in the equation form
        equation_form.equation.data = session['raw_equation']
        session["var_bindings"] = []

        #Add variable bindings (dictionary) to list
        for n, i in enumerate(var_form.vars.data):
            r = dict(i)
            del r['csrf_token']
            session["var_bindings"].append({list(session["var_list_of_dicts"][n].keys())[0] : list(r.values())[0]})

        unknown_var = ""
        for d in session["var_bindings"]:
            if d[list(d)[0]] == "":
                unknown_var = list(d)[0]

        # Evaluate the equation with numbers substituted in for the variables in racket
        try:
            ans = calc(substitute(session["var_bindings"], session["raw_equation"]))
        except:
            return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]), exception = 1)

        # Generate the diagram from the constraint system and return both the answer and diagram
        generate_diagram_from_eqn(substitute_ans(session["var_bindings"], session["raw_equation"]))
        dpath = 'static/diagram1.png'
        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]), unknown = unknown_var, answer = ans, diagram = dpath)

    elif var_form.vars.data and not var_form.validate():
        # BRANCH for invalid variable form submission
        
        # Grab session data to preserve equation in equation box
        equation_form.equation.data = session['raw_equation']
        session["var_bindings"] = []

        # flash a warning on invalid variables and return the page
        flash(list(var_form.errors.items())[0][1][0])
        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]))

    # Default return - page with just equation form
    return render_template("calc.html", equation_form = equation_form, equation = equation)
