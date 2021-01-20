from . import app
from .CalcWrapper import *
from .forms import *
from flask import render_template, request, session, flash
import os

app.config['SECRET_KEY'] = 'secret'
app.config['SEND_FILE_MAX_AGE_DEFAULT'] = 1

#Home Page of the Website
#GET method is default when loading a page
#Once submitted, "POST" method is used
@app.route("/", methods = ["POST", "GET"])
def home():

    print("***Creating VariableForm***")
    var_form = VariableForm()
    var_list_of_dicts = None

    var_bindings = None

    print("***Creating EquationForm***")
    equation_form = EquationForm()

    equation = None

    # constraint_system = build_constraint_system(equation)
    if equation_form.data and equation_form.validate():
        print("***Running equation_form submission!***")

        raw_equation = equation_form.equation.data
        session["raw_equation"] = raw_equation
        equation = to_racket_lists(raw_equation)
        session["equation"] = equation

        var_list_of_dicts = [{i:f"Enter value for {i}"} for i in get_vars(raw_equation)]
        session["var_list_of_dicts"] = var_list_of_dicts

        #Populate var_form with variables and set their labels
        print("*** Populating var_form with variables")
        for i in var_list_of_dicts:
            var_form.vars.append_entry(i)

        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]))

    elif var_form.vars.data and var_form.validate():
        print("***Running var_form submission!***")

        equation_form.equation.data = session['raw_equation']

        session["var_bindings"] = []

        # constraint_system = build_constraint_system(substitute(session["constraint_list"]))

        # answer = exec_constraint_system(constraint_system)

        #Add variable bindings (dictionary) to list
        for n, i in enumerate(var_form.vars.data):
            r = dict(i)
            del r['csrf_token']
            session["var_bindings"].append({list(session["var_list_of_dicts"][n].keys())[0] : list(r.values())[0]})

        unknown_var = ""
        for d in session["var_bindings"]:
            if d[list(d)[0]] == "":
                unknown_var = list(d)[0]

        try:
            ans = calc(substitute(session["var_bindings"], session["raw_equation"]))
        except:
            return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]), exception = 1)

        generate_diagram_from_eqn(substitute_ans(session["var_bindings"], session["raw_equation"]))
        dpath = 'static/diagram1.png'
        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]), unknown = unknown_var, answer = ans, diagram = dpath)

    elif var_form.vars.data and not var_form.validate():
        flash(list(var_form.errors.items())[0][1][0])
        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]))

    return render_template("calc.html", equation_form = equation_form, equation = equation)

@app.after_request
def add_header(response):
    # response.cache_control.no_store = True
    if 'Cache-Control' not in response.headers:
        response.headers['Cache-Control'] = 'no-store'
    return response
