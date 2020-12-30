from . import app
from .CalcWrapper import *
from .forms import *
from flask import render_template, request, session, flash

app.config['SECRET_KEY'] = 'secret'

#Home Page of the Website
#GET method is default when loading a page
#Once submitted, "POST" method is used
@app.route("/", methods = ["POST", "GET"])
def home():
    #if the method that gets called is POST, pass values to compute()
    equation = convert_to_scheme(request.form["equation"])
    return f"<h1>{convert(exec_constraint_system(constraint_system))}<h1>"

    print("***Creating EquationForm***")
    equation_form = EquationForm()
    equation = None

    print("***Creating VariableForm***")
    var_form = VariableForm()
    var_list_of_dicts = None

    var_bindings = None

    # constraint_system = build_constraint_system(equation)
    if equation_form.data and equation_form.validate():
        print("***Running equation_form submission!***")

        raw_equation = equation_form.equation.data
        equation = convert_to_scheme(raw_equation)
        session["equation"] = equation

        constraint_list = build_constraint_list(equation)
        session["constraint_list"] = constraint_list

        var_list_of_dicts = [{i:f"Enter value for {i}"} for i in get_vars(raw_equation)]
        session["var_list_of_dicts"] = var_list_of_dicts

        #Populate var_form with variables and set their labels
        print("*** Populating var_form with variables")
        for i in var_list_of_dicts:
            var_form.vars.append_entry(i)


        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]))

    elif var_form.vars.data and var_form.validate():
        print("***Running var_form submission!***")

        session["var_bindings"] = []

        constraint_system = build_constraint_system(substitute(session["constraint_list"]))

        answer = exec_constraint_system(constraint_system)
        
        #Add variable bindings (dictionary) to list
        for n, i in enumerate(var_form.vars.data):
            r = dict(i)
            del r['csrf_token']
            session["var_bindings"].append({list(session["var_list_of_dicts"][n].keys())[0] : list(r.values())[0]})

        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]), var_bindings = session["var_bindings"])

    elif var_form.vars.data and not var_form.validate():
        flash(list(var_form.errors.items())[0][1][0])
        return render_template("calc.html", equation_form = equation_form, equation = session["equation"], var_form = format_var_form(var_form, session["var_list_of_dicts"]))

    return render_template("calc.html", equation_form = equation_form, equation = equation)
>>>>>>> Stashed changes
