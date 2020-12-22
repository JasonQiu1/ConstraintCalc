from . import app
from flask import render_template, request
from .CalcWrapper import *

#Home Page of the Website
#Form with two boxes "a" and "b"
#GET method is default when loading a page
#Once submitted, "POST" method is used
@app.route("/", methods = ["POST", "GET"])
def home():
    #if the method that gets called is POST, pass values to compute()
    if request.method == "POST":
        equation = convert_to_scheme(request.form["equation"])
        constraint_system = build_constraint_system(equation)
        return f"<h1>{calc(constraint_system)}<h1>"

    #else display calc page
    else:
        return render_template("calc.html")
