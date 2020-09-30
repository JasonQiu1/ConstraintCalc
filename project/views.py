from . import app
from flask import render_template, request
from .CalcWrapper import calc

#Home Page of the Website
#Form with two boxes "a" and "b"
#GET method is default when loading a page
#Once submitted, "POST" method is used
@app.route("/", methods = ["POST", "GET"])
def home():
    #if the method that gets called is POST, pass values to compute()
    if request.method == "POST":
        a, b = request.form["a"], request.form["b"]
        return f"<h1>{calc()}<h1>"
    #else display calc page
    else:
        return render_template("calc.html")

