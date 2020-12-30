from flask_wtf import FlaskForm
from wtforms import Form, StringField, DecimalField, TextField, SubmitField, FieldList, FormField, Form
from wtforms.validators import DataRequired, NumberRange, Length, ValidationError
from wtforms.fields import Label

class EquationForm(FlaskForm):
    equation = StringField(
        'Equation',
        [DataRequired()]
    )
    submit = SubmitField('submit')

class VariableEntryForm(FlaskForm):
    variable = StringField(
        'var'
    )

class VariableForm(FlaskForm):
    vars = FieldList(FormField(VariableEntryForm))
    submit = SubmitField('submit')

    # Validate all of subforms in vars by checking if all but 1 var has a value
    def validate_vars(form, field):
        num_null_values = 0
        for var_entry_form in field.data:
            r = dict(var_entry_form)
            del r['csrf_token']
            val = list(r.values())[0]
            if val is "":
                num_null_values += 1
            elif not val.isnumeric():
                raise ValidationError(f" \"{val}\" is not a number. Please input numbers.")
        if num_null_values != 1:
            print(f"*** ERROR: There were {num_null_values} unassigned variable(s). There should be 1 ***")
            raise ValidationError(f"There were {num_null_values} unassigned variable(s). There should be 1")

def format_var_form(var_form, var_list_of_dicts):
    for n, i in enumerate(var_list_of_dicts):
        l = Label(field_id = "variable", text = list(i.keys())[0])
        var_form.vars[n].variable.label = l
    return var_form
