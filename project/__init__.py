#imports
from flask import Flask

#config
app = Flask(__name__, instance_relative_config=True)
app.config.from_pyfile('flask.cfg')

from . import views
