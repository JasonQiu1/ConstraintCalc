# Constraint Calculator
https://concal.codes. A website implementing and extending the concept of constraint programming as found in Section 3.3.5 of _Structure and Interpretation of Computer Programs_ (see https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html#%_sec_3.3.5) into a functional, user-friendly website. Uses Racket and Python for backend calculations and Flask, Gunicorn, and NGINX to serve the webpage.

## Overview
- input any formula you want (no need to add spaces)
- input the values for all the variables except the unknown
- hit calculate and the result will be displayed
- a diagram of the internal constraint system will also be displayed

## Limitations
- the formula cannot contain more than one unknown, nor can it contain more than one instance of an unknown
- currently only supports addition, subtraction, multiplication, division, power, and log.
