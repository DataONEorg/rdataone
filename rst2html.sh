#!/bin/bash
# Script to generate a HTML version of the README.txt file
# Using a stylesheet similar to that used by the 
# architecture docs

curl "https://repository.dataone.org/software/tools/trunk/docutils/css/dataone.css" > /tmp/dataone.css
rst2html.py --stylesheet /tmp/dataone.css README.rst > README.html

