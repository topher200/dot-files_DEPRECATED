pylint -f parseable -i yes -r no %1
pep8.py --ignore=E221,E701,E202 --repeat %1
exit /b 0
