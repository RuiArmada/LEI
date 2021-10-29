import re

file = open("../Files/report.xml")

for line in file:
    m = re.search(r'(<[a-z]+>)?([^<]*)(</[a-z]+>)?',line.strip())
    print(m.groups())

file.close()