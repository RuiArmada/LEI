import re

file = open("../Files/report.xml")

for line in file:

    if re.search(r'<\?xml version="1.0" encoding= "(.+)"\?>',line):
        encoding = re.search(r'encoding="(.+)"', line).group(1)
        print(f'<!DOCTYPE html>\n<html>\n<head>\n   <meta charset="{encoding}">\n</head>')

    elif re.search(r'<report>',line):
        print('<body>')

    elif re.search(r'</report>',line):
        print('</body>')

    elif m := re.search(r'<title>(.*)</title>',line):
        print(f'<h1>{m.group(1)}</h1>')

    elif m := re.search(r'<date>(.*)</date>',line):
        print(f'<h2>{m.group(1)}</h2><hr>')

    elif re.search(r'authors',line):
        print(r'<h3>Authors</h3><ul>')
    
    elif re.search(r'</authors>',line):
        print(r'</ul>')

    elif re.search(r'author',line):
        print(r'<li>')
    
    elif re.search(r'</author>',line):
        print(r'</li>')

    elif re.search(r'<name>', line):
        m = re.search(r'<name>(.*)</name>',line)
        print('{' + m.group(1) + '}')

    elif re.search(r'<number>', line):
        m = re.search(r'<number>(.*)</number>',line)
        print('(' + m.group(1) + ')')

    elif re.search(r'<email>', line):
        m = re.search(r'<email>(.*)</email>',line)
        print(': ' + m.group(1))

    elif re.search(r'<summary>',line):
        print(r'<h3>Summary</h3><p>')
    
    elif re.search(r'</summary',line):
        print('</p>')

    else:
        m = re.search(r'.+', line)
        print(m.group())

file.close()