import re



file = open("../Files/report.xml")

content = file.read()



content = re.sub(r'<\?xml version="1.0" encoding= "(.+)"\?>',r'<!DOCTYPE html>\n<html>\n<head>\n   <meta charset="\1">\n</head> ', content)

content = re.sub(r'(</?)report>',r'\1body>', content)

content = re.sub(r'(</?)title(>)',r'\1h1\2', content)

content = re.sub(r'(</?)date(>)',r'\1h3\2\n<hr>', content)

content = re.sub(r'<authors>',r'<h3>Authors</h3><ul>', content)

content = re.sub(r'</authors>',r'</ul>', content)

content = re.sub(r'(</?)author(>)',r'\1li\2', content)

content = re.sub(r'</?name>',r'',content)

content = re.sub(r'<number>',r'(',content)

content = re.sub(r'</number>',r') ',content)

content = re.sub(r'<email>',r': ',content)

content = re.sub(r'</email>',r' ',content)

content = re.sub(r'<summary>',r'<h3>summary</h3><p>', content)

content = re.sub(r'</summary>',r'</p>', content)

print(content)



file.close()