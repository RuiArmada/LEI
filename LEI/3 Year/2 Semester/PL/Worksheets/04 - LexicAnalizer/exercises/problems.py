import re
import sys

def sum1():
    flag = False
    sum = 0
    for line in sys:stdin:
        if re.search(r'on', line):
            flag = True
            print('Sum Enabled')
        elif re.search(r'off', line):
            flag = False
            print('Sum Disabled')
        elif re.search(r'=', line):
            print('Actual Sum: ', sum)
        elif m := re.search(r'[0-9]+', line):
            if flag:
                sum += int(m.group())
    print('Sum Total:',sum)

def sum2():
    for line in sys:stdin:
        elements = re.findall(r'on|off|\d+|=', line)
        print(elements)
        flag = False
        sum = 0
        for e in elements:
            if e == 'on':
                flag = True
                print('Sum Enabled')
            elif e == 'off':
                flag = False
                print('Sum Disabled')
            elif e == '=':
                print('Actual Sum: ', sum)
            else:
                if flag:
                    sum += int(e)
    print('Sum Total:', sum)