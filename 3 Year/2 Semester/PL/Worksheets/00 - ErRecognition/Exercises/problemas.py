import re



def mysearch(ER):
    source = input(">>")
    while source != "": #while there is no input the source will be ""
        y = re.search(ER, source)
        if(y): #if it is found
            print("VALID")
        else:
            print("INVALID")
        source = input(">>")



#Problem 1
def alienUsername():
    mysearch(r'^[_.][0-9]+(?i:[a-z]){3,}_?$')



#Problem 2
def ipAdress():
    file = open("../Files/Ip.txt" , "r")

    for line in file:
        y = re.search(r'((2[0-4][0-9]|25[0-5]|1[0-9][0-9]|[1-9][0-9])\.){3}(2[0-4][0-9]|25[0-5]|1[0-9][0-9]|[1-9][0-9])', line)
        x = re.search(r'([0-9a-f]{1,4}:){7}[0-9a-f]{1,4}', line)
        if y:
            print("IPv4",end=" - ")
        elif x:
            print("IPv6",end=" - ")
        else:
            print("ERROR",end=" - ")
        print(line,end="")
    file.close()



#Problem 3
def latLong():
    file = open("../Files/Coords.txt" , "r")

    print("----------------Match Initiating------------------")

    for line in file:
        c = re.search(r'^\(((\+|\-)?(([0-8][0-9](\.[0-9]+)?)|90(\.0+)?)), ((\+|\-)?((1[0-7][0-9](\.[0-9]+)?)|180(\.0+)?))\)$', line)
        if c:
            print("VALID")
        else:
            print("INVALID")
    file.close()
    print("----------------Match Finalizing------------------")



#Problem 4
def licencePlate():
    with open("../Files/Text.txt" , "r") as file:
        string = file.read().replace('\n',' ')

        print("----------------Match Initiating------------------")
        print(string + '\n')

        pattern = r'([0-9]{4}-){3}([0-9]{4})|([0-9]{4}:){3}([0-9]{4})|([0-9]{4}\.\.\.){3}([0-9]{4})'

        pad = re.findall(pattern,string)
        print(pad)

        print("----------------Match Finalizing------------------")

