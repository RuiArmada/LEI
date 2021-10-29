import re



#Problem 1
def name1():
    with open('../Files/emd.csv') as file:
        next(file)
        for line in file:
            name = re.search(r'^(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+),(.+)$',line)
            if name:
                first = name.group(4)
                last = name.group(5)
                city = name.group(8)
                print(first, last, city)

                

def name2():
    with open('../Files/emd.csv') as file:
        next(file)
        for i, line in enumerate(file):
            if i == 0: #ignores the first line
                continue
            slot = re.split(r',',line)
            print(f'Name: {slot[3]} {slot[4]}, City: {slot[7]}')



#Problem2
def alphClubs():
    with open('../Files/emd.csv') as file:
        next(file) 
        clubs = set()
        for line in file:
            slot = re.split(r',',line)
            club = slot[-4]
            clubs.add(club)
        clubs = sorted(clubs)
        print(clubs)
        print('Number of existing Clubs: ' + len(clubs))



#Problem 3
def alphMods():
    with open('../Files/emd.csv') as file:
        next(file)
        mods = set()
        for line in file:
            slot = re.split(r',',line)
            mod = slot[-5]
            mods.add(mod)
        mods = sorted(mods)
        print(mods)
        print('Number of existing Modalities: ' + len(mods))



#Problem 4
def gender():
    with open('../Files/emd.csv') as file:
        next(file)
        aux = { 'M': 0, 'F': 0}
        for line in file:
            slot = re.split(r',',line)
            gender = slot[6]
            aux[gender] += \
    print(f'Masculine: {aux["M"]}')
    print(f'Feminine: {aux["F"]}')



#Problem 5
def fem2020():
    with open('../Files/emd.csv') as file:
        next(file)
        federated = 0
        approved = 0
        for line in file:
            slot = re.split(r',',line.strip()) # strip takes the blank spaces
            [fed, aprov] = slot[-2:] #last 2 elements of the list
            if fed:
                federated += 1
            if aprov:
                approved += 1
    print('Federated:' + str(federated))
    print('Approved:' + str(approved))



#Problem 6
def ageAtl():
    with open('../Files/emd.csv') as file:
        next(file)
        age = {}
        for line in file:
            slot = re.split(r',',line)
            gender = slot[6]
            if gender != 'F':
                continue
            date = re.split(r'-', slot[2])
            year = date[0]
            if year in age:
                age[year] += 1
            else:
                age[year] = 1
    print(dict(sorted(age.items(), key=lambda p: p[0])))



#Problem 7
def nAtlMod():
    with open('../Files/emd.csv') as file:
        next(file)
        athletes = []
        for line in file:
            slot = re.split(r',',line)
            if slot[8] == 'Atletismo':
                complete_name = slot[3] + ' ' + slot[4]
                athletes.append(complete_name, int(slot[5]))
    athletes.sort(key=lambda p: (p[1],p[0]))
    print(athletes)