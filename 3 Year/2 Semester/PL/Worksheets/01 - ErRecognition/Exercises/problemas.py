import re

exercice_a = re.compile(r"(?i:alentejo)")
exercise_a_ans = set()

exercise_b_ans = dict()

exercise_c = re.compile(r"\.mp3")
exercise_c_ans = set()

exercise_d = re.compile(r"(?i:jesus)")
exercise_d_ans = set()

with open("../Files/ArqM.txt" , encoding="utf-8") as file:
    for line in file.readlines():
        args = re.split(r"::", line)

        #exercise a
        if exercice_a.match(args[0]):
            exercise_a_ans.add(args[2])

        #exercise b
        try:
            exercise_b_ans[args[0]] += 1
        except KeyError:
            exercise_b_ans[args[0]] = 1

        #exercise c
        if exercise_c.search(args[4]):
            exercise_c_ans.add(args[2])

        #exercise d
        if exercise_d.search(args[2]):
            exercise_d_ans.add(args[2])

print("\n")
print("Exercise a) \n As musicas alentejanas são:", exercise_a_ans)
print("\n")
print("Exercise b) \n Musicas por regiao:",exercise_b_ans)
print("\n")
print("Exercise c) \n As musicas disponiveis em MP3 sao:", exercise_c_ans) 
print("\n")
print(f"Exercise d) \n Existem {len(exercise_d_ans)} musicas com 'Jesus' no título, que são:", exercise_d_ans)     