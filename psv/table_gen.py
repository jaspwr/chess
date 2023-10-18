import sys
from os import listdir
from os.path import isfile, join

path = "./"

def clamp_string(s, max_len):
    if len(s) > max_len:
        return s[:max_len]
    return s

def gen_table(file):
    lines = ""
    with open(file) as f:
        lines = f.readlines()

    if lines[0] != "P2\n":
        print("Invalid file.")
        exit()

    values = []

    max_grey_value = 0;

    for line in lines:
        line = line.replace("\n", "")
        if line[0] == '#' or line == "P2":
            continue

        if " " in line:
            w, h = line.split(" ")
            if w != "8" or h != "8":
                print("Invalid dimensions.")
                exit()
            continue

        if not line.isdigit():
            print("Invalid file.")
            exit()

        n = int(line)

        if max_grey_value == 0:
            max_grey_value = n
            continue

        values.append(clamp_string(str(float(n) / float(max_grey_value)), 8) + "f")

    if len(values) != 64:
        print("Invalid number of values.")
        exit()

    var_name = file.replace(".pgm", "").replace(path, "").upper()

    for c in var_name:
        if not c.isalnum() and c != "_":
            print("Invalid character in filename. Only alphanumeric characters and underscores are allowed.")
            exit()

    c_definition = "static float PSV_" + var_name + "[64] = { "
    c_definition += ", ".join(values)
    c_definition += " };\n"

    return c_definition

c_definitions = "#ifndef PSV_H\n#define PSV_H\n\n"

for file in listdir(path):
    if not isfile(join(path, file)):
        continue

    if file[-4:] != ".pgm":
        continue

    c_definitions += gen_table(join(path, file))

c_definitions += "\n#endif"

with open("psv.h", "w") as f:
    f.write(c_definitions)