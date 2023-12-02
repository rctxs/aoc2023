R = 12
G = 13
B = 14
def draw_possible(draw):
    colorDraws = draw.split(", ")
    r = 0
    g = 0
    b = 0
    for colorDraw in colorDraws:
        color = colorDraw.split(" ")[1]
        number = int(colorDraw.split(" ")[0])
        if color == "red":
            r = number
        elif color == "green":
            g = number
        elif color == "blue":
            b = number
    return all([r <= R, g <= G, b <= B])

def taskOne():
    with open("input01.txt") as f:
        id_sum = 0
        lines = f.readlines()
        for line in lines:
            line = line.split("\n")[0]
            id = int(line.split(": ")[0].split(" ")[1])
            game = line.split(": ")[1].split("; ")
            if all([draw_possible(draw) for draw in game]):
                id_sum = id_sum + id
        print(id_sum)
        
def power(game):
    r = 0
    g = 0
    b = 0
    for draw in game:
        colorDraws = draw.split(", ")
        for colorDraw in colorDraws:
            color = colorDraw.split(" ")[1]
            number = int(colorDraw.split(" ")[0])
            if color == "red" and number > r:
                r = number
            elif color == "green" and number > g:
                g = number
            elif color == "blue" and number > b:
                b = number
    return r * g * b
        

def taskTwo():
    with open("input02.txt") as f:
        power_sum = 0
        lines = f.readlines()
        for line in lines:
            line = line.split("\n")[0]
            id = int(line.split(": ")[0].split(" ")[1])
            game = line.split(": ")[1].split("; ")
            power_sum = power_sum + power(game)
        print(power_sum)
        

def main():
    taskOne()
    taskTwo()

if __name__ == "__main__":
    main()