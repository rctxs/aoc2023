class Number():
    def __init__(self, number_str):
        self.number_str = number_str
        self.adjacent = False

    def isEmpty(self):
        return len(self.number_str) == 0
    
    def append(self, sub_str):
        self.number_str += sub_str
    
    def markAdjacent(self):
        self.adjacent = True

    def value(self):
        return int(self.number_str)
    
    def isAdjacent(self):
        return self.adjacent
    
    def __str__(self):
        return self.number_str

def taskOne():
    with open("input01.txt") as f:
        lines = f.readlines()

        # setup
        HEIGHT = len(lines)
        LENGTH = len(lines[0].strip())

        adjacent_fields = []
        for i in range(len(lines)):
            line = lines[i].strip()
            adjacent_fields.append([])
            for j in range(len(line)):
                adjacent_fields[i].append(False)
        
        for i in range(len(lines)):
            line = lines[i].strip()
            for j in range(len(line)):
                character = lines[i][j]
                if character not in ".0123456789":
                    for _i in range(i-1,i+2):
                        for _j in range(j-1, j+2):
                            if _i >= 0 and _i < HEIGHT and _j >= 0 and _j < LENGTH:
                                adjacent_fields[_i][_j] = True

        numbers = []
        next_number = None
        for i in range(len(lines)):
            line = lines[i].strip()
            if next_number != None and not next_number.isEmpty():
                numbers.append(next_number)
                next_number = Number("")
            next_number = Number("")
            for j in range(len(line)):
                character = lines[i][j]
                if character in "0123456789":
                    next_number.append(character)
                    if adjacent_fields[i][j]:
                        next_number.markAdjacent()
                else:
                    if not next_number.isEmpty():
                        numbers.append(next_number)
                        next_number = Number("")
        
        sum = 0
        for number in numbers:
            if number.isAdjacent():
                sum += number.value()
        print(sum)

class Gear():
    def __init__(self, i, j):
        self.numbers = []
        self.i = i
        self.j = j

    def append(self, number):
        if number not in self.numbers:
            self.numbers.append(number)
    
    def value(self):
        if len(self.numbers) == 2:
            return self.numbers[0].value() * self.numbers[1].value()
        else:
            return 0

    def __str__(self):
        return str(self.i) + "/" + str(self.j)

def taskTwo():
    with open("input02.txt") as f:
        lines = f.readlines()

        # setup
        HEIGHT = len(lines)
        LENGTH = len(lines[0].strip())
        
        gears = []
        adjacent_gear_fields = []
        for i in range(len(lines)):
            line = lines[i].strip()
            adjacent_gear_fields.append([])
            for j in range(len(line)):
                adjacent_gear_fields[i].append([])
        
        for i in range(len(lines)):
            line = lines[i].strip()
            for j in range(len(line)):
                character = lines[i][j]
                if character in "*":
                    gear = Gear(i, j)
                    gears.append(gear)
                    for _i in range(i-1,i+2):
                        for _j in range(j-1, j+2):
                            if _i >= 0 and _i < HEIGHT and _j >= 0 and _j < LENGTH:
                                adjacent_gear_fields[_i][_j].append(gear)

        next_number = None
        for i in range(len(lines)):
            line = lines[i].strip()
            next_number = Number("")
            for j in range(len(line)):
                character = lines[i][j]
                if character in "0123456789":
                    next_number.append(character)
                    for gear in adjacent_gear_fields[i][j]:
                        gear.append(next_number)

                else:
                    next_number = Number("")
        
        sum = 0
        for gear in gears:
            sum += gear.value()
                    
        print(sum)

def main():
    taskOne()
    taskTwo()

if __name__ == "__main__":
    main()