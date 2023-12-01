def taskOne():
    with open("input01.txt") as f:
        lines = f.readlines()
        cali = []
        for line in lines:
            chars = []
            for c in line:
                if c in "0123456789":
                    chars.append(c)
            if len(chars) == 1:
                cali.append(int(chars[0]+chars[0]))

            else:
                cali.append(int(chars[0]+chars[-1]))
        sum = 0
        for s in cali:
            sum = sum + s  
                    
        print(sum)

def taskTwo():
    with open("input02.txt") as f:
        lines = f.readlines()
        cali = []
        for line in lines:
            tail = line
            chars = []
            while len(tail) > 0:
                if tail[0] in "0123456789":
                    chars.append(tail[0])
                else:
                    if tail.startswith("one"):
                        chars.append("1")
                    elif tail.startswith("two"):
                        chars.append("2")
                    elif tail.startswith("three"):
                        chars.append("3")
                    elif tail.startswith("four"):
                        chars.append("4")
                    elif tail.startswith("five"):
                        chars.append("5")
                    elif tail.startswith("six"):
                        chars.append("6")
                    elif tail.startswith("seven"):
                        chars.append("7")
                    elif tail.startswith("eight"):
                        chars.append("8")
                    elif tail.startswith("nine"):
                        chars.append("9")
                tail = tail [1::]

            print(chars)
            if len(chars) == 1:
                cali.append(int(chars[0]+chars[0]))
            else:
                cali.append(int(chars[0]+chars[-1]))
        sum = 0
        for s in cali:
            sum = sum + s                
                    
        print(sum)

def main():
    taskOne()
    taskTwo()

if __name__ == "__main__":
    main()