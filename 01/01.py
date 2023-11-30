def taskOne():
    with open("input01.txt") as f:
        file = f.read()
        print(file)

def taskTwo():
    with open("input01.txt") as f:
        lines = f.readlines()
        print(lines)

def main():
    taskOne()
    taskTwo()

if __name__ == "__main__":
    main()