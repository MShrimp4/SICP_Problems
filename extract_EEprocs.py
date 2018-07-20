import sys
with open(sys.argv[1]) as f:
    for i in f:
        noSem = i.find(";")
        if not noSem == 0 :
            iv = i[:noSem]
            print("(list '%s %s)"%(iv,iv))
a = input("END")
