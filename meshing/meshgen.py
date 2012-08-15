import sys
import re

# Read in the first line
def checkHeader():
    line = mesh.readline()
    if "JonMesh" not in line:
        sys.exit("Incorrect mesh header.")
    else:
        print 'File format: ' + 'JonMesh'
        print 'Version    : ' + str(re.findall(r'\d.\d+',line))
    return

# Figures out how many blocks we need to draw
def findBlocks():
    for line in mesh:
        if not line.strip():
            continue
        elif "blocks" in line:
            nBlock = re.findall(r'\d+',line)
            print ('Reading '+ str(nBlock) + ' blocks...')
            return nBlock
        else:
            sys.exit("Blocks not defined in file!")

# Checks what kind of block we have then calls it's respective routine
def checkObject():
    for line in mesh:
        if not line.strip():
            continue
        else:
            elemType[line.strip()]()
            return

# if the object is a rectangle then we draw the mesh for a rectangle
def rectangle():
    print 'Found a rectangle...'
    for line in mesh:
        if not line.strip():
            continue
        elif "origin" in line:
            origin = re.findall('\d+.\d+',line.strip('origin:').lstrip())
            print 'Origin is located at: ' + str(origin)
            continue
        elif "x:" in line:
            xLeng = readLine(line)
        elif "y:" in line:
            yLeng = readLine(line)

def square():
    pass

# Reads everything after the equals sign
def readLine(line):
    temp = line.strip
    

# These are the possible element types, they are defined in the format
# 'name' : calledRoutine
elemType = { 'rect' : rectangle,
             'sq'   : square    }

# Open the mesh file
mesh = open('cavity.in','r')

# Check the mesh type
status = checkHeader()

# See how many objects we're reading in
blocks = findBlocks()

# Find the first object
for n in blocks:
    status = checkObject()