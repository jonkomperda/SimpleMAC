import sys
import re
import pyvtk
import numpy as ar

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
            nBlock = getLineValInt(line)
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
    global xMin,yMin,xLeng,yLeng
    print 'Found a rectangle...'
    for line in mesh:
        if not line.strip():
            continue
        elif "X-origin" in line:
            xMin = getLineValDouble(line)
            continue
        elif "Y-origin" in line:
            yMin = getLineValDouble(line)
        elif "X-length" in line:
            xLeng = getLineValDouble(line)
        elif "Y-length" in line:
            yLeng = getLineValDouble(line)
        elif "X-points" in line:
            xP = getLineValInt(line)
        elif "Y-points" in line:
            yP = getLineValInt(line)
        elif "end" and "rect" in line:
            createRect(xMin,yMin,xLeng,yLeng,xP,yP)

def square():
    pass

def createRect(xMin,yMin,xLeng,yLeng,xP,yP):
    global structure
    print 'creating a rectangle..' 
    dx = xLeng / (xP-1)
    dy = yLeng / (yP-1)
    xTot = xMin + xLeng + dx
    yTot = yMin + yLeng + dy
    x = ar.arange(xMin,xTot,dx)
    y = ar.arange(yMin,yTot,dy)
    print x
    print y
    
    ps = [(xp,yp,zp) for xp in x for yp in y for zp in [0]]
    
    structure = pyvtk.PolyData(points=ps)

# Strip all unwanted line data and return the value
def getLineVal(line):
    temp = line.partition('=')[2]
    value = temp.strip().rstrip().lstrip()
    return value

# Returns a stripped int value from the line
def getLineValInt(line):
    value = getLineVal(line)
    return int(value)

# Returns a stripped float value from the line
def getLineValDouble(line):
    value = getLineVal(line)
    return float(value)
    

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
for n in range(0,blocks):
    status = checkObject()

#write the vtk
vtk = pyvtk.VtkData(structure)
vtk.tofile('testOut','ascii')