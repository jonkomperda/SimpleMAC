import sys
from os import rename, listdir
import re
import pyvtk
import numpy as ar
from Tkinter import Tk
from tkFileDialog import askopenfilename
from tkFileDialog import asksaveasfilename as savename
import subprocess as shell

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
    print 'Shape:   Found a rectangle...'
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
        elif "bound" in line:
            continue
        elif "Points" in line:
            lineError(line)
            sys.exit("Keyword error!")
            sys.exit("You must specify both x and y points")
        elif "Length" in line:
            lineError(line)
            sys.exit("Keyword error!")
            sys.exit("You must specify both x and y lenghts")
        else:
            lineError(line)
            sys.exit("Invalid keyword specified...")

def square():
    print 'Shape:   Found a square...'
    for line in mesh:
        if not line.strip():
            continue
        elif "X-origin" in line:
            xMin = getLineValDouble(line)
            continue
        elif "Y-origin" in line:
            yMin = getLineValDouble(line)
            continue
        elif "Length" in line:
            leng = getLineValDouble(line)
            continue
        elif 'X-points' in line:
            nP = getLineValInt(line)
            continue
        elif "end" and "sq" in line:
            createRect(xMin,yMin,leng,leng,nP,nP)
        elif "bound" in line:
            continue
        elif "X-length" or "Y-length" in line:
            lineError(line)
            sys.exit("Keyword error!")
            sys.exit("A square has equal length sides... =P")
        else:
            lineError(line)
            sys.exit("Invalid keyword specified...")
            

def createRect(xMin,yMin,xLeng,yLeng,xP,yP):
    global structure
    print 'creating a rectangle..' 
    dx = xLeng / (xP-1)
    dy = yLeng / (yP-1)
    xTot = xMin + xLeng + dx*0.1
    yTot = yMin + yLeng + dy*0.1
    x = ar.arange(xMin,xTot,dx)
    y = ar.arange(yMin,yTot,dy)
    print x
    print y
    
    ps = [(xp,yp,zp) for yp in y for xp in x for zp in [0]]
    
    structure = pyvtk.StructuredGrid([xP,yP,1],ps)

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

def lineError(line):
    print "\n\nError in line: "+line
    
# These are the possible element types, they are defined in the format
# 'name' : calledRoutine
elemType = { 'rect' : rectangle,
             'sq'   : square    }

# Open the mesh file
Tk().withdraw()
filename = askopenfilename()
#filename = 'cavity.in'
mesh = open(filename,'r')

# Check the mesh type
status = checkHeader()

# See how many objects we're reading in
blocks = findBlocks()

# Find the first object
for n in range(0,blocks):
    status = checkObject()

#write the vtk
tempFileName = 'temp'
vtk = pyvtk.VtkData(structure)

#print vtk
vtk.tofile(tempFileName,'ascii')

#clean the mesh file
saveFileName = savename()
tempFile = open(tempFileName+'.vtk','r')
outFile = open(saveFileName+'.vtk','w+')
for line in tempFile:
      if 'int' and 'POINTS' in line:
            line = line.replace('int','float')
            outFile.write(line)
            #print line
      else:
            outFile.write(line)
            #print line

tempFile.close()
outFile.close()
