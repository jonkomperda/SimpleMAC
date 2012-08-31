import re
import os
import sys

############## Begin data handling
class readMesh:
    """
    This class reads a mesh file and stores the data
    """
    
    def __init__(self,theFile):
        
        self.elemType = {   'rectangle' :   self.rectangle,      \
                            'square'    :   self.square,         \
                            'unstruct'  :   self.unstructShape   }
        
        self.checkHeader(theFile)
        self.blocks     = self.findBlocks(theFile)
        self.shape = [0,0,0]
        for self.n in range(0,self.blocks):
            self.shape[self.n]  = self.getShape(theFile)
    
    # Read the first line for a file header
    def checkHeader(self,theFile):
        line = theFile.readline()
        if "JonMesh" not in line:
            sys.exit("Error: Incorrect mesh header.")
        else:
            print 'File format: ' + 'JonMesh'
            print 'Version    : ' + str(re.findall(r'\d.\d+',line))
        return
    
    # Get number of blocks from file being read
    def findBlocks(self,theFile):
        for line in theFile:
            if not line.strip():
                continue
            elif "!" in line:
                continue
            elif "blocks" in line:
                nBlock = self.getLineValInt(line)
                print ('Blocks     : '+ str(nBlock))
                return nBlock
            else:
                sys.exit("Error: Blocks not defined in file!")
    
    # Gets the shapes from the file
    def getShape(self,theFile):
        for line in theFile:
            if not line.strip():
                continue
            elif "!" in line:
                continue
            else:
                struct = self.elemType[line.strip()](theFile)
                return struct
                
    
    def rectangle(self,theFile):
        print "\nShape "+ str(self.n) +"    : Rectangle"
        
    def square(self,theFile):
        pass
    
    def unstructShape(self,theFile):
        pass
    
    # Returns a stripped value as a string
    def getLineVal(self,line):
        temp = line.partition('=')[2]
        value = temp.strip().rstrip().lstrip()
        return value
    
    # Returns a stripped int value from the line
    def getLineValInt(self,line):
        value = self.getLineVal(line)
        return int(value)
    
    # Returns a stripped float value from the line
    def getLineValDP(self,line):
        value = self.getLineVal(line)
        return float(value)
    
    # Tells you there's an error in the file and prints the line
    def lineError(line):
        print "\n\nError in line: "+line

if __name__ == "__main__":
    """
    For testing this file
    """
    meshFile = open('cavity.in','r')
    test = readMesh(meshFile)