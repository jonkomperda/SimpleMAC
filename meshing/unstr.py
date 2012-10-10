class unstructShape:
    # Constructs an unstructured shape, usually the result of adding together other shapes
    def __init__(self,points, old1='none', old2='none'):
        print 'Shape: Creating an unstructured shape...'
        
        self.points = points
        
        if( old1 == 'none' and old2 == 'none' ):
            self.old1 = 0
            self.old2 = 0
        else:
            self.old1 = old1
            self.old2 = old2
        
        self.connect= self.connections(self.points)
    
    # Finds the element connections (*Needs to be improved, issues with gaps)
    def connections(self,points):
        connection = ()
        for i in range(0,len(points)-1):
            here    = points[i]
            next    = points[i+1]
            #see if the next point is the right point
            if (next[0] > here[0]) and (next[1] == here[1]):
                #print i
                for j in range(i+1,len(points)-1):
                    other   = points[j]
                    rightO  = points[j+1]
                    if ((other[0] == here[0]) and (rightO[0] > here[0]) and (rightO[1] > here[1])):
                        temp        = (i,i+1,j+1,j)
                        connection  = connection + temp
                        break
        #conList     = list(self.chopper(connection,4))
        
        # This condition is if we are making the shape for the first time
        if(self.old1 == 0 and self.old2 == 0):
            conList     = list(self.chopper(connection,4))
            return conList
        # This condition is if we have added two shapes and need to check for messed up connections
        else:
            conList     = list(self.chopper(connection,4))
            conList = self.cleanConnects(conList)
            conList     = list(self.chopper(conList,4))
            return conList
    
    # Checks if the connection is supposed to exist or if it was added by mistake
    def cleanConnects(self,cons):
        connection  = ()
        for item in cons:
            point1  = self.points[item[0]]
            point2  = self.points[item[1]]
            point3  = self.points[item[2]]
            point4  = self.points[item[3]]
            cond1   = False
            cond2   = False
            exists  = False
            
            if point1 in self.old1:
                if point2 in self.old1:
                    if point3 in self.old1:
                        if point4 in self.old1:
                            cond1 = True
            if point1 in self.old2:
                if point2 in self.old2:
                    if point3 in self.old2:
                        if point4 in self.old2:
                            cond2 = True
            
            if (cond1 == True or cond2 == True):
                connection = connection + item
        return connection
    
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    # Overloading of addition
    def __add__(self,other):
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points)
        return out
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out