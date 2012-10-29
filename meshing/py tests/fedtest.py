
class testClass():
    def __init__(self,var):
        self.var = var
    
    def printer(self):
        print str(self.var)
    
    def __add__(self,right):
        return self.var + right.var
    
    def __sub__(self,other):
        return self.var - other.var
        

if __name__ == '__main__':
    a = testClass(5)
    b = testClass(2)
    
    print str(a+b)
    print str(a-b)