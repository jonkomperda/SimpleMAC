#! /usr/bin/env python
from Tkinter import *
from tkFileDialog import *
import tkMessageBox
import sys
import os
try:
    import pyvtk
except ImportError:
    sys.exit("You must have pyVTK installed to use this program. http://http://code.google.com/p/pyvtk/")

class App(Frame):
    def __init__(self, master):
        """Initialise the base class"""
        Frame.__init__(self,master)
        
        # Initialize the body of the window
        self.bodyInit(master)
        
        #Create the Menu base
        self.menuInit(master)

        
    def bodyInit(self,master):
        self.master.title('SimpleMAC')
        self.configure(height=200,width=200)
        self.grid(padx=15, pady=15,sticky=N+S+E+W)
    
    def menuInit(self,master):
        self.menu = Menu(self)
        self.master.config(menu=self.menu)
        
        # File menu pane
        self.fileMenu = Menu(self.menu)
        self.menu.add_cascade(label='File', menu=self.fileMenu)
        self.fileMenu.add_command(label='Open', command = self.fileOpen)
        self.fileMenu.add_command(label='Save as...', command = self.fileSaveAs)
        self.fileMenu.add_separator()
        self.fileMenu.add_command(label='Quit', command = master.quit)
        
        # Help menu pane
        self.helpMenu = Menu(self.menu)
        self.menu.add_cascade(label='Help', menu=self.helpMenu)
        self.helpMenu.add_command(label='About', command = self.about)
    
    def about(self):
        tkMessageBox.showinfo("About", "Author: Jon Komperda\
                                      \nE-Mail: Komperda.Jon@gmail.com \
                                      \nWeb:    github.com/jonkomperda")
    
    def fileOpen(self):
        self.inFileName = askopenfilename(filetypes=[("SimpleMAC Mesh Input","*.in")])
        
    
    def fileSaveAs(self):
        try:
            vtk
        except NameError:
            vtk = None
        
        if vtk is None:
            tkMessageBox.showerror("Error","There is no data to save!")
        else:
            self.saveAsName = asksaveasfilename()
            vtk.tofile(self.saveAsName,'ascii')

if __name__ == "__main__":
    root = Tk()
    app = App(root)
    root.mainloop()