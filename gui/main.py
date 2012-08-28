#! /usr/bin/env python
from Tkinter import *
import tkMessageBox

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
        
        self.helpMenu = Menu(self.menu)
        self.menu.add_cascade(label='Help', menu=self.helpMenu)
        self.helpMenu.add_command(label='About', command= self.about)
    
    def about(self):
        tkMessageBox.showinfo("About", "Author: Jon Komperda\
                                      \nE-Mail: Komperda.Jon@gmail.com \
                                      \nWeb:    github.com/jonkomperda")          


if __name__ == "__main__":
    root = Tk()
    app = App(root)
    root.mainloop()