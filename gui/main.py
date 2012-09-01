#! /usr/bin/env python
from Tkinter import *
from tkFileDialog import *
import tkMessageBox
import sys
import os
import readIn
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
        # window properties
        master.title("SimpleMAC Mesh")
        master.wm_state("zoomed")
        
        self.editor = Text(master)
        self.editor.pack(fill=Y,expand=1)
        self.editor.config(
            borderwidth = 0,
            font="Courier 13",
            wrap=WORD,
            undo=True
            )
        self.editor.focus_set()
        
        self.master.title('SimpleMAC')
        #self.configure(height=200,width=200)
        #self.grid(padx=15, pady=15,sticky=N+S+E+W)
        

        
    
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
        
        # preview menu
        self.previewMenu = Menu(self.menu)
        self.menu.add_cascade(label='Preview', menu=self.previewMenu)
        self.previewMenu.add_command(label='in VTK...', command = self.previewVtk)
    
    def about(self):
        tkMessageBox.showinfo("About", "Author: Jon Komperda\
                                      \nE-Mail: Komperda.Jon@gmail.com \
                                      \nWeb:    github.com/jonkomperda")
    
    def fileOpen(self):
        self.inFileName = askopenfilename(filetypes=[("SimpleMAC Mesh Input","*.in")])
        self.meshFile = open(self.inFileName,'r')
        self.text = open(self.inFileName).read()
        self.editor.delete(1.0,END)
        self.editor.insert(END, self.text)
        self.editor.mark_set(INSERT,1.0)
        readIn.readMesh(self.meshFile)
    
    def fileSaveAs(self):
        try:
            self.saveAsName = asksaveasfilename()
            if '.in' in self.saveAsName:
                self.writeFile = open(self.saveAsName,'w')
            else:
                self.writeFile = open(str(self.saveAsName) + '.in','w')
            self.text = self.editor.get(1.0,END)
            self.writeFile.write(self.text.rstrip())
            self.writeFile.write("\n")
        finally:
            self.writeFile.close()
    
    def fileSaveAsVTK(self):
        try:
            vtk
        except NameError:
            vtk = None
        
        if vtk is None:
            tkMessageBox.showerror("Error","There is no data to save!")
        else:
            self.saveAsName = asksaveasfilename()
            vtk.tofile(self.saveAsName,'ascii')
            
    def previewVtk(self):
        import vtk

        # The source file
        file_name = "newTest.vtk"

        # Read the source file.
        reader = vtk.vtkUnstructuredGridReader()
        reader.SetFileName(file_name)
        reader.Update() # Needed because of GetScalarRange
        output = reader.GetOutput()
        scalar_range = output.GetScalarRange()

        # Create the mapper that corresponds the objects of the vtk file
        # into graphics elements
        mapper = vtk.vtkDataSetMapper()
        mapper.SetInput(output)
        mapper.SetScalarRange(scalar_range)

        # Create the Actor
        actor = vtk.vtkActor()
        actor.SetMapper(mapper)

        # Create the Renderer
        renderer = vtk.vtkRenderer()
        renderer.AddActor(actor)
        renderer.SetBackground(1, 1, 1) # Set background to white

        # Create the RendererWindow
        renderer_window = vtk.vtkRenderWindow()
        renderer_window.AddRenderer(renderer)

        # Create the RendererWindowInteractor and display the vtk_file
        interactor = vtk.vtkRenderWindowInteractor()
        interactor.SetRenderWindow(renderer_window)
        interactor.Initialize()
        interactor.Start()

if __name__ == "__main__":
    root = Tk()
    app = App(root)
    
    root.mainloop()
    