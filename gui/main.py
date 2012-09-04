#! /usr/bin/env python
from Tkinter import *
from tkFileDialog import *
import tkMessageBox
import sys
import os
import readIn
import tkFont
from meshIn import *
try:
    import pyvtk
except ImportError:
    sys.exit("You must have pyVTK installed to use this program. http://http://code.google.com/p/pyvtk/")

class App(Frame):
    _shapes = ['rectangle','square','square(','rectangle(']
    _commands = ['mesh','blocks','JonMesh','X-origin','Y-origin','X-length','Y-length','Length','X-points','Y-points']
    
    def __init__(self, master):
        """Initialise the base class"""
        Frame.__init__(self,master)
        
        # Initialize the body of the window
        self.bodyInit(master)
        
        #Create the Menu base
        self.menuInit(master)
        
        # Syntax highlighting
        self.editor.bind("<space>", self.highlight)
        self.editor.bind("<Key>", self.highlight)
        self.editor.tag_configure("shape", foreground="blue", underline=False)
        self.editor.tag_configure("command", foreground="dark green")
    
    
    def bodyInit(self,master):
        # window properties
        master.wm_state("zoomed")
        self.master.title('SimpleMAC Mesh')
        
        # Text box
        self.editorFrame = Frame(borderwidth=1,relief="sunken")
        self.editor = Text(master)
        self.editor.pack(in_=self.editorFrame, side="left", fill="both", expand=True)
        self.editor.config(
        borderwidth = 0,
        background="white",
        highlightthickness=0,
        font="Courier 14",
        wrap=WORD,
        undo=True )
        self.editor.focus_set()
        self.editorFrame.pack(side="bottom", fill="both", expand=True)
        
        # scrollbar
        self.scroll = Scrollbar(orient="vertical", borderwidth = 1, command=self.editor.yview)
        self.scroll.pack(in_=self.editorFrame, side="right", fill=Y, expand=False)
        self.editor.configure(yscrollcommand=self.scroll.set)
    
    
    def menuInit(self,master):
        self.menu = Menu(self)
        self.master.config(menu=self.menu)
        
        # File menu pane
        self.fileMenu = Menu(self.menu)
        self.menu.add_cascade(label='File', menu=self.fileMenu)
        self.fileMenu.add_command(label='Open', command = self.fileOpen)
        self.fileMenu.add_command(label='Save as mesh...', command = self.fileSaveAs)
        self.fileMenu.add_command(label='Save as VTK...', command = self.fileSaveAsVTK)
        self.fileMenu.add_separator()
        self.fileMenu.add_command(label='Quit', command = master.quit)
        
        # preview menu
        self.previewMenu = Menu(self.menu)
        self.menu.add_cascade(label='Preview', menu=self.previewMenu)
        self.previewMenu.add_command(label='Start in VTK...', command = self.previewVtk)
        #self.previewMenu.add_command(label='Stop VTK...', command = self.vtkStop)
        
        #run menu
        self.runMenu = Menu(self.menu)
        self.menu.add_cascade(label='Run', menu = self.runMenu)
        self.runMenu.add_command(label='in Terminal...', command = self.run)
        
        # Help menu pane
        self.helpMenu = Menu(self.menu)
        self.menu.add_cascade(label='Help', menu=self.helpMenu)
        self.helpMenu.add_command(label='About', command = self.about)
    
    
    def run(self):
        contents = self.editor.get(1.0,END)
        
        try:
            exec contents
        except:
            tkMessageBox.showerror("Error!","This is not valid SimpleMesh format code!\
            \nRead the documentation!")
        else:
            try:
                self.vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( mesh.points, quad=mesh.connect))
                #self.vtk.tofile('tmp')
            except:
                tkMessageBox.showerror("Error!", "You appear to be missing the \'mesh\' variable")
    
    
    def about(self):
        tkMessageBox.showinfo("About", "Author: Jon Komperda\
                                      \nE-Mail: Komperda.Jon@gmail.com \
                                      \nWeb:    github.com/jonkomperda")
    
    
    def fileOpen(self):
        self.inFileName = askopenfilename(filetypes=[("SimpleMAC Mesh Input","*.in")])
        if self.inFileName == '':
            pass
        else:
            self.meshFile = open(self.inFileName,'r')
            self.text = open(self.inFileName).read()
            self.editor.delete(1.0,END)
            self.editor.insert(END, self.text)
            self.editor.mark_set(INSERT,1.0)
            #readIn.readMesh(self.meshFile) #probably not needed for new format
            self.highlight(Frame)
    
    
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
        self.run()
        try:
            self.vtk
        except NameError:
            self.vtk = None
        
        if self.vtk is None:
            tkMessageBox.showerror("Error","There is no data to save!")
        else:
            self.run()
            try:
                self.saveAsName = asksaveasfilename()
                self.vtk.tofile(self.saveAsName,'ascii')
            except:
                tkMessageBox.showerror("Error!","Issue writing VTK data to file.")
    
    def previewVtk(self):
        
        self.run()
        
        try:
            self.vtk.tofile('tmp')
        except:
            tkMessageBox.showerror("Error!", "Issue writing temporary mesh file to display.")
        
        import vtk
        
        # The source file
        file_name = "tmp.vtk"
        
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
        renderer.SetBackground(.1, .1, .1) # Set background 
        
        # add an axes indicator
        transform = vtk.vtkTransform()
        transform.Translate(-.5,-.5,0.0)
        axes = vtk.vtkAxesActor()
        axes.SetUserTransform(transform)
        renderer.AddActor(axes)
        
        # Create the RendererWindow
        renderer_window = vtk.vtkRenderWindow()
        renderer_window.AddRenderer(renderer)
        
        # Create the RendererWindowInteractor and display the vtk_file
        self.interactor = vtk.vtkRenderWindowInteractor()
        self.interactor.SetRenderWindow(renderer_window)
        
        # this is buggy
        #def exitCheck(obj,event):
        #    if obj.GetEventPending() != 0:
        #        obj.SetAbortRender(1)
        #
        #renderer_window.AddObserver("AbortCheckEvent",exitCheck)
        
        #self.interactor.Initialize()
        renderer_window.Render()
        #self.interactor.Start()
    
    
    def vtkStop(self):
        try:
            self.interactor
        except NameError:
            pass
        else:
            self.interactor.Stop()
    
    
    def highlight(self,master):
        index = self.editor.search(r'\s',"insert", backwards=True, regexp=True)
        if index == "":
            index = "1.0"
        else:
            index = self.editor.index("%s+1c" % index)
        
        word = self.editor.get(index, "insert")
        
        if word in self._shapes:
            self.editor.tag_add("shape", index, "%s+%dc" % (index, len(word)))
        elif word in self._commands:
            self.editor.tag_add("command", index,"%s+%dc" % (index, len(word)))
        else:
            self.editor.tag_remove("shape", index, "%s+%dc" % (index,len(word)))
            self.editor.tag_remove("command", index, "%s+%dc" % (index,len(word)))
    
        
    
    


if __name__ == "__main__":
    root = Tk()
    app = App(root)
    
    root.mainloop()
    