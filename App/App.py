import imp
import os
import tkinter as tk
import tkinter.ttk as ttk
import tkinter.messagebox as messagebox
import numpy as np

from App.Plot import MaxwellPlot

class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Maxewell viewer")
        self.minsize(1000, 750)

        self.vclt_start = 10
        self.vclt_end = 150
  
        self.vclt_var = tk.DoubleVar(master = self, value=self.vclt_start)
        self.vclt_var.trace_add('write', self.update_vars)

        self.vclt_slider = tk.Scale(master=  self, 
                                   variable = self.vclt_var,
                                   orient = tk.HORIZONTAL,
                                   label='vclt scale',
                                   tickinterval= (self.vclt_end-self.vclt_start)/7,
                                   from_= self.vclt_start,
                                   to= self.vclt_end, 
                                   resolution= 1, #(self.vclt_end-self.vclt_start)/n, 
                                   length = 250 )
        self.vclt_slider.grid(row=1, column=0, padx=5, pady=5,sticky=tk.N + tk.S + tk.E + tk.W)   

        self.enorm_start = 10
        self.enorm_end = 150
  
        self.enorm_var = tk.DoubleVar(master = self, value=self.enorm_start)
        self.enorm_var.trace_add('write', self.update_vars)

        self.enorm_slider = tk.Scale(master=  self, 
                                   variable = self.enorm_var,
                                   orient = tk.HORIZONTAL,
                                   label='enorm scale',
                                   tickinterval= (self.enorm_end-self.enorm_start)/7,
                                   from_= self.enorm_start,
                                   to= self.enorm_end, 
                                   resolution= 1, #(self.enorm_end-self.enorm_start)/n, 
                                   length = 250 )
        self.enorm_slider.grid(row=2, column=0, padx=5, pady=5,sticky=tk.N + tk.S + tk.E + tk.W)           

        self.distrib = {'v':[], 'f_classic':[], 'f_ext':[]}

        self.plot = MaxwellPlot(self, self.distrib )
        self.plot.grid(row=3, column=0, padx=5, pady=5,sticky=tk.N + tk.S + tk.E + tk.W) 
    
        self.columnconfigure(0, weight=1)
        self.protocol("WM_DELETE_WINDOW", self.on_closing)

    def update_vars(self, var, indx, mode):
        print(self.vclt_var.get())
        print(self.enorm_var.get())
        self.distrib = {'v':[], 'f_classic':[], 'f_ext':[]}
        for i in range(0,200):
            x = 2*np.pi*i/200
            self.distrib['v'].append(x)
            self.distrib['f_classic'].append(np.sin(x*self.vclt_var.get()/20))
            self.distrib['f_ext'].append(np.cos(x*self.enorm_var.get()/20))
        self.plot.update(self.distrib)
            
    def on_closing(self):
        if messagebox.askokcancel("Quit", "Do you want to quit?"):
            #self.controller.destroy()
            #Storage().close()
            self.destroy()
            


