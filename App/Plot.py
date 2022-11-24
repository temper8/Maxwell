import tkinter as tk
import tkinter.ttk as ttk

from matplotlib import cm
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import ( FigureCanvasTkAgg, NavigationToolbar2Tk)

class VerticalNavigationToolbar2Tk(NavigationToolbar2Tk):
   def __init__(self, canvas, window):
      super().__init__(canvas, window, pack_toolbar=False)

   # override _Button() to re-pack the toolbar button in vertical direction
   def _Button(self, text, image_file, toggle, command):
      b = super()._Button(text, image_file, toggle, command)
      b.pack(side=tk.TOP) # re-pack button in vertical direction
      return b

   # override _Spacer() to create vertical separator
   def _Spacer(self):
      s = tk.Frame(self, width=26, relief=tk.RIDGE, bg="DarkGray", padx=2)
      s.pack(side=tk.TOP, pady=5) # pack in vertical direction
      return s

   # disable showing mouse position in toolbar
   def set_message(self, s):
      pass


class MaxwellPlot(ttk.Frame):
    def __init__(self, master, distrib) -> None:
        super().__init__(master)  
        #self.fig, self.axs = plt.subplots(2, 2, figsize=(7, 6))
        self.fig = plt.figure(figsize=(8, 6.6))
        self.fig.suptitle(f'Maxewll distrib')
        self.ax = self.fig.subplots(1, 1)
        
        # профили токов
        self.f_classic,    = self.ax.plot(distrib['v'], distrib['f_classic'])
        self.f_ext, = self.ax.plot(distrib['v'], distrib['f_ext'])
        self.ax.set_title("Plh, Poh")
    

        self.canvas = FigureCanvasTkAgg(self.fig, self)
        self.canvas.draw()
        self.canvas.get_tk_widget().grid(row=0, column=1, sticky=tk.N + tk.S + tk.E + tk.W)

        
        tb = VerticalNavigationToolbar2Tk(self.canvas, self)
        tb.update()
        tb.grid(row=0, column=0, sticky=tk.N)        
        
        self.columnconfigure(1, weight=1)
        self.rowconfigure(0, weight=1)

    def update(self, distrib):
        self.fig.suptitle(f'Maxewll distrib')

        self.f_classic.set_xdata(distrib['v']) 
        self.f_classic.set_ydata(distrib['f_classic']) 
        self.f_ext.set_xdata(distrib['v']) 
        self.f_ext.set_ydata(distrib['f_ext']) 
        
        self.ax.relim()
        self.ax.autoscale_view(True,True,True)        
   
        self.canvas.draw()

    def destroy(self):
        if self.fig:
            plt.close(self.fig)
        super().destroy()   
