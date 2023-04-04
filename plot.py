##!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 20 12:20:55 2020

@author: gabrielbarbosa
"""
import numpy as np
import matplotlib
from matplotlib import pyplot as plt
from matplotlib import animation
import matplotlib.colors as mcolors
from scipy.interpolate import make_interp_spline, BSpline
#from PIL import Image
import os


#matplotlib.use("pgf")
#matplotlib.rcParams.update({
#    "pgf.texsystem": "pdflatex",
#    'font.family': 'serif',
#    'text.usetex': True,
#    'pgf.rcfonts': False,
#})
matplotlib.rcParams["font.size"] = 26
matplotlib.rcParams["axes.labelsize"] = 28
matplotlib.rcParams["xtick.labelsize"] = 26
matplotlib.rcParams["ytick.labelsize"] = 26
matplotlib.rcParams["legend.fontsize"] = 23
matplotlib.rcParams["legend.framealpha"] = 0.0
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.weight'] = 'bold'
plt.rcParams['font.serif'] = ['Arial']

n_size=1.3
plt.figure(figsize=(n_size*10,n_size*8))

result=np.loadtxt('5.xvg',unpack=True,skiprows=0)
xnew1 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl1 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth1 = spl1(xnew1)
plt.plot(xnew1,y_smooth1,'-',color='xkcd:blue',linewidth=2,label="N=5")

result=np.loadtxt('20.xvg',unpack=True,skiprows=0)
xnew2 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl2 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth2 = spl2(xnew2)
plt.plot(xnew2,y_smooth2,'-',color='xkcd:red',linewidth=2,label="N=20")

result=np.loadtxt('40.xvg',unpack=True,skiprows=0)
xnew7 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl7 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth7 = spl7(xnew7)
plt.plot(xnew7,y_smooth7,'-',color='xkcd:orange',linewidth=2,label="N=40")

result=np.loadtxt('60.xvg',unpack=True,skiprows=0)
xnew3 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl3 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth3 = spl3(xnew3)
plt.plot(xnew3,y_smooth3,'-',color='xkcd:purple',linewidth=2,label="N=60")

result=np.loadtxt('100.xvg',unpack=True,skiprows=0)
xnew4 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl4 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth4 = spl4(xnew4)
plt.plot(xnew4,y_smooth4,'-',color='xkcd:green',linewidth=2,label="N=100")

result=np.loadtxt('120.xvg',unpack=True,skiprows=0)
xnew5 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl5 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth5 = spl5(xnew5)
plt.plot(xnew5,y_smooth5,'-',color='xkcd:pink',linewidth=1.5,label="N=120")

result=np.loadtxt('240.xvg',unpack=True,skiprows=0)
xnew6 = np.linspace(result[0,:].min(),result[0,:].max(), 200)
spl6 = make_interp_spline(result[0,:],result[1,:], k=3)
y_smooth6 = spl6(xnew6)
plt.plot(xnew6,y_smooth6,'-',color='xkcd:black',linewidth=6,label="N=240")



plt.text(10,8,"ADF C18-C19-C20")
plt.ylim((0,2))
plt.xlim((0,1.00))  
plt.xlabel(r"r(nm)",fontweight='bold')
plt.ylabel(r"g(r)",fontweight='bold')
plt.legend()
plt.savefig('C_O.jpg')
plt.clf()


