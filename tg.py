import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

plt.rcParams["font.size"] = 26
plt.rcParams["axes.labelsize"] = 28
plt.rcParams["xtick.labelsize"] = 26
plt.rcParams["ytick.labelsize"] = 26
plt.rcParams["legend.fontsize"] = 24
plt.rcParams["legend.framealpha"] = 0.0
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.weight'] = 'bold'
plt.rcParams['font.serif'] = ['Arial']

n_size=1.3
plt.figure(figsize=(n_size*10,n_size*8))

# Define the two linear functions to fit to the data
def line1(x, a, b):
    return a*x + b

def line2(x, c, d):
    return c*x + d

# Define the glass transition temperature as the point where the two lines intersect
def intersection(line1, line2):
    a, b = line1
    c, d = line2
    x = (d - b) / (a - c)
    y = a * x + b
    return x, y

# Load data from file
data = np.loadtxt('aa.xvg')

# Extract the temperature and volume data from the loaded file
temperature = data[:,0]
volume = data[:,1]

# Define the temperature ranges to fit the two linear functions
low_temp_range = (temperature < 380)
high_temp_range = (temperature > 380)

# Fit the two linear functions to the low and high temperature ranges
popt1, pcov1 = curve_fit(line1, temperature[low_temp_range], volume[low_temp_range])
popt2, pcov2 = curve_fit(line2, temperature[high_temp_range], volume[high_temp_range])

# Find the intersection point of the two lines
Tg, Vg = intersection(popt1, popt2)

# Plot the scatter plot and the two fitted lines
plt.scatter(temperature, volume,c = "black", linewidths = 1, marker = "o", edgecolor = "black", s = 100)
plt.plot(temperature[low_temp_range], line1(temperature[low_temp_range], *popt1), 'r--', linewidth = 4)
plt.plot(temperature[high_temp_range], line2(temperature[high_temp_range], *popt2), 'r--', linewidth = 4)
plt.plot([Tg], [Vg], marker='o', markersize=15, color="red")
plt.xlabel(r"Temperature (K)",fontweight='bold')
plt.ylabel(r"Volume (nm$^3$)",fontweight='bold')
plt.show()

# Print the glass transition temperature
print("Glass transition temperature is {:.2f} K".format(Tg))

plt.ylim((120, 190))
plt.xlim((100,700))
plt.savefig('fit.jpg')
plt.clf()



