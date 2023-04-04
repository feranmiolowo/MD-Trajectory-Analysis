import numpy as np
import os

# Number of files
num_files = 40

# Create a list to store the data from each file
all_data = []

# Loop through each file
for i in range(0, num_files):
    # Construct the file name
    file = 'rdf.{}.xvg'.format(i)
    # Load the data from the file into a numpy array
    data = np.loadtxt(file, usecols=(0,1), max_rows=55)
    # Store the data in the list
    all_data.append(data)

# Stack the data from each file along the first axis to form a 3D array
all_data = np.stack(all_data, axis=0)

# Calculate the average of the data across all files
mean_data = np.mean(all_data, axis=0)

# Print the mean data
np.savetxt('avg.xvg', mean_data)