import numpy as np

# Set the maximum number of rows to read from each file
maxrows = 834

# Create an array to accumulate the distances for each row
dist_sum = np.zeros(maxrows)

# Loop over the file indices 
for i in range(1, 50, 2):

    # Open the current and next files for reading
    with open(f"{i}.xvg", "r") as file1, open(f"{i+1}.xvg", "r") as file2:

        # Read the data from the current and next files
        dist1 = np.loadtxt(file1, usecols=(1, 2, 3), max_rows=maxrows)
        dist2 = np.loadtxt(file2, usecols=(1, 2, 3), max_rows=maxrows)

        # Resultant distance
        dist = np.sqrt((dist1[:, 0] - dist2[:, 0])**2 +
                       (dist1[:, 1] - dist2[:, 1])**2 +
                       (dist1[:, 2] - dist2[:, 2])**2)

        # Add the distances to the accumulator array
        dist_sum += dist

# Calculate the average distance for each row
avg_dist = dist_sum / 25

with open("result.xvg", "w") as outfile:
    for i in range(maxrows):
        outfile.write(f"{i+1}\t{avg_dist[i]:.6f}\n")
