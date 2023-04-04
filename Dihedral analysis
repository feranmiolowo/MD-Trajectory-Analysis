#Install the MDAnalysis package: pip install --upgrade MDAnalysis

import MDAnalysis as mda
from MDAnalysis.analysis.dihedrals import Dihedral
import numpy as np
import matplotlib.pyplot as plt

# Load PDB (trajectory) file into Universe object
u = mda.Universe('0_double_bond.pdb')

# Select the four atoms in the dihedral. The list of atoms is attached.
# You just need to change the atom names e.g. C007, C008 
atoms = u.select_atoms('resid 1 and (name C006 or name C007 or name C008 or name C009)')

# Print the names of the selected atoms. This is just to crosscheck the above command works
print(atoms.names)

# Calculate the dihedral angle
dihedral = Dihedral([atoms]).run()

print(f"The dihedral angle is: {float(dihedral.angles[0]):.2f} degrees.")

#If you wish to plot the dihedral distribution, remove the # below.

#bins = np.arange(-180, 180, 5)
#hist, bin_edges = np.histogram(dihedral.angles, bins=bins, density=True)
#bin_centers = 0.5 * (bin_edges[:-1] + bin_edges[1:])
#plt.plot(bin_centers, hist)
#plt.xlabel('Dihedral angle (degrees)')
#plt.ylabel('Probability')
#plt.savefig('dihedral.jpg')
