# Read input data
with open("mapping.ndx", "r") as f_in:
    input_data = f_in.readlines()

input_data = [line.strip() for line in input_data]

with open("output.txt", "w") as f_out:
    for line in input_data:
        f_out.write(line + "\n")

for i in range(50):
    # Write updated data to temporary list
    updated_data = []
    for j, line in enumerate(input_data):
        if line.startswith("[ B"):
           
            updated_data.append(f"[ B{j} ]")
        else:
            
            numbers = line.split()
            updated_numbers = [str(int(n) + 242) for n in numbers]
            updated_data.append("\t".join(updated_numbers))
    
    # Write updated data to output file
    with open("output.txt", "a") as f_out:
        for line in updated_data:
            f_out.write(line + "\n")
    
    input_data = updated_data
