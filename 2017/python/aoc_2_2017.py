instructions = []

with open('../inputs/day2_data.txt') as opened_file:
    for line in opened_file:
        instructions.append([int(i) for i in line.split()])

def calc_total_difference():
    total = 0
    for row in instructions:
        largest = max(row)
        smallest = min(row)
        difference = largest - smallest
        total += difference
    return total


print(calc_total_difference())