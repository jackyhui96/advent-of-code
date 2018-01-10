with open('../inputs/day1_data.txt') as opened_file:
    data = opened_file.readline().rstrip()

def calc_captcha(offset, string):
    length = len(string)
    total = 0
    for i in range(0, length):
        current_value = string[i]
        target_index = (i + offset) % length
        if current_value == string[target_index]:
            total += int(current_value)
    return total

part1 = calc_captcha(1, data)
part2 = calc_captcha(int(len(data)/2), data)

print((part1, part2))