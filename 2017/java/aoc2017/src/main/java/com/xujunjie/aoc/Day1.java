package com.xujunjie.aoc;

import java.util.stream.IntStream;

public class Day1 {

    /**
     * Returns the total of all digits in the input string that are equal to the digit at the offset index.
     * Input string is treated as a circular data structure i.e. indices past its length will wrap round to the beginning.
     */
    int calcCapatcha(String input, int offset) {
        int length = input.length();
        int total = IntStream.range(0, length)
            .map(i -> compareTo(i, offset, input, length))
            .sum();
        return total;
    }

    /**
     * Returns the digit from input at the given index if it is equal to the digit at the offset index.
     */
    int compareTo(int index, int offset, String string, int length) {
        int currentValue = (int) string.charAt(index) - 48;
        int targetIndex = (index + offset) % length;
        int targetValue = (int) string.charAt(targetIndex) - 48;
        return currentValue == targetValue ? currentValue : 0;
    }
}
