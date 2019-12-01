package com.xujunjie.aoc;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day2 {

    int main1(Stream<String[]> input) {
        IntStream checksum = input.mapToInt(line -> {
            Integer[] sortedArray = Arrays.stream(line)
                .map(s -> Integer.valueOf(s))
                .sorted()
                .toArray(Integer[]::new);
            return sortedArray[sortedArray.length-1] - sortedArray[0];
        });
        return checksum.sum();
    }

    int main2(Stream<String[]> input) {
        IntStream checksum = input.mapToInt(line -> {
            List<Integer> list = Arrays.stream(line)
                .map(s -> Integer.valueOf(s))
                .collect(Collectors.toList());
            int result = list.stream()
                .mapToInt(n -> divisible_values_sum(n, list))
                .sum();

            return result;
        });
        return checksum.sum();
    }

    Integer divisible_values_sum(Integer currentVal, List<Integer> list) {
        IntStream divisibleValues = list.stream().mapToInt(x -> {
            int remainder = currentVal % x;
            int quotient = currentVal / x;
            return x != currentVal && remainder == 0 ? quotient : 0; 
        });
        return divisibleValues.sum();
    }
}
