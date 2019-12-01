package com.xujunjie.aoc;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.Test;

/**
 * Unit test for Day1 of AOC.
 */
public class Day2Test {

    private Stream<String[]> puzzleInput;
    private Day2 testInstance;

    @Before
    public void setup() {
        testInstance = new Day2();
        // Get the input file path relative to the java project
        String[] pathParts = System.getProperty("user.dir").split("java");
        Path path = Paths.get(pathParts[0] + "inputs/day2_data.txt");

        try {
            Stream<String[]> stream = Files.lines(path)
                .map(s -> s.split("\\W+"));
            puzzleInput = stream;
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Test
    public void testDay2_case1() {
        assertThat(testInstance.main1(puzzleInput), is(51139));
    }

    @Test
    public void testDay2_case2() {
        assertThat(testInstance.main2(puzzleInput), is(272));
    }
}