package com.xujunjie.aoc;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.Before;
import org.junit.Test;

/**
 * Unit test for Day1 of AOC.
 */
public class Day1Test {

    private String puzzleInput;
    private Day1 testInstance;

    @Before
    public void setup() {
        testInstance = new Day1();
        // Get the input file path relative to the java project
        String[] pathParts = System.getProperty("user.dir").split("java");
        Path path = Paths.get(pathParts[0] + "inputs/day1_data.txt");

        try {
            byte[] fileBytes = Files.readAllBytes(path);
            puzzleInput = new String(fileBytes).trim();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Test
    public void testDay1_case1() {
        assertThat(testInstance.calcCapatcha(puzzleInput, 1), is(1031));
    }

    @Test
    public void testDay1_case2() {
        int offset = puzzleInput.length() / 2;
        assertThat(testInstance.calcCapatcha(puzzleInput, offset), is(1080));
    }
}