/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser.tests;

import es.uvigo.esei.ephyslab.fortrananalyser.TasksBar;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author michael
 */
public class TasksBarTests {

    private File file;
    private TasksBar tb;

    public TasksBarTests() {
        try {
            file = new File(getClass().getClassLoader().getResource("files/DISTANCE.f90").toURI());
            tb = new TasksBar();
        } catch (URISyntaxException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }


    @Test
    public void TestCalculateAverage() {
        int average = 0;

        average = 6 / 2;
        assertEquals(3, average);
    }

    @Test
    public void TestCheckFileExist() {

        assertTrue(file.exists());

    }

    @Test
    public void TestAnalyseNumberOfLines() {

        int numLines = 0;

        try {
            numLines = tb.analyseNumberOfLines(file.getPath());
        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(38, numLines);
    }

    @Test
    public void TestAnalyseUseImplicitNone() {

        try {

            assertTrue(tb.analyseUseImplicitNone(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    @Test
    public void TestAnalyseNumFunctions() {

        int numFunctions = 0;

        try {

            numFunctions = tb.analyseNumFunctions(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0, numFunctions);
    }

    @Test
    public void TestAnalyseNumCalls() {

        int numCalls = 0;

        try {

            numCalls = tb.analyseNumCalls(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0, numCalls);
    }

    @Test
    public void TestAnalyseNumComments() {

        int numComments = 0;

        try {

            numComments = tb.analyseNumComments(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(3, numComments);
    }

    @Test
    public void TestAnalyseUseCycle() {

        try {

            assertFalse(tb.analyseUseCycle(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseUseExit() {

        try {

            assertFalse(tb.analyseUseExit(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseNumberSubroutines() {

        int numSubroutines = 0;

        try {

            numSubroutines = tb.analyseNumberSubroutines(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0, numSubroutines);
    }

    @Test
    public void TestAnalyseNestedLoops() {

        try {

            assertTrue(tb.analyseNestedLoops(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseNumberOfDeclaredVariables() {

        int numDeclaredVariables = 0;

        try {

            numDeclaredVariables = tb.analyseNumberOfDeclaredVariables(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(12, numDeclaredVariables);
    }

    @Test
    public void TestAnalyseGoodCommentControlStructures() {

        try {

            assertFalse(tb.analyseGoodCommentControlStructures(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentSubroutines() {

        try {

            assertTrue(tb.analyseGoodCommentSubroutines(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentVariables() {

        try {

            assertFalse(tb.analyseGoodCommentedVariables(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentInitDoc() {

        try {

            assertFalse(tb.analyseGoodCommentInitDoc(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test
    public void TestAnalyseGoodCommentFunctions() {

        try {

            assertFalse(tb.analyseGoodCommentFunctions(file.getPath()));

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void TestGetDurationAnalyseException() throws Exception {

        TasksBar.getDurationAnalyse(-1);

    }

    @Test
    public void TestGetDurationAnalyse() {

        String expectedDate = "17 D 10 h 41 min 23 s 23700 ms";

        assertEquals(expectedDate, TasksBar.getDurationAnalyse(1507283723));
    }
    
    @Test
    public void TestAnalyseRatio(){
        double ratio = 0.0;

        try {

            ratio = tb.analyseRatio(file.getPath());

        } catch (IOException ex) {
            Logger.getLogger(TasksBarTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        assertEquals(0.0, ratio, 0.01);
    }
    
}
