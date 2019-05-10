/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser.tests;

import com.itextpdf.kernel.color.DeviceRgb;
import es.uvigo.esei.ephyslab.fortrananalyser.PDF;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author miki
 */
public class PDFJUnitTest {
    
    public PDFJUnitTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

    // TODO add test methods here.
    // The methods must be annotated with annotation @Test. For example:
    //
    // @Test
    // public void hello() {}
    
    @Test
    public void testHEADER_COLOR(){
        assertEquals(new DeviceRgb(0, 130, 130), PDF.getHEADER_COLOR());
    }
    
    @Test
    public void testHEADER_2_COLOR(){
        assertEquals(new DeviceRgb(0, 69, 69), PDF.getHEADER_2_COLOR());
    }
    
    @Test
    public void testFinal_note_color(){
        assertEquals(new DeviceRgb(77, 135, 133), PDF.getFINAL_NOTE_COLOR());
    }
    
    @Test
    public void testIcon(){
        assertEquals(PDF.class.getResource("fortranAnalyser.png").toString(), PDF.getICON_FORTRAN_ANALYSER());
    }
    
    @Test
    public void testResult_COLOR(){
        assertEquals(new DeviceRgb(38, 50, 61), PDF.getRESULT_COLOR());
    }
    
    @Test
    public void testSection_COLOR(){
        assertEquals(new DeviceRgb(207, 106, 11), PDF.getSECTION_COLOR());
    }
    
    @Test
    public void testSub_section_COLOR(){
        assertEquals(new DeviceRgb(11, 136, 207), PDF.getSUB_SECTION_COLOR());
    }
}
