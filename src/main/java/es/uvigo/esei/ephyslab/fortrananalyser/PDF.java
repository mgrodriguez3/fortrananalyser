/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
 /*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.color.Color;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfDocumentInfo;
import com.itextpdf.kernel.pdf.PdfString;
import com.itextpdf.kernel.pdf.PdfViewerPreferences;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.pdf.WriterProperties;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.TextAlignment;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class create the document PDF with the quality report
 *
 * @author Michael García Rodríguez
 * @version 1.9
 */
public class PDF {

    /**
     * the file with the report information.
     */
    private Document document;

    /**
     * the name of the author.
     */
    private final static String AUTHOR = "Michael García Rodríguez";

    /**
     * the font type of the document.
     */
    private final PdfFont PDF_FONT = loadPdfFont();

    /**
     * The icon of the application.
     */
    private final static String ICON_FORTRAN_ANALYSER
            = PDF.class.getResource("fortranAnalyser.png").toString();

    /**
     * the title of the document.
     */
    private final static String TITLE_PDF = "FortranAnalyser: Quality report";

    /**
     * the name of the application.
     */
    private final static String APP_NAME = "Fortran Analyser";

    /**
     * Method that create the cover from the report document.
     *
     * @param dest
     * @throws IOException
     */
    public void createPdf(String dest) throws IOException {

        PdfDocument pdf = new PdfDocument(new PdfWriter(dest, new WriterProperties().addXmpMetadata()));
        this.document = new Document(pdf);

        this.document.setFont(PDF_FONT);

        //Setting some required parameters
        pdf.setTagged();

        pdf.getCatalog()
                .setLang(new PdfString("es"));
        pdf.getCatalog()
                .setViewerPreferences(
                        new PdfViewerPreferences().setDisplayDocTitle(true));
        PdfDocumentInfo info = pdf.getDocumentInfo();

        info.setTitle(TITLE_PDF);

        info.addCreationDate();
        info.setAuthor(AUTHOR);
        info.setTitle(TITLE_PDF);

        Paragraph par = new Paragraph();
        Date date = new Date();
        DateFormat hourdateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

        par.add(hourdateFormat.format(date));

        this.document.add(par);

        Paragraph p = new Paragraph();
        Text title = new Text(APP_NAME);

        Image coverImage = new Image(ImageDataFactory.create(ICON_FORTRAN_ANALYSER));

        coverImage.getAccessibilityProperties()
                .setAlternateDescription("FortranAnalyser");
        coverImage.setHeight(320);
        coverImage.setWidth(320);

        p.add(coverImage.setTextAlignment(TextAlignment.CENTER));
        p.add("\n");

        p.add(title.setFontSize(36).setFontColor(Color.DARK_GRAY)).setTextAlignment(TextAlignment.CENTER);
        p.add("\n");

        p.add(new Text("Quality report").setFontSize(36).setFontColor(Color.DARK_GRAY).setTextAlignment(TextAlignment.CENTER));
        p.add("\n\n\n\n\n\n\n\n\n\n\n\n");

        this.document.add(p);

        Paragraph p2 = new Paragraph();
        p2.setTextAlignment(TextAlignment.RIGHT);

        p2.add(new Text(PDF.AUTHOR).setFontSize(11).setFontColor(Color.BLACK));

        this.document.add(p2);

    }

    /**
     * add a paragraph in the report document
     *
     * @param text the text to add in a paragraph
     * @throws IOException
     */
    public void addParagraph(String text) throws IOException {

        //Fonts need to be embedded
        Paragraph p = new Paragraph();
        Text t = new Text(text);

        p.add(t.setFontSize(12).setFontColor(Color.BLACK));
        p.add("\n");

        this.document.add(p);

    }

    /**
     * add a subsection in the report document
     *
     * @param text the name of the subsection
     * @throws IOException
     */
    public void addSubSection(String text) throws IOException {
        Paragraph p = new Paragraph();
        Text t = new Text(text);

        p.add(t.setFontSize(16).setFontColor(Color.BLACK));
        p.add("\n");

        this.document.add(p);

    }

    /**
     * add a section in the report document
     *
     * @param section
     * @throws IOException
     */
    public void addSection(String section) throws IOException {
        Paragraph p = new Paragraph();
        Text sect = new Text(section);

        p.add(sect.setFontSize(18).setFontColor(Color.GRAY));
        p.add("\n");

        this.document.add(p);
    }

    /**
     * add the result from a specific analysis
     *
     * @param result the text to insert as a result
     * @throws IOException
     */
    public void addResult(String result) throws IOException {
        Paragraph p = new Paragraph();
        Text t = new Text(result);

        p.add(t.setFontSize(12).setFontColor(Color.BLACK));
        p.add("\n");

        this.document.add(p);
    }

    /**
     * add the final note to the results from the PDF
     *
     * @param result
     * @throws IOException
     */
    public void addFinalNote(String result) throws IOException {
        Paragraph p = new Paragraph();
        p.setTextAlignment(TextAlignment.RIGHT);
        Text t = new Text(result);

        p.add(t.setFontSize(18).setFontColor(Color.DARK_GRAY));
        p.add("\n");

        this.document.add(p);
    }

    /**
     * Close the report document
     */
    public void closePDF() {
        this.document.close();
    }

    /**
     * this method created a new font to write in the pdf document
     *
     * @return the font selected
     */
    private static PdfFont loadPdfFont() {

        try {
            Path tmpFile = Files.createTempFile("fa-arial", ".ttf");
            Files.copy(PDF.class.getResourceAsStream("arial.ttf"), tmpFile, StandardCopyOption.REPLACE_EXISTING);

            return PdfFontFactory.createFont(tmpFile.toString());
        } catch (IOException ex) {
            Logger.getLogger(PDF.class.getName()).log(Level.SEVERE, null, ex);
            throw new RuntimeException(ex);
        }
    }

    /**
     * this methos add the summary score table
     *
     * @param scores
     * @param messages
     */
    public void addTableScore(ArrayList<Double> scores, ResourceBundle messages) {   
        
        Table table = new Table(2);
        table.addHeaderCell(messages.getString("headerLeft_table"));
        table.addHeaderCell(messages.getString("headerRight_table"));

        table.addCell(messages.getString("implicitNone_table"));
        table.addCell(scores.get(0).toString());
        table.addCell(messages.getString("percentLines_table"));
        table.addCell(scores.get(1).toString());
        table.addCell(messages.getString("useNestedLoops_table"));
        table.addCell(scores.get(2).toString());
        table.addCell(messages.getString("CommentsBeginning_table"));
        table.addCell(scores.get(3).toString());
        table.addCell(messages.getString("CommentsVariables_table"));
        table.addCell(scores.get(4).toString());
        table.addCell(messages.getString("CommentsFunctions_table"));
        table.addCell(scores.get(5).toString());
        table.addCell(messages.getString("CommentsSubroutines_table"));
        table.addCell(scores.get(6).toString());
        table.addCell(messages.getString("CommentsControlStructures_table"));
        table.addCell(scores.get(7).toString());
        table.addCell(messages.getString("UseExit_table"));
        table.addCell(scores.get(8).toString());
        table.addCell(messages.getString("UseCycle_table"));
        table.addCell(scores.get(9).toString());
         
        

        this.document.add(table);

    }

}
