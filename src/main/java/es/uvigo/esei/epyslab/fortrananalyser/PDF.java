/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.epyslab.fortrananalyser;

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
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.TextAlignment;
import java.io.IOException;

/**
 * This class create the document PDF with the quality report
 *
 * @author michael
 * @version 1.0
 */
public class PDF {

    /**
     * Variables from the class
     */
    private Document document;

    /**
     * Method that create the cover from the report document
     *
     * @param dest
     * @throws IOException
     */
    public void createPdf(String dest) throws IOException {

        String COVER = "./img/ephyslab.png";

        PdfDocument pdf = new PdfDocument(new PdfWriter(dest, new WriterProperties().addXmpMetadata()));
        this.document = new Document(pdf);

        //Fonts need to be embedded
        PdfFont arial = PdfFontFactory.createFont("./Fonts/arial.ttf");

        //Setting some required parameters
        pdf.setTagged();

        pdf.getCatalog()
                .setLang(new PdfString("es"));
        pdf.getCatalog()
                .setViewerPreferences(
                        new PdfViewerPreferences().setDisplayDocTitle(true));
        PdfDocumentInfo info = pdf.getDocumentInfo();

        info.setTitle(
                "FortranAnalyser: Quality report");

        Paragraph p = new Paragraph();
        Text title = new Text("Fortran Analyser");

        Image coverImage = new Image(ImageDataFactory.create(COVER));

        coverImage.getAccessibilityProperties()
                .setAlternateDescription("Fox");
        coverImage.setHeight(350);
        coverImage.setWidth(350);

        p.add(coverImage.setTextAlignment(TextAlignment.CENTER));
        p.add("\n");

        p.add(title.setFont(arial).setFontSize(36).setFontColor(Color.DARK_GRAY)).setTextAlignment(TextAlignment.CENTER);
        p.add("\n");

        p.add(new Text("Quality report").setFont(arial).setFontSize(36).setFontColor(Color.DARK_GRAY).setTextAlignment(TextAlignment.CENTER));
        p.add("\n\n\n\n\n\n\n\n\n\n\n\n");

        this.document.add(p);

        Paragraph p2 = new Paragraph();

        p2.add(new Text("Michael García Rodríguez").setFont(arial).setFontSize(11).setFontColor(Color.BLACK).setTextAlignment(TextAlignment.RIGHT));

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
        PdfFont textFont = PdfFontFactory.createFont("./Fonts/arial.ttf");
        Paragraph p = new Paragraph();
        Text t = new Text(text);

        p.add(t.setFont(textFont).setFontSize(12).setFontColor(Color.BLACK));
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

        //Fonts need to be embedded
        PdfFont textFont = PdfFontFactory.createFont("./Fonts/arial.ttf");
        Paragraph p = new Paragraph();
        Text t = new Text(text);

        p.add(t.setFont(textFont).setFontSize(16).setFontColor(Color.BLACK));
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

        //Fonts need to embedded
        PdfFont textFont = PdfFontFactory.createFont("./Fonts/arial.ttf");
        Paragraph p = new Paragraph();
        Text sect = new Text(section);

        p.add(sect.setFont(textFont).setFontSize(18).setFontColor(Color.GRAY));
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

        //Fonts need to be embedded
        PdfFont textFont = PdfFontFactory.createFont("./Fonts/arial.ttf");
        Paragraph p = new Paragraph();
        Text t = new Text(result);

        p.add(t.setFont(textFont).setFontSize(12).setFontColor(Color.BLACK));
        p.add("\n");

        this.document.add(p);
    }

    /**
     * Close the report document
     */
    public void closePDF() {
        this.document.close();
    }

}
