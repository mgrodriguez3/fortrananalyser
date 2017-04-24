/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.epyslab.fortrananalyser;

import com.itextpdf.io.font.PdfEncodings;
import com.itextpdf.io.image.ImageDataFactory;
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
import java.io.IOException;

/**
 * This class create the document PDF with the quality report
 * @author michael
 */
public class PDF {

    public void createPdf(String dest) throws IOException {

        String COVER = "./img/ephyslab.png";
        String FOX = "/home/michael/Descargas/wave.png";
        String FONT = "/home/michael/Descargas/FreeSans.ttf";

        PdfDocument pdf = new PdfDocument(new PdfWriter(dest, new WriterProperties().addXmpMetadata()));
        Document document = new Document(pdf);

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

        //Fonts need to be embedded
        PdfFont font = PdfFontFactory.createFont(FONT, PdfEncodings.WINANSI, true);
        Paragraph p = new Paragraph();

        p.setFont(font);
        p.setFontSize(40);

        p.add(
                new Text("FortranAnalyser: Quality report"));
        Image coverImage = new Image(ImageDataFactory.create(COVER));

        coverImage.getAccessibilityProperties()
                .setAlternateDescription("Fox");
        coverImage.setHeight(150);
        coverImage.setWidth(150);
        coverImage.setAutoScaleWidth(true);
        
        p.add(coverImage);

        p.setFontSize(12);
        p.add(
                " jumps over the lazy ");
        Image dogImage = new Image(ImageDataFactory.create(COVER));

        dogImage.getAccessibilityProperties()
                .setAlternateDescription("Dog");
        p.add(dogImage);

        document.add(p);

        document.close();
    }

}
