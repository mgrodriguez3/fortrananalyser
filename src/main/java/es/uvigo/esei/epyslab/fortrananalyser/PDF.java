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
 *
 * @author michael
 */
public class PDF {

    public void createPdf(String dest) throws IOException {
        
    String DOG = "/home/michael/Descargas/Word Cloud(30).png";
    String FOX = "/home/michael/Descargas/wave.png";
    String FONT = "/home/michael/Descargas/FreeSans.ttf";

    PdfDocument pdf = new PdfDocument(new PdfWriter(dest, new WriterProperties().addXmpMetadata()));
    Document document = new Document(pdf);

    //Setting some required parameters
    pdf.setTagged ();

    pdf.getCatalog ()

    .setLang(new PdfString("en-US"));
    pdf.getCatalog ()
    .setViewerPreferences(
                new PdfViewerPreferences().setDisplayDocTitle(true));
        PdfDocumentInfo info = pdf.getDocumentInfo();

    info.setTitle (
    "iText7 PDF/UA example");
 
        //Fonts need to be embedded
        PdfFont font = PdfFontFactory.createFont(FONT, PdfEncodings.WINANSI, true);
    Paragraph p = new Paragraph();

    p.setFont (font);

    p.add (
    new Text("The quick brown "));
        Image foxImage = new Image(ImageDataFactory.create(FOX));
    //PDF/UA: Set alt text

    foxImage.getAccessibilityProperties ()

    .setAlternateDescription("Fox");
    p.add (foxImage);

    p.add (
    " jumps over the lazy ");
        Image dogImage = new Image(ImageDataFactory.create(DOG));
    //PDF/UA: Set alt text

    dogImage.getAccessibilityProperties ()

    .setAlternateDescription("Dog");
    p.add (dogImage);

    document.add (p);

    document.close ();
}

}
