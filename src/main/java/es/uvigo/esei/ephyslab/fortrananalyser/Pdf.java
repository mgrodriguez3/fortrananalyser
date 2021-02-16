/*
 * Copyright (C) 2019 Michael Garvcía Rodríguez
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package es.uvigo.esei.ephyslab.fortrananalyser;

import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.color.Color;
import com.itextpdf.kernel.color.DeviceRgb;
import com.itextpdf.kernel.events.PdfDocumentEvent;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.geom.PageSize;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfDocumentInfo;
import com.itextpdf.kernel.pdf.PdfString;
import com.itextpdf.kernel.pdf.PdfViewerPreferences;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.kernel.pdf.WriterProperties;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.element.AreaBreak;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.AreaBreakType;
import com.itextpdf.layout.property.TextAlignment;

import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

public class Pdf {
    private static final com.itextpdf.kernel.color.Color MAIN_HEADER_COLOR = new DeviceRgb(0, 130, 130);
    private static final com.itextpdf.kernel.color.Color HEADER_COLOR = new DeviceRgb(0, 69, 69);
    private static final com.itextpdf.kernel.color.Color SECTION_COLOR = new DeviceRgb(207, 106, 11);
    private static final com.itextpdf.kernel.color.Color RESULT_COLOR = new DeviceRgb(38, 50, 61);
    private static final com.itextpdf.kernel.color.Color SUBSECTION_COLOR = new DeviceRgb(11, 136, 207);
    private static final com.itextpdf.kernel.color.Color FINAL_SCORE_COLOR = new DeviceRgb(77, 135, 133);
    private static final String DATE_FORMAT = "EEEE, dd MMMM yyyy HH:mm:ss";
    private static final String AUTHOR = "Michael García Rodríguez";
    private static final String FORTRANANALYSER_ICON
            = Pdf.class.getResource("fortranAnalyser.png").toString();
    private static final String PDF_TITLE = "FortranAnalyser: Quality report";
    private static final String APP_NAME = "FortranAnalyser";
    private Document report;

    public void createPdf(String dest, Locale l) throws IOException {
        PdfFont fontPDF;
        fontPDF = loadPdfFont();
        PdfWriter writer = new PdfWriter(dest, new WriterProperties().addXmpMetadata());
        PdfDocument pdf = new PdfDocument(writer);
        report = new Document(pdf, PageSize.A4);
        report.setFont(fontPDF);

        PdfPageEvent pdfPageEvent = new PdfPageEvent(report);
        pdf.addEventHandler(PdfDocumentEvent.END_PAGE, pdfPageEvent);
        pdf.setTagged();
        pdf.getCatalog()
                .setLang(new PdfString("es"));
        pdf.getCatalog()
                .setViewerPreferences(
                        new PdfViewerPreferences().setDisplayDocTitle(true));
        PdfDocumentInfo info = pdf.getDocumentInfo();
        info.setTitle(PDF_TITLE);
        info.addCreationDate();
        info.setAuthor(AUTHOR);
        info.setTitle(PDF_TITLE);
        addReportCover(l);
        report.add(new AreaBreak(AreaBreakType.NEXT_PAGE));
    }

    private void addReportCover(Locale l) throws MalformedURLException {
        Paragraph par = new Paragraph();
        Date date = new Date();
        DateFormat hourdateFormat = new SimpleDateFormat(DATE_FORMAT, l);
        par.add(hourdateFormat.format(date)).setTextAlignment(TextAlignment.RIGHT);
        report.add(par);
        par = new Paragraph();
        Text title = new Text(APP_NAME);
        Image coverImage = new Image(ImageDataFactory.create(FORTRANANALYSER_ICON));
        coverImage.getAccessibilityProperties()
                .setAlternateDescription(APP_NAME);
        coverImage.setHeight(320);
        coverImage.setWidth(320);
        par.add(coverImage.setTextAlignment(TextAlignment.CENTER));
        par.add("\n");
        par.add(title.setFontSize(36).setFontColor(Color.DARK_GRAY)).setTextAlignment(TextAlignment.CENTER);
        par.add("\n");
        par.add(new Text("Quality report").setFontSize(36).setFontColor(Color.DARK_GRAY).setTextAlignment(TextAlignment.CENTER));
        par.add("\n\n\n\n\n\n\n\n\n\n\n\n");
        report.add(par);
    }

    public void addSubSection(String text) {
        Paragraph p = new Paragraph();
        Text t = new Text(text);
        p.add(t.setFontSize(16).setFontColor(FINAL_SCORE_COLOR));
        p.add("\n");
        report.add(p);
    }

    public void addSubSectionInBold(String text) {
        Paragraph p = new Paragraph();
        Text t = new Text(text);
        p.add(t.setFontSize(16).setFontColor(FINAL_SCORE_COLOR)).setBold();
        p.add("\n");
        report.add(p);
    }

    public void addSection(String section) {
        Paragraph p = new Paragraph();
        Text sect = new Text(section);
        p.add(sect.setFontSize(18).setFontColor(SECTION_COLOR));
        p.add("\n");
        report.add(p);
    }

    public void addResult(String result) {
        Paragraph p = new Paragraph();
        Text t = new Text(result);
        p.add(t.setFontSize(12).setFontColor(RESULT_COLOR));
        p.add("\n");
        report.add(p);
    }

    public void addScoreResult(String result) {
        Paragraph p = new Paragraph();
        Text t = new Text(result);
        p.add(t.setFontSize(12).setFontColor(RESULT_COLOR)).setBold();
        p.add("\n");
        report.add(p);
        report.add(new AreaBreak(AreaBreakType.NEXT_PAGE));
    }

    public void addFinalNote(String result) {
        Paragraph p = new Paragraph();
        p.setTextAlignment(TextAlignment.RIGHT);
        Text t = new Text(result);
        p.add(t.setFontSize(18).setFontColor(SUBSECTION_COLOR));
        p.add("\n");
        report.add(p);
    }

    public void addSummaryInformation(String result) {
        Paragraph p = new Paragraph();
        Text t = new Text(result);
        p.add(t.setFontSize(12).setFontColor(RESULT_COLOR)).setBold();
        p.add("\n");
        report.add(p);
    }

    public void closePDF() {
        report.close();
    }

    private static PdfFont loadPdfFont() throws IOException {
        Path tmpFile = Files.createTempFile("fa-arial", ".ttf");
        Files.copy(Pdf.class.getResourceAsStream("arial.ttf"), tmpFile, StandardCopyOption.REPLACE_EXISTING);

        return PdfFontFactory.createFont(tmpFile.toString());
    }

    private Cell configureHeaderCells(String headerText, int fontSize, com.itextpdf.kernel.color.Color colorText) {
        Cell headerCell = new Cell();
        headerCell.add(headerText);
        headerCell.setFontSize(fontSize);
        headerCell.setFontColor(colorText);
        headerCell.setBorder(Border.NO_BORDER);
        headerCell.setTextAlignment(TextAlignment.CENTER);

        return headerCell;
    }

    private Cell addLeftCell(String textCell) {
        Cell leftCell = new Cell();
        leftCell.add(textCell);

        return leftCell;
    }

    private Cell addRightCell(Double textCell) {
        Cell rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", textCell));
        rightCell.setTextAlignment(TextAlignment.CENTER);

        return rightCell;
    }

    public void addTableScores(List<Double> scores, ResourceBundle messages, int size, int color, int[] position) {
        Table table = new Table(2);
        Cell headerCellLeft;
        Cell headerCellRight;
        Cell leftCell;
        Cell rightCell;
        com.itextpdf.kernel.color.Color headerColor;
        String[] row = {"implicitNone_table",
                "ratio_table",
                "useNestedLoops_table",
                "CommentsBeginning_table",
                "CommentsVariables_table",
                "CommentsFunctions_table",
                "CommentsSubroutines_table",
                "CommentsControlStructures_table",
                "UseExit_table",
                "UseCycle_table",
                "cyclomaticComplexity"};

        if (color == 0) {
            headerColor = Pdf.MAIN_HEADER_COLOR;
        } else {
            headerColor = Pdf.HEADER_COLOR;
        }
        headerCellLeft = configureHeaderCells(messages.getString("headerLeft_table"), size, headerColor);
        table.addHeaderCell(headerCellLeft);
        headerCellRight = configureHeaderCells(messages.getString("headerRight_table"), size, headerColor);
        table.addHeaderCell(headerCellRight);

        for (int i = 0; i < row.length; i++) {
            leftCell = addLeftCell(messages.getString(row[i]));
            table.addCell(leftCell);

            rightCell = addRightCell(scores.get(position[i]));
            table.addCell(rightCell);
        }
        report.add(table);
    }

    public void addFinalSummary(List<Double> fileScores, List<String> fileNames, ResourceBundle messages) {
        Table table = new Table(2, true);
        Cell headerCellLeft;
        Cell headerCellRight;
        headerCellLeft = configureHeaderCells(messages.getString("headerLeftSummaryTable"), 15, MAIN_HEADER_COLOR);
        headerCellLeft.setKeepTogether(true);
        table.addHeaderCell(headerCellLeft);
        headerCellRight = configureHeaderCells(messages.getString("headerRightSummaryTable"), 15, MAIN_HEADER_COLOR);
        headerCellRight.setKeepTogether(true);
        table.addHeaderCell(headerCellRight);
        report.add(table);
        for (int i = 0; i < fileNames.size(); i++) {
            table.addCell(new Cell().setKeepTogether(true).add(new Paragraph(fileNames.get(i))).setTextAlignment(TextAlignment.LEFT));
            table.addCell(new Cell().setKeepTogether(true).add(new Paragraph(String.format(Locale.ROOT, "%.3f", fileScores.get(i)))).setTextAlignment(TextAlignment.CENTER));
        }
        table.complete();
    }

    public static Color getHEADERCOLOR() {
        return MAIN_HEADER_COLOR;
    }

    public static Color getHEADER2COLOR() {
        return HEADER_COLOR;
    }

    public static Color getSECTIONCOLOR() {
        return SECTION_COLOR;
    }

    public static Color getRESULTCOLOR() {
        return RESULT_COLOR;
    }

    public static Color getSUBSECTIONCOLOR() {
        return SUBSECTION_COLOR;
    }

    public static Color getFINALNOTECOLOR() {
        return FINAL_SCORE_COLOR;
    }

    public static String getICONFORTRANANALYSER() {
        return FORTRANANALYSER_ICON;
    }
}
