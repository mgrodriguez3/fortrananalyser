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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * This class create the document PDF with the quality report
 *
 * @author Michael García Rodríguez
 * @version 1.9.2
 */
public class PDF {

    /**
     * Define the color of headers of the main table.
     */
    private static final com.itextpdf.kernel.color.Color HEADER_COLOR = new DeviceRgb(0, 130, 130);

    /**
     * Define the color of header of the table.
     */
    private static final com.itextpdf.kernel.color.Color HEADER_2_COLOR = new DeviceRgb(0, 69, 69);

    /**
     * Define the color of sections.
     */
    private static final com.itextpdf.kernel.color.Color SECTION_COLOR = new DeviceRgb(207, 106, 11);

    /**
     * Define the color of the results.
     */
    private static final com.itextpdf.kernel.color.Color RESULT_COLOR = new DeviceRgb(38, 50, 61);

    /**
     * Define the color of the subsections.
     */
    private static final com.itextpdf.kernel.color.Color SUB_SECTION_COLOR = new DeviceRgb(11, 136, 207);

    /**
     * Define the color of the text of the final score.
     */
    private static final com.itextpdf.kernel.color.Color FINAL_NOTE_COLOR = new DeviceRgb(77, 135, 133);

    /**
     * the file with the report information.
     */
    private Document document;

    /**
     * the name of the author.
     */
    private static final String AUTHOR = "Michael García Rodríguez";

    /**
     * The icon of the application.
     */
    private static final String ICON_FORTRAN_ANALYSER
            = PDF.class.getResource("fortranAnalyser.png").toString();

    /**
     * the title of the document.
     */
    private static final String TITLE_PDF = "FortranAnalyser: Quality report";

    /**
     * the name of the application.
     */
    private static final String APP_NAME = "FortranAnalyser";

    /**
     * Method that create the cover from the report document.
     *
     * @param dest path of the report
     * @param l local data
     * @throws IOException in case something wrong with intput/output file
     */
    public void createPdf(String dest, Locale l) throws IOException {

        PdfFont fontPDF;
        fontPDF = loadPdfFont();
        PdfWriter writer = new PdfWriter(dest, new WriterProperties().addXmpMetadata());
        PdfDocument pdf = new PdfDocument(writer);

        this.document = new Document(pdf, PageSize.A4);

        this.document.setFont(fontPDF);

        PageEvent evento = new PageEvent(this.document);

        pdf.addEventHandler(PdfDocumentEvent.END_PAGE, evento);

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

        DateFormat hourdateFormat = new SimpleDateFormat("EEEE, dd MMMM yyyy HH:mm:ss", l);

        par.add(hourdateFormat.format(date)).setTextAlignment(TextAlignment.RIGHT);

        this.document.add(par);

        Paragraph p = new Paragraph();
        Text title = new Text(APP_NAME);

        Image coverImage = new Image(ImageDataFactory.create(ICON_FORTRAN_ANALYSER));

        coverImage.getAccessibilityProperties()
                .setAlternateDescription(APP_NAME);
        coverImage.setHeight(320);
        coverImage.setWidth(320);

        p.add(coverImage.setTextAlignment(TextAlignment.CENTER));
        p.add("\n");

        p.add(title.setFontSize(36).setFontColor(Color.DARK_GRAY)).setTextAlignment(TextAlignment.CENTER);
        p.add("\n");

        p.add(new Text("Quality report").setFontSize(36).setFontColor(Color.DARK_GRAY).setTextAlignment(TextAlignment.CENTER));
        p.add("\n\n\n\n\n\n\n\n\n\n\n\n");

        this.document.add(p);
        this.document.add(new AreaBreak(AreaBreakType.NEXT_PAGE));

    }

    /**
     * add a paragraph in the report document
     *
     * @param text the text to add in a paragraph
     * @throws java.io.IOException in case something wrong with intput/output
     * file
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
     * @throws IOException in case something wrong with intput/output file
     */
    public void addSubSection(String text) throws IOException {
        Paragraph p = new Paragraph();
        Text t = new Text(text);

        p.add(t.setFontSize(16).setFontColor(FINAL_NOTE_COLOR));
        p.add("\n");

        this.document.add(p);

    }

    /**
     * add a subsection in the report document
     *
     * @param text the name of the subsection
     * @throws IOException in case something wrong with intput/output file
     */
    public void addSubSectionInBold(String text) throws IOException {
        Paragraph p = new Paragraph();
        Text t = new Text(text);

        p.add(t.setFontSize(16).setFontColor(FINAL_NOTE_COLOR)).setBold();
        p.add("\n");

        this.document.add(p);

    }

    /**
     * add a section in the report document
     *
     * @param section with the name of the section
     * @throws IOException in case something wrong with intput/output file
     */
    public void addSection(String section) throws IOException {
        Paragraph p = new Paragraph();
        Text sect = new Text(section);

        p.add(sect.setFontSize(18).setFontColor(SECTION_COLOR));
        p.add("\n");

        this.document.add(p);
    }

    /**
     * add the result from a specific analysis
     *
     * @param result the text to insert as a result
     * @throws IOException in case something wrong with intput/output file
     */
    public void addResult(String result) throws IOException {
        Paragraph p = new Paragraph();
        Text t = new Text(result);

        p.add(t.setFontSize(12).setFontColor(RESULT_COLOR));
        p.add("\n");

        this.document.add(p);
    }

    /**
     * add the result from a specific analysis
     *
     * @param result the text to insert as a result
     * @throws IOException in case something wrong with intput/output file
     */
    public void addScoreResult(String result) throws IOException {
        Paragraph p = new Paragraph();
        Text t = new Text(result);

        p.add(t.setFontSize(12).setFontColor(RESULT_COLOR)).setBold();
        p.add("\n");

        this.document.add(p);
        this.document.add(new AreaBreak(AreaBreakType.NEXT_PAGE));
    }

    /**
     * add the final note to the results from the PDF
     *
     * @param result the final note of the report
     * @throws IOException in case something wrong with intput/output file
     */
    public void addFinalNote(String result) throws IOException {

        Paragraph p = new Paragraph();
        p.setTextAlignment(TextAlignment.RIGHT);
        Text t = new Text(result);

        p.add(t.setFontSize(18).setFontColor(SUB_SECTION_COLOR));
        p.add("\n");

        this.document.add(p);
    }

    public void addSummaryInformation(String result) {
        Paragraph p = new Paragraph();
        Text t = new Text(result);

        p.add(t.setFontSize(12).setFontColor(RESULT_COLOR)).setBold();
        p.add("\n");

        this.document.add(p);
    }

    /**
     * Close the report document
     *
     * @throws java.io.IOException in case something wrong with intput/output
     * file
     */
    public void closePDF() throws IOException {
        this.document.close();
    }

    /**
     * This method created a new font to write in the pdf document.
     *
     * @return the font loaded to use in pdf file
     * @throws IOException in case there are something wrong with the input or
     * output file
     */
    private static PdfFont loadPdfFont() throws IOException {

        Path tmpFile = Files.createTempFile("fa-arial", ".ttf");
        Files.copy(PDF.class.getResourceAsStream("arial.ttf"), tmpFile, StandardCopyOption.REPLACE_EXISTING);

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

    /**
     * This method configure a cell for the left position.
     *
     * @param textCell is the text to put into the cell
     * @return the cell with this configuration
     */
    private Cell addLeftCell(String textCell) {
        Cell leftCell = new Cell();
        leftCell.add(textCell);

        return leftCell;
    }

    /**
     * This method configure a cell for the right position.
     *
     * @param textCell is the score to put into the cell
     * @return the cell with this configuration
     */
    private Cell addRightCell(Double textCell) {
        Cell rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", textCell));
        rightCell.setTextAlignment(TextAlignment.CENTER);

        return rightCell;
    }

    /**
     * this methos add the summary score table.
     *
     * @param scores all scores obtains in tableScore
     * @param messages with all strings needed to build the table
     * @param size Text size in headers
     * @param color Color of the text headers
     * @param position position in the array of the corresponding score obtain
     * in this metric
     */
    public void addTableScores(List<Double> scores, ResourceBundle messages, int size, int color, int[] position) {

        Table table = new Table(2);
        Cell headerCellLeft;
        Cell headerCellRight;
        Cell leftCell;
        Cell rightCell;
        com.itextpdf.kernel.color.Color headerColor;

        /**
         * select header color
         */
        if (color == 0) {
            headerColor = PDF.HEADER_COLOR;
        } else {
            headerColor = PDF.HEADER_2_COLOR;
        }

        /**
         * configuration of the left header of the table
         */
        headerCellLeft = this.configureHeaderCells(messages.getString("headerLeft_table"), size, headerColor);
        table.addHeaderCell(headerCellLeft);

        /**
         * configuration of the right header of the table
         */
        headerCellRight = this.configureHeaderCells(messages.getString("headerRight_table"), size, headerColor);
        table.addHeaderCell(headerCellRight);

        /**
         * 1. Implicit None
         */
        leftCell = this.addLeftCell(messages.getString("implicitNone_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[0]));
        table.addCell(rightCell);

        /**
         * 2. Ratio
         */
        leftCell = this.addLeftCell(messages.getString("ratio_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[1]));
        table.addCell(rightCell);

        /**
         * 3. Nested Loops
         */
        leftCell = this.addLeftCell(messages.getString("useNestedLoops_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[2]));
        table.addCell(rightCell);

        /**
         * 4. Comments beginning
         */
        leftCell = this.addLeftCell(messages.getString("CommentsBeginning_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[3]));
        table.addCell(rightCell);

        /**
         * 5. Comments variables
         */
        leftCell = this.addLeftCell(messages.getString("CommentsVariables_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[4]));
        table.addCell(rightCell);

        /**
         * 6. Comments in functions
         */
        leftCell = this.addLeftCell(messages.getString("CommentsFunctions_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[5]));
        table.addCell(rightCell);

        /**
         * 7. Comments subroutines
         */
        leftCell = this.addLeftCell(messages.getString("CommentsSubroutines_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[6]));
        table.addCell(rightCell);

        /**
         * 8. Comments control structures
         */
        leftCell = this.addLeftCell(messages.getString("CommentsControlStructures_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[7]));
        table.addCell(rightCell);

        /**
         * 9. Use sentence EXIT
         */
        leftCell = this.addLeftCell(messages.getString("UseExit_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[8]));
        table.addCell(rightCell);

        /**
         * 10. Use sentence CYCLE
         */
        leftCell = this.addLeftCell(messages.getString("UseCycle_table"));
        table.addCell(leftCell);

        rightCell = this.addRightCell(scores.get(position[9]));
        table.addCell(rightCell);

        this.document.add(table);

    }

    /**
     * This method add a summary information about the file names with the
     * scores obtains by each file of analysed software.
     *
     * @param fileScores list with all scores for each file analysed
     * @param fileNames list with all names for each file analysed
     * @param messages with all strings necessaries to summary
     */
    public void addFinalSummary(List<Double> fileScores, List<String> fileNames, ResourceBundle messages) {

        Table table = new Table(2, true);
        Cell headerCellLeft;
        Cell headerCellRight;

        /**
         * configuration of the left header of the table
         */
        headerCellLeft = this.configureHeaderCells(messages.getString("headerLeftSummaryTable"), 15, HEADER_COLOR);
        headerCellLeft.setKeepTogether(true);
        table.addHeaderCell(headerCellLeft);

        /**
         * configuration of the right header of the table
         */
        headerCellRight = this.configureHeaderCells(messages.getString("headerRightSummaryTable"), 15, HEADER_COLOR);
        headerCellRight.setKeepTogether(true);
        table.addHeaderCell(headerCellRight);

        this.document.add(table);

        for (int i = 0; i < fileNames.size(); i++) {

            table.addCell(new Cell().setKeepTogether(true).add(new Paragraph(fileNames.get(i))).setTextAlignment(TextAlignment.LEFT));
            table.addCell(new Cell().setKeepTogether(true).add(new Paragraph(String.format(Locale.ROOT, "%.3f", fileScores.get(i)))).setTextAlignment(TextAlignment.CENTER));
        }

        table.complete();

    }

}
