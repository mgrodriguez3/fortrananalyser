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

    /**
     * this methos add the summary score table.
     *
     * @param scores all scores obtains in tableScore
     * @param messages with all strings needed to build the table
     */
    public void addTableScore(List<Double> scores, ResourceBundle messages) {

        Table table = new Table(2);
        Cell headerCellLeft = new Cell();
        Cell headerCellRight = new Cell();
        Cell leftCell = new Cell();
        Cell rightCell = new Cell();

        /**
         * configuration of the left header of the table
         */
        headerCellLeft.add(messages.getString("headerLeft_table"));
        headerCellLeft.setFontSize(13);
        headerCellLeft.setFontColor(HEADER_2_COLOR);
        headerCellLeft.setBorder(Border.NO_BORDER);

        headerCellLeft.setTextAlignment(TextAlignment.CENTER);
        table.addHeaderCell(headerCellLeft);

        /**
         * configuration of the right header of the table
         */
        headerCellRight.add(messages.getString("headerRight_table"));
        headerCellRight.setFontSize(13);
        headerCellRight.setFontColor(HEADER_2_COLOR);
        headerCellRight.setBorder(Border.NO_BORDER);

        headerCellRight.setTextAlignment(TextAlignment.CENTER);
        table.addHeaderCell(headerCellRight);

        /**
         * 1. Implicit None
         */
        leftCell.add(messages.getString("implicitNone_table"));
        table.addCell(leftCell);

        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(5)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 2. Ratio
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("ratio_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(6)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 3. Nested Loops
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("useNestedLoops_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(7)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 4. Comments beginning
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsBeginning_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(1)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 5. Comments variables
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsVariables_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(2)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 6. Comments in functions
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsFunctions_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(0)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 7. Comments subroutines
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsSubroutines_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(3)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 8. Comments control structures
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsControlStructures_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(4)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 9. Use sentence EXIT
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("UseExit_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(8)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 10. Use sentence CYCLE
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("UseCycle_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(9)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        this.document.add(table);

    }

    /**
     * this method add the final score table with a diferent style from the
     * addTableScore method (previously implemented).
     *
     * @param scores all scores to calculate the final note
     * @param messages all strings neccesaries to build the final table
     */
    public void addFinalTableScore(List<Double> scores, ResourceBundle messages) {

        Table table = new Table(2);
        Cell headerCellLeft = new Cell();
        Cell headerCellRight = new Cell();
        Cell leftCell = new Cell();
        Cell rightCell = new Cell();

        /**
         * configuration of the left header of the table
         */
        headerCellLeft.add(messages.getString("headerLeft_table"));
        headerCellLeft.setFontSize(15);
        headerCellLeft.setFontColor(HEADER_COLOR);
        headerCellLeft.setBorder(Border.NO_BORDER);

        headerCellLeft.setTextAlignment(TextAlignment.CENTER);
        table.addHeaderCell(headerCellLeft);

        /**
         * configuration of the right header of the table
         */
        headerCellRight.add(messages.getString("headerRight_table"));
        headerCellRight.setFontSize(15);
        headerCellRight.setFontColor(HEADER_COLOR);
        headerCellRight.setBorder(Border.NO_BORDER);

        headerCellRight.setTextAlignment(TextAlignment.CENTER);
        table.addHeaderCell(headerCellRight);

        /**
         * 1. Implicit None
         */
        leftCell.add(messages.getString("implicitNone_table"));
        table.addCell(leftCell);

        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(0)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 2. Ratio
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("ratio_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(1)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 3. Nested Loops
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("useNestedLoops_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(2)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 4. Comments beginning
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsBeginning_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(3)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 5. Comments variables
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsVariables_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(4)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 6. Comments in functions
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsFunctions_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(5)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 7. Comments subroutines
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsSubroutines_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(6)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 8. Comments control structures
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("CommentsControlStructures_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(7)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 9. Use sentence EXIT
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("UseExit_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(8)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
        table.addCell(rightCell);

        /**
         * 10. Use sentence CYCLE
         */
        leftCell = new Cell();
        leftCell.add(messages.getString("UseCycle_table"));
        table.addCell(leftCell);

        rightCell = new Cell();
        rightCell.add(String.format(Locale.ROOT, "%.3f", scores.get(9)));
        rightCell.setTextAlignment(TextAlignment.CENTER);
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
        Cell headerCellLeft = new Cell().setKeepTogether(true);
        Cell headerCellRight = new Cell().setKeepTogether(true);

        /**
         * configuration of the left header of the table
         */
        headerCellLeft.add(messages.getString("headerLeftSummaryTable"));
        headerCellLeft.setFontSize(15);
        headerCellLeft.setFontColor(HEADER_COLOR);
        headerCellLeft.setBorder(Border.NO_BORDER);

        headerCellLeft.setTextAlignment(TextAlignment.CENTER);
        table.addHeaderCell(headerCellLeft);

        /**
         * configuration of the right header of the table
         */
        headerCellRight.add(messages.getString("headerRightSummaryTable"));
        headerCellRight.setFontSize(15);
        headerCellRight.setFontColor(HEADER_COLOR);
        headerCellRight.setBorder(Border.NO_BORDER);

        headerCellRight.setTextAlignment(TextAlignment.CENTER);
        table.addHeaderCell(headerCellRight);

        this.document.add(table);

        for (int i = 0; i < fileNames.size(); i++) {

            table.addCell(new Cell().setKeepTogether(true).add(new Paragraph(fileNames.get(i))).setTextAlignment(TextAlignment.LEFT));
            table.addCell(new Cell().setKeepTogether(true).add(new Paragraph(String.format(Locale.ROOT, "%.3f", fileScores.get(i)))).setTextAlignment(TextAlignment.CENTER));
        }

        table.complete();

    }

}
