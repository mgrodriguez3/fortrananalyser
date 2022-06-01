/*
 * Copyright (C) 2019 Michael García Rodríguez
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

import es.uvigo.esei.ephyslab.fortrananalyser.GuiComponent.MainWindow;
import es.uvigo.esei.ephyslab.fortrananalyser.metric.CyclomaticComplexity;
import es.uvigo.esei.ephyslab.fortrananalyser.metric.NumberOfLines;
import es.uvigo.esei.ephyslab.fortrananalyser.statistics.Calculation;
import es.uvigo.esei.ephyslab.fortrananalyser.util.FileUtils;

import javax.swing.*;
import java.awt.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class TasksBar extends SwingWorker<Void, Integer> {

    private static final Set<String>    FORTRAN_EXTENSIONS                  = new HashSet<>(Arrays.asList("f90", "h90", "f", "for"));
    private static final String         REPORT_ID                           = "";//UUID.randomUUID().toString();
    private static final String         REPORT_NAME                         = System.getProperty("user.home") + "/temp/QualityReport_"+ REPORT_ID +".pdf";
    private static final String         REPORT_PATH                         = System.getProperty("user.home") + "/temp";
    private static final int[]          POSITION_TABLE_SCORES               = new int[]{5, 6, 7, 1, 2, 0, 3, 4, 8, 9, 10};
    private static final int[]          POSITIONS_FINAL_TABLE_SCORES        = new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    private static final String         END_DO                              = "END DO";
    private static final String         ARROW                               = "\n\t--> ";
    private JPanel                      progressBarPanel;
    private MainWindow                  mw;
    private String                      path;
    private ResourceBundle              messages;
    private int                         totalNumLines;
    private long                        startTime;
    private double                      assesment;
    private double                      auxScore;
    private double                      commentableElements;
    private double                      commentedElements;
    private double                      partialCalification;
    private List<File>                  filesInDirectory;
    private ArrayList<Double>           scores;
    private ArrayList<Double>           implicitNoneScores;
    private ArrayList<Double>           ratioScores;
    private ArrayList<Double>           nestedLoopsScores;
    private ArrayList<Double>           commentsBeginningScores;
    private ArrayList<Double>           commentsVariablesScores;
    private ArrayList<Double>           commentsFunctionScores;
    private ArrayList<Double>           commentsSubroutineScores;
    private ArrayList<Double>           commentsControlStructuresScores;
    private ArrayList<Double>           exitScores;
    private ArrayList<Double>           cycleScores;
    private ArrayList<String>           fileNames;
    private ArrayList<Double>           fileScores;
    private ArrayList<Double>           cycloScores;

    public TasksBar(MainWindow mw, String path, ResourceBundle messages) {
        initializeVariables();
        this.mw                 = mw;
        this.messages           = messages;
        this.progressBarPanel   = new JPanel();
        this.path               = path;
        this.progressBarPanel.setLayout(new FlowLayout());
    }

    public void initializeVariables() {
        scores                          = new ArrayList<>();
        implicitNoneScores              = new ArrayList<>();
        ratioScores                     = new ArrayList<>();
        nestedLoopsScores               = new ArrayList<>();
        commentsBeginningScores         = new ArrayList<>();
        commentsVariablesScores         = new ArrayList<>();
        commentsFunctionScores          = new ArrayList<>();
        commentsSubroutineScores        = new ArrayList<>();
        commentsControlStructuresScores = new ArrayList<>();
        exitScores                      = new ArrayList<>();
        cycleScores                     = new ArrayList<>();
        fileNames                       = new ArrayList<>();
        fileScores                      = new ArrayList<>();
        filesInDirectory                = new ArrayList<>();
        cycloScores                     = new ArrayList<>();
        commentableElements             = 0.0;
        commentedElements               = 0.0;
        partialCalification             = 0.0;
        assesment                       = 0.0;
    }

    public TasksBar() {
    }

    /**
     * In background, this method obtains all the files in an specific directory
     * and subdirectories and analyse them where the extension of the files are
     * ".f90" or ".h90" or ".f"
     *
     * @return the note of the analysed software
     * @throws java.lang.Exception in case something wrong with intput/output
     *                             file
     */
    @Override
    protected Void doInBackground() throws Exception {

        Pdf     pdf;
        int     countNumberOfFiles  = 0;
        double  percentage          = 0.0;
        commentableElements         = 0.0;
        commentedElements           = 0.0;
        partialCalification         = 0.0;
        auxScore                    = 0.0;
        totalNumLines               = 0;
        implicitNoneScores.clear();
        ratioScores.clear();
        nestedLoopsScores.clear();
        commentsBeginningScores.clear();
        commentsVariablesScores.clear();
        commentsFunctionScores.clear();
        commentsSubroutineScores.clear();
        commentsControlStructuresScores.clear();
        exitScores.clear();
        cycleScores.clear();
        fileNames.clear();
        fileScores.clear();
        filesInDirectory.clear();
        cycloScores.clear();

        try {
            String auxDir = "";
            pdf = new Pdf();
            startTime = System.currentTimeMillis();
            percentage += 1.0;
            publish((int) percentage);
            FileUtils.checkTempFileExist();
            pdf.createPdf(TasksBar.REPORT_NAME, messages.getLocale());
            FileUtils.scanFilesInDirectory(path, filesInDirectory);

            for (File file : filesInDirectory) {
                scores.clear();
                if (file.length() > 0) {
                    if (FORTRAN_EXTENSIONS.contains(FileUtils.getFileExtension(file).toLowerCase())) {
                        if(!auxDir.equals(FileUtils.getPathFromFile(file))) {
                            auxDir = FileUtils.getPathFromFile(file);
                            pdf.addSection(auxDir);
                        }
                        pdf.addSubSection(file.getName());
                        fileNames.add(file.getName());
                        pdf.addResult(analyseFile(file.getAbsolutePath()));
                        pdf.addTableScores(scores, messages, 13, 1, TasksBar.POSITION_TABLE_SCORES);
                        countNumberOfFiles++;
                        fileScores.add(assesment);
                        pdf.addScoreResult(messages.getString("noteFile") + String.format("%.3f", assesment));
                    }
                    percentage += 98.0 / filesInDirectory.size();
                    publish((int) percentage);
                }
            }

            scores.clear();
            scores.add(Calculation.calculateAverage(implicitNoneScores));
            scores.add(Calculation.calculateAverage(ratioScores));
            scores.add(Calculation.calculateAverage(nestedLoopsScores));
            scores.add(Calculation.calculateAverage(commentsBeginningScores));
            scores.add(Calculation.calculateAverage(commentsVariablesScores));
            scores.add(Calculation.calculateAverage(commentsFunctionScores));
            scores.add(Calculation.calculateAverage(commentsSubroutineScores));
            scores.add(Calculation.calculateAverage(commentsControlStructuresScores));
            scores.add(Calculation.calculateAverage(exitScores));
            scores.add(Calculation.calculateAverage(cycleScores));
            scores.add(Calculation.calculateAverage(cycloScores));

            if (!scores.get(0).isNaN()) {
                pdf.addSection(messages.getString("summary"));
                pdf.addFinalSummary(fileScores, fileNames, messages);
                pdf.addSummaryInformation(messages.getString("totalNumberOfFiles") + " " + countNumberOfFiles);
                pdf.addSummaryInformation(messages.getString("totalNumberOfLines") + " " + totalNumLines);
                pdf.addSubSectionInBold(messages.getString("finalTable"));
                pdf.addTableScores(scores, messages, 15, 0, TasksBar.POSITIONS_FINAL_TABLE_SCORES);

                if (totalNumLines != 0) {
                    auxScore = partialCalification / totalNumLines;
                }
                pdf.addFinalNote(messages.getString("arithmeticAverage") + " " + String.format(Locale.ROOT, "%.3f", auxScore));
            }
            pdf.closePDF();
            partialCalification = 0.0;
            percentage          = 100;
            totalNumLines       = 0;
            publish((int) percentage);

        } catch (IOException ex) {
            Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
        }
        mw.setEnabled(true);
        return null;
    }

    @Override
    protected void process(List<Integer> chunks) {
        mw.customProgressBar1.updateProgressBar(chunks.get(0));
        mw.customProgressBar1.repaint();
    }

    @Override
    protected void done() {
        long timeStop = System.currentTimeMillis();
        timeStop = timeStop - startTime;
        mw.getjLabel19().setText(Calculation.getDurationAnalyse(timeStop));
        mw.getjLabel20().setText(String.format(Locale.ROOT, "%.3f", auxScore));
        mw.getjLabel21().setText(TasksBar.REPORT_NAME);
        mw.getjLabel15().setVisible(true);
        mw.getjLabel16().setVisible(true);
        mw.getjLabel17().setVisible(true);
        mw.getjLabel18().setVisible(true);
        mw.getjLabel19().setVisible(true);
        mw.getjLabel20().setVisible(true);
        mw.getjLabel21().setVisible(true);
        mw.getjLabel22().setVisible(true);
        mw.getjLabel24().setVisible(true);
        mw.getjButton1().setEnabled(true);
        mw.getjButton3().setEnabled(true);
    }

    public String analyseFile(String pathFile) throws IOException {
        String result               = "";
        assesment                   = 0.0;
        double ratio                = 0.0;
        double avgCyclo;
        Thread numberOfLinesThread  = new Thread(new NumberOfLines(pathFile));
        numberOfLinesThread.start();

        int numLines                = analyseNumberOfLines(pathFile);
        boolean useImplicitNone     = analyseUseImplicitNone(pathFile);
        boolean checkNestedLoops    = analyseNestedLoops(pathFile);
        boolean useExit             = analyseUseExit(pathFile);
        boolean useCycle            = analyseUseCycle(pathFile);
        int numFunctions            = analyseNumFunctions(pathFile);
        int numSubroutines          = analyseNumberSubroutines(pathFile);
        int numVariables            = analyseNumberOfDeclaredVariables(pathFile);
        String goodComments         = analyseGoodComment(pathFile);
        CyclomaticComplexity cc     = new CyclomaticComplexity();
        String cycloResult          = cc.simpleComplexityCalculation(pathFile, messages);
        commentableElements         += numFunctions;
        commentableElements         += numSubroutines;
        commentableElements         += numVariables;
        result                      += messages.getString("numberOfLines") + numLines;
        result                      += "\n";
        totalNumLines               += numLines;
        /**
         * 6. Use or not use the sentence IMPLICIT NONE
         */
        result                      += messages.getString("implicitNone") + useImplicitNone;
        result                      += "\n";
        if (useImplicitNone) {
            assesment += 2.0;
            scores.add(2.0);
            implicitNoneScores.add(2.0);
        } else {
            scores.add(0.0);
            implicitNoneScores.add(0.0);
        }
        result                      += messages.getString("numFunctions") + numFunctions;
        result                      += "\n";
        result                      += messages.getString("subroutinesCall") + analyseNumCalls(pathFile);
        result                      += "\n";

        /**
         * 7. calcule the ratio and show it in percentage in the report
         */
        if (commentableElements > 0.0) {
            ratio = (commentedElements / commentableElements);
        }
        result                      += messages.getString("ratio") + String.format(Locale.ROOT, "%.2f", (ratio * 100)) + "%";
        result                      += " \n";
        ratio                       = ratio * 2.0;
        assesment                   += ratio;
        scores.add(ratio);
        ratioScores.add(ratio);
        result                      += messages.getString("numVariables") + numVariables;
        result                      += "\n";
        /**
         * 8. check the Nested loops
         */
        result                      += messages.getString("nestedLoops") + checkNestedLoops;
        result                      += "\n";
        if (checkNestedLoops) {
            assesment += 2.0;
            scores.add(2.0);
            nestedLoopsScores.add(2.0);
        } else {
            scores.add(0.0);
            nestedLoopsScores.add(0.0);
        }
        result                      += messages.getString("goodComments") + goodComments;
        result                      += "\n";
        result                      += messages.getString("subroutines") + numSubroutines;
        result                      += "\n";
        /**
         * 9. Check the use of EXIT
         */
        result                      += messages.getString("exit") + useExit;
        result                      += "\n";
        if (useExit) {
            assesment += 1.0;
            scores.add(1.0);
            exitScores.add(1.0);
        } else {
            scores.add(0.0);
            exitScores.add(0.0);
        }
        /**
         * 10. Check the use of CYCLE
         */
        result                      += messages.getString("cycle") + useCycle;
        result                      += "\n";
        if (useCycle) {
            assesment += 1.0;
            scores.add(1.0);
            cycleScores.add(1.0);
        } else {
            scores.add(0.0);
            cycleScores.add(0.0);
        }
        partialCalification += assesment * numLines;
        /**
         * 11. Add the Cyclomatic complexity.
         */
        if (!cycloResult.isEmpty()) {
            result      += "\n";
            result      += messages.getString("cyclomaticComplexity").toUpperCase();
            result      += "\n\n";
            result      += cycloResult;
            avgCyclo    = Calculation.calculateAverage(cc.getScoresCC());
            cycloScores.add(avgCyclo);
            scores.add(avgCyclo);
        } else {
            scores.add(0.0);
        }
        return result;
    }

    public static int analyseNumberOfLines(String filePath) throws IOException {
        int         count       = 0;
        String      line        = "";
        File        file        = new File(filePath);
        FileReader  fr          = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((line = b.readLine()) != null) {
                count++;
            }
        }
        return count;
    }

    public static boolean analyseUseImplicitNone(String filePath) throws IOException {
        String      chain   = "";
        File        file    = new File(filePath);
        FileReader  fr      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!") && chain.contains("IMPLICIT NONE")) {
                    return true;
                }
            }
        }
        return false;
    }

    public static int analyseNumFunctions(String filePath) throws IOException {
        int         count   = 0;
        String      chain   = "";
        File        file    = new File(filePath);
        FileReader  fr      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains("END FUNCTION ")
                        && chain.contains("FUNCTION ")) {
                    count++;
                }
            }
        }
        return count;
    }

    public static int analyseNumCalls(String filePath) throws IOException {
        int         count   = 0;
        String      chain   = "";
        File        file    = new File(filePath);
        FileReader  fr      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!") && chain.contains("CALL")) {
                    count++;
                }
            }
        }
        return count;
    }

    public static boolean analyseUseCycle(String filePath) throws IOException {
        String      chain       = "";
        int         numCycles   = 0;
        int         numLoops    = 0;
        File        file        = new File(filePath);
        FileReader  fr          = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains(TasksBar.END_DO)
                        && chain.contains("DO")) {
                    numLoops++;
                }
                if (chain.contains("CYCLE")
                        && !chain.contains("!")) {

                    numCycles++;
                }
            }
        }
        return numLoops == numCycles;
    }

    public static boolean analyseUseExit(String filePath) throws IOException {
        String      chain       = "";
        int         numLoops    = 0;
        int         numExit     = 0;
        File        file        = new File(filePath);
        FileReader  fr          = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains(TasksBar.END_DO)
                        && chain.contains("DO")) {
                    numLoops++;
                }
                if ((chain.contains("EXIT"))
                        && !chain.contains("!")) {

                    numExit++;
                }
            }
        }
        return numLoops == numExit;
    }

    public static int analyseNumberSubroutines(String filePath) throws IOException {
        String      chain   = "";
        int         count   = 0;
        File        file    = new File(filePath);
        FileReader  fr      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (chain.contains("SUBROUTINE")
                        && !chain.contains("!")
                        && chain.contains("END SUBROUTINE")) {
                    count++;
                }
            }
        }
        return count;
    }

    public static boolean analyseNestedLoops(String filePath) throws IOException {
        int         nestedLoops = 0;
        String      chain       = "";
        File        file        = new File(filePath);
        FileReader  fr          = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains(TasksBar.END_DO)
                        && chain.contains("DO")) {
                    nestedLoops++;
                    if (nestedLoops > 3) {
                        return false;
                    }
                }
                if (!chain.contains("!")
                        && chain.contains(TasksBar.END_DO)) {
                    nestedLoops--;
                    if (nestedLoops < 0) {
                        return false;
                    }
                }
            }
        }
        return (nestedLoops >= 0 && nestedLoops <= 3 );
    }

    public static int analyseNumberOfDeclaredVariables(String filePath) throws IOException {
        String      chain   = "";
        int         count   = 0;
        File        file    = new File(filePath);
        FileReader  fr      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("::")) {
                    count++;
                    if (chain.contains(",") && chain.indexOf("::") <= chain.indexOf(',')) {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    public String analyseGoodComment(String filePath) throws IOException {
        String  sb                              = "";
        boolean goodCommentFunctions            = analyseGoodCommentFunctions(filePath);
        boolean goodCommentInitDoc              = analyseGoodCommentInitDoc(filePath);
        boolean goodCommentVariables            = analyseGoodCommentedVariables(filePath);
        boolean goodCommentSubroutines          = analyseGoodCommentSubroutines(filePath);
        boolean goodCommentControlStructures    = analyseGoodCommentControlStructures(filePath);
        sb                                      = TasksBar.ARROW + messages.getString("function") + goodCommentFunctions;
        sb                                      += TasksBar.ARROW + messages.getString("initDoc") + goodCommentInitDoc;
        sb                                      += TasksBar.ARROW + messages.getString("variables") + goodCommentVariables;
        sb                                      += TasksBar.ARROW + messages.getString("commentSubroutines") + goodCommentSubroutines;
        sb                                      += TasksBar.ARROW + messages.getString("commentControlStructures") + goodCommentControlStructures;

        /**
         * 1. comments in functions
         */
        if (goodCommentFunctions) {
            assesment += 0.4;
            scores.add(0.4);
            commentsFunctionScores.add(0.4);
        } else {
            scores.add(0.0);
            commentsFunctionScores.add(0.0);
        }
        /**
         * 2. comments at the begining of the document
         */
        if (goodCommentInitDoc) {
            assesment += 0.4;
            scores.add(0.4);
            commentsBeginningScores.add(0.4);
        } else {
            scores.add(0.0);
            commentsBeginningScores.add(0.0);
        }
        /**
         * 3. comments in variables
         */
        if (goodCommentVariables) {
            assesment += 0.4;
            scores.add(0.4);
            commentsVariablesScores.add(0.4);
        } else {
            scores.add(0.0);
            commentsVariablesScores.add(0.0);
        }
        /**
         * 4. comments in subroutines
         */
        if (goodCommentSubroutines) {
            assesment += 0.4;
            scores.add(0.4);
            commentsSubroutineScores.add(0.4);
        } else {
            scores.add(0.0);
            commentsSubroutineScores.add(0.0);
        }
        /**
         * 5. comments in control structures
         */
        if (goodCommentControlStructures) {
            assesment += 0.4;
            scores.add(0.4);
            commentsControlStructuresScores.add(0.4);
        } else {
            scores.add(0.0);
            commentsControlStructuresScores.add(0.0);
        }
        return sb;
    }

    public static boolean analyseGoodCommentControlStructures(String filePath) throws IOException {
        String      chain                   = "";
        String      previousChain           = "";
        File        file                    = new File(filePath);
        int         numControlStructures    = 0;
        int         totalControlStructures  = 0;
        FileReader  fr                      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if ((!chain.contains("!")
                        && !chain.contains("ENDIF")
                        && chain.contains("IF ("))
                        || (!chain.contains("!")
                        && !chain.contains("END SELECT")
                        && chain.contains("SELECT CASE"))) {
                    totalControlStructures++;
                    if (previousChain.contains("!")) {
                        numControlStructures++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalControlStructures == numControlStructures;
    }

    public boolean analyseGoodCommentSubroutines(String filePath) throws IOException {
        String      chain               = "";
        String      previousChain       = "";
        File        file                = new File(filePath);
        int         numSubroutines      = 0;
        int         totalSubroutines    = 0;
        FileReader  fr                  = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains("END SUBROUTINE")
                        && chain.contains("SUBROUTINE")) {
                    totalSubroutines++;
                    if (previousChain.contains("!")) {
                        numSubroutines++;
                        commentedElements++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalSubroutines == numSubroutines;
    }

    public boolean analyseGoodCommentedVariables(String filePath) throws IOException {
        String      rowLine             = "";
        String      previousRowLine     = "";
        int         variablesCommented  = 0;
        int         totalVariables      = 0;
        File        file                = new File(filePath);
        FileReader  fr                  = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((rowLine = b.readLine()) != null) {
                if (rowLine.contains("::")) {
                    totalVariables++;
                    if (previousRowLine.contains("!")) {
                        variablesCommented++;
                        commentedElements++;
                    }
                }
                previousRowLine = rowLine;
            }
        }
        return totalVariables == variablesCommented;
    }

    public static boolean analyseGoodCommentInitDoc(String filePath) throws IOException {
        String      chain   = "";
        int         count   = 0;
        int         ite     = 0;
        File        file    = new File(filePath);
        FileReader  fr      = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null && count < 2 && ite < 3) {
                if (chain.contains("!")) {
                    count++;
                }
                ite++;
            }
        }
        return count > 1;
    }

    public boolean analyseGoodCommentFunctions(String filePath) throws IOException {
        String      chain           = "";
        String      previousChain   = "";
        File        file            = new File(filePath);
        int         numFunction     = 0;
        int         totalFunctions  = 0;
        FileReader  fr              = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains("END FUNCTION")
                        && chain.contains("FUNCTION")) {
                    totalFunctions++;
                    if (previousChain.contains("!")) {
                        numFunction++;
                        commentedElements++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalFunctions == numFunction;
    }

    public static String getDEST() {
        return REPORT_NAME;
    }

    public static String getDESTPATH() {
        return REPORT_PATH;
    }

    public static int[] getPOSITIONTABLESCORES() {
        return POSITION_TABLE_SCORES;
    }

    public static int[] getPOSITIONSFINALTABLESCORES() {
        return POSITIONS_FINAL_TABLE_SCORES;
    }

    public static String getARROW() {
        return ARROW;
    }

    public static Set<String> getFORTRAN_EXTENSIONS() {
        return FORTRAN_EXTENSIONS;
    }


}
