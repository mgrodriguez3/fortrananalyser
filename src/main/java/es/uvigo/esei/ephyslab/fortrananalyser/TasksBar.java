/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import java.awt.Color;
import java.awt.Desktop;
import java.awt.FlowLayout;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;
import javax.swing.UIManager;

/**
 * This class create a taskBar and support the logic part of the application. it
 * generate the quallity report
 *
 * @author Michael García Rodríguez
 * @version 1.9.2
 */
public class TasksBar extends
        SwingWorker<Void, Integer> {

    TasksBar(JTextArea textArea, int numbersToFind) {

    }

    /**
     * the extension file to search.
     */
    private static final String EXTENSION = "f90";

    /**
     * the second extension file to search.
     */
    private static final String EXTENSION2 = "h90";

    /**
     * the third extension file to search.
     */
    private static final String EXTENSION3 = "f";

    /**
     * the path and the name of the file.
     */
    public static final String DEST = System.getProperty("user.home") + "/temp/QualityReport.pdf";

    /**
     * the path of the destination of the file
     */
    public static final String PATH = System.getProperty("user.home") + "/temp";

    /**
     * the frame that represent the taskbar.
     */
    private JFrame taskbar;

    /**
     * the progressbar of the taskbar.
     */
    private JProgressBar progreso;

    /**
     * the panel where is the progressbar.
     */
    private JPanel pane;

    /**
     * the window of the progressbar.
     */
    private Window w;

    /**
     * the path of the directory to analyse.
     */
    private String path;

    /**
     * the assesment from file of the quality report.
     */
    private double assesment = 0.0;

    /**
     * auxiliar variable to calcule the final calification.
     */
    private double auxNote;

    /**
     * the time when the process started.
     */
    private long timeStart;

    /**
     * the list with the parcial score of each file.
     */
    private ArrayList<Double> scores;

    /**
     * the list with all scores obtain by implicit none metric.
     */
    private ArrayList<Double> scoresImplicitNone;

    /**
     * the list with all scores obtain by percentage of comments metric.
     */
    private ArrayList<Double> scoresRatio;

    /**
     * the list with all scores obtain by nested loops metric.
     */
    private ArrayList<Double> scoresNestedLoops;

    /**
     * the list with all scores obtain by comments metric at the beginning.
     */
    private ArrayList<Double> scoresCommentsBeginning;

    /**
     * the list with all scores obtain by comments metric in variables.
     */
    private ArrayList<Double> scoresCommentsVariables;

    /**
     * the list with all scores obtain by comments metric in functions.
     */
    private ArrayList<Double> scoresCommentsfunction;

    /**
     * the list with all scores obtain by comments metric in subroutine.
     */
    private ArrayList<Double> scoresCommentsSubroutine;

    /**
     * the list with all scores obtain by control structures metric.
     */
    private ArrayList<Double> scoresCommentsControlStructures;

    /**
     * the list with scores obtain by exit metric.
     */
    private ArrayList<Double> scoresExit;

    /**
     * the list with scores obtain by cycle metric.
     */
    private ArrayList<Double> scoresCycle;

    /**
     * number of comentable elements in file
     */
    private double commentableElements;

    /**
     * number of comented elements in file
     */
    private double commentedElements;

    /**
     * total number of lines of the analysed software
     */
    private int totalNumLines;

    /**
     * Sum of all scores for each file analysed
     */
    private double partialCalification;
    
    /**
     * Names of each file
     */
    private ArrayList<String> fileNames;
    
    /**
     * Scores obtain by each file
     */
    private ArrayList<Double> fileScores;

    /**
     * the string resources i18n.
     */
    ResourceBundle messages;

    TasksBar(Window w, String path, ResourceBundle messages) {

        this.scores = new ArrayList<>();
        this.scoresImplicitNone = new ArrayList<>();
        this.scoresRatio = new ArrayList<>();
        this.scoresNestedLoops = new ArrayList<>();
        this.scoresCommentsBeginning = new ArrayList<>();
        this.scoresCommentsVariables = new ArrayList<>();
        this.scoresCommentsfunction = new ArrayList<>();
        this.scoresCommentsSubroutine = new ArrayList<>();
        this.scoresCommentsControlStructures = new ArrayList<>();
        this.scoresExit = new ArrayList<>();
        this.scoresCycle = new ArrayList<>();
        this.fileNames = new ArrayList<>();
        this.fileScores = new ArrayList<>();
        this.commentableElements = 0.0;
        this.commentedElements = 0.0;
        this.partialCalification = 0.0;

        this.w = w;
        this.messages = messages;
        this.taskbar = new JFrame();
        this.pane = new JPanel();
        this.path = path;

        this.pane.setLayout(new FlowLayout());
        this.progreso = new JProgressBar(0, 100);
        this.progreso.setValue(0);
        this.progreso.setStringPainted(true);
        this.pane.add(progreso);
        this.pane.setVisible(true);

        this.taskbar.add(this.pane);
        this.taskbar.pack();
        this.taskbar.setLocationRelativeTo(null);
        this.taskbar.setResizable(false);
        this.taskbar.setVisible(true);

    }

    TasksBar(String path, ResourceBundle messages) {
        this.scores = new ArrayList<>();
        this.scoresImplicitNone = new ArrayList<>();
        this.scoresRatio = new ArrayList<>();
        this.scoresNestedLoops = new ArrayList<>();
        this.scoresCommentsBeginning = new ArrayList<>();
        this.scoresCommentsVariables = new ArrayList<>();
        this.scoresCommentsfunction = new ArrayList<>();
        this.scoresCommentsSubroutine = new ArrayList<>();
        this.scoresCommentsControlStructures = new ArrayList<>();
        this.scoresExit = new ArrayList<>();
        this.scoresCycle = new ArrayList<>();
        this.fileNames = new ArrayList<>();
        this.fileScores = new ArrayList<>();
        this.commentableElements = 0.0;
        this.commentedElements = 0.0;
        this.partialCalification = 0.0;

        this.messages = messages;
        this.path = path;

    }

    /**
     * Empty constructor
     */
    public TasksBar() {
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    /**
     * In background, this method obtains all the files in an specific directory
     * and subdirectories and analyse them where the extension of the files are
     * ".f90" or ".h90" or ".f"
     *
     *
     * @return the note of the analysed software
     * @throws java.lang.Exception
     */
    @Override
    protected Void doInBackground() throws Exception {

        /**
         * initialice all arrayList and global variables
         */
        this.scoresImplicitNone.clear();
        this.scoresRatio.clear();
        this.scoresNestedLoops.clear();
        this.scoresCommentsBeginning.clear();
        this.scoresCommentsVariables.clear();
        this.scoresCommentsfunction.clear();
        this.scoresCommentsSubroutine.clear();
        this.scoresCommentsControlStructures.clear();
        this.scoresExit.clear();
        this.scoresCycle.clear();
        this.fileNames.clear();
        this.fileScores.clear();
        this.commentableElements = 0.0;
        this.commentedElements = 0.0;
        this.totalNumLines = 0;
        this.partialCalification = 0.0;

        PDF pdf;
        int countNumberOfFiles = 0;
        auxNote = 0.0;

        double percentage = 0.0;

        try {

            List<File> filesInFolder;
            String auxDir = "";
            pdf = new PDF();
            String extensionFile = "";

            //start the duration of the analysis
            timeStart = System.currentTimeMillis();

            percentage += 1.0;
            publish((int) percentage);

            /**
             * In case the temp folder doesn't exits
             */
            if (!Files.exists(Paths.get(TasksBar.DEST))) {
                new File(TasksBar.PATH).mkdirs();
            }

            pdf.createPdf(TasksBar.DEST, this.messages.getLocale());

            filesInFolder = Files.walk(Paths.get(this.path))
                    .filter(Files::isRegularFile)
                    .map(java.nio.file.Path::toFile)
                    .collect(Collectors.toList());

            /**
             * for each file of the directory and subdirectory
             */
            for (File file : filesInFolder) {

                this.scores.clear();
                extensionFile = getFileExtension(file).toLowerCase();

                /**
                 * Check if the file is not empty
                 */
                if (file.length() > 0) {
                    /**
                     * If it is a new directory, the path is added into the
                     * report.
                     */
                    if (!auxDir.equals(getPathFromFile(file))
                            && (extensionFile.equals(TasksBar.EXTENSION)
                            || extensionFile.equals(TasksBar.EXTENSION2)
                            || extensionFile.equals(TasksBar.EXTENSION3))) {
                        auxDir = getPathFromFile(file);
                        pdf.addSection(auxDir);
                    }

                    /**
                     * If it is a new file of fortran code, the path is added
                     * into the report.
                     */
                    if (extensionFile.equals(TasksBar.EXTENSION)
                            || extensionFile.equals(TasksBar.EXTENSION2)
                            || extensionFile.equals(TasksBar.EXTENSION3)) {
                        pdf.addSubSection(file.getName());
                        this.fileNames.add(file.getName());
                        pdf.addResult(analyseFile(file.getAbsolutePath()));
                        pdf.addTableScore(scores, this.messages);
                        countNumberOfFiles++;
                        this.fileScores.add(assesment);
                        pdf.addScoreResult(this.messages.getString("noteFile") + String.format("%.3f", assesment));
                    }

                    percentage += 98.0 / filesInFolder.size();
                    publish((int) percentage);
                }
            }

            /**
             * the list scores is reused to stock the average of all metrics.
             */
            this.scores.clear();
            this.scores.add(this.calculateAverage(this.scoresImplicitNone));
            this.scores.add(this.calculateAverage(this.scoresRatio));
            this.scores.add(this.calculateAverage(this.scoresNestedLoops));
            this.scores.add(this.calculateAverage(this.scoresCommentsBeginning));
            this.scores.add(this.calculateAverage(this.scoresCommentsVariables));
            this.scores.add(this.calculateAverage(this.scoresCommentsfunction));
            this.scores.add(this.calculateAverage(this.scoresCommentsSubroutine));
            this.scores.add(this.calculateAverage(this.scoresCommentsControlStructures));
            this.scores.add(this.calculateAverage(this.scoresExit));
            this.scores.add(this.calculateAverage(this.scoresCycle));

            /**
             * Check if the software analysed have not Fortran files
             */
            if (!this.scores.get(0).isNaN()) {
                pdf.addSection(this.messages.getString("summary"));
                pdf.addFinalSummary(this.fileScores, this.fileNames, this.messages);
                pdf.addSummaryInformation(this.messages.getString("totalNumberOfFiles") + " " + countNumberOfFiles);
                pdf.addSummaryInformation(this.messages.getString("totalNumberOfLines") + " " + this.totalNumLines);
                pdf.addSubSectionInBold(this.messages.getString("finalTable"));
                pdf.addFinalTableScore(this.scores, this.messages);
                auxNote = partialCalification / this.totalNumLines;
                pdf.addFinalNote(this.messages.getString("arithmeticAverage") + " " + String.format(Locale.ROOT, "%.3f", auxNote));
            }

            pdf.closePDF();
            partialCalification = 0.0;
            percentage = 100;
            this.totalNumLines = 0;
            publish((int) percentage);

        } catch (IOException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }

        this.w.setEnabled(true);
        return null;
    }

    /**
     * This override method update the value of the progressBar when publish
     * method is called.
     *
     * @param chunks
     */
    @Override
    protected void process(List<Integer> chunks) {
        progreso.setValue(chunks.get(0));
    }

    /**
     * when the task is done (so the PDF was created), this override method is
     * called to finalice the task execution.
     */
    @Override
    protected void done() {

        taskbar.setVisible(false);
        taskbar.dispose();

        UIManager.put("OptionPane.background", Color.white);
        UIManager.put("Panel.background", Color.white);
        ImageIcon icon = new ImageIcon(Window.class.getResource("fortranAnalyserIcon.png"));
        long timeStop = System.currentTimeMillis();

        timeStop = timeStop - timeStart;

        JOptionPane.showMessageDialog(taskbar, "<html> <span style='color:#007A82'>" + messages.getString("exitMessage") + "</span></html>"
                + messages.getString("directoryMessage") + "\n" + TasksBar.DEST + "\n"
                + "\n<html> <span style='color:#cf6a0b'>"
                + messages.getString("timeMessage") + TasksBar.getDurationAnalyse(timeStop)
                + "</span></html>\n"
                + "\n<html> <span style='color:#089650'>" + messages.getString("arithmeticAverage") + String.format(Locale.ROOT, "%.3f", auxNote) + "</span></html>",
                this.messages.getString("headMessageDialog"), JOptionPane.INFORMATION_MESSAGE, icon);

        /**
         * open the pdf file if it is possible
         */
        if (Desktop.isDesktopSupported()) {
            try {
                File myFile = new File(TasksBar.DEST);
                Desktop.getDesktop().open(myFile);
            } catch (IOException ex) {
                Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    /**
     * This method obtains the path from file without it name
     *
     * @param file the file that you want to get the path without it name
     * @return the path from a file
     */
    private static String getPathFromFile(File file) {

        return file.getAbsolutePath().
                substring(0, file.getAbsolutePath().lastIndexOf(File.separator));

    }

    /**
     * This method obtains the extension of a file
     *
     * @param file the file that we want to check the extension
     * @return the extension of the file
     */
    private static String getFileExtension(File file) {

        String name = file.getName();
        try {
            return name.substring(name.lastIndexOf(".") + 1);
        } catch (Exception e) {
            return "";
        }
    }

    /**
     * It call all the analyses from a file
     *
     * @param pathFile the path from the file to analyse
     * @return the result with all the output data
     * @throws IOException
     */
    public String analyseFile(String pathFile) throws IOException {

        String result = "";
        assesment = 0.0;
        double ratio = 0.0;
        int numLines = this.analyseNumberOfLines(pathFile);
        boolean useImplicitNone = this.analyseUseImplicitNone(pathFile);
        boolean checkNestedLoops = this.analyseNestedLoops(pathFile);
        boolean useExit = this.analyseUseExit(pathFile);
        boolean useCycle = this.analyseUseCycle(pathFile);
        int numFunctions = this.analyseNumFunctions(pathFile);
        int numSubroutines = this.analyseNumberSubroutines(pathFile);
        int numVariables = this.analyseNumberOfDeclaredVariables(pathFile);
        String goodComments = this.analyseGoodComment(pathFile);

        this.commentableElements += numFunctions;
        this.commentableElements += numSubroutines;
        this.commentableElements += numVariables;

        /**
         * count the number of lines in the file
         */
        result += this.messages.getString("numberOfLines") + numLines;
        result += "\n";

        this.totalNumLines += numLines;

        /**
         * 6. Use or not use the sentence IMPLICIT NONE
         */
        result += this.messages.getString("implicitNone") + useImplicitNone;
        result += "\n";

        /**
         * in case the sentente IMPLICIT NONE is used
         */
        if (useImplicitNone) {
            assesment += 2.0;
            this.scores.add(2.0);
            this.scoresImplicitNone.add(2.0);

        } else {
            this.scores.add(0.0);
            this.scoresImplicitNone.add(0.0);
        }

        /**
         * count the number of functions declared
         */
        result += this.messages.getString("numFunctions") + numFunctions;
        result += "\n";

        /**
         * count the number of subroutines calls
         */
        result += this.messages.getString("subroutinesCall") + this.analyseNumCalls(pathFile);
        result += "\n";

        /**
         * 7. calcule the ratio and show it in percentage in the report
         */
        if (commentableElements > 0.0) {
            ratio = (commentedElements / commentableElements);
        }

        result += this.messages.getString("ratio") + String.format(Locale.ROOT, "%.2f", (ratio * 100)) + "%";
        result += " \n";

        ratio = ratio * 2.0;

        assesment += ratio;
        this.scores.add(ratio);
        this.scoresRatio.add(ratio);

        /**
         * count the number of variables declared
         */
        result += this.messages.getString("numVariables") + numVariables;
        result += "\n";

        /**
         * 8. check the Nested loops
         */
        result += this.messages.getString("nestedLoops") + checkNestedLoops;
        result += "\n";

        /**
         * in case the nestedLoop are ok
         */
        if (checkNestedLoops) {
            assesment += 2.0;
            this.scores.add(2.0);
            this.scoresNestedLoops.add(2.0);
        } else {
            this.scores.add(0.0);
            this.scoresNestedLoops.add(0.0);
        }

        /**
         * Good comments in file
         */
        result += this.messages.getString("goodComments") + goodComments;
        result += "\n";

        /**
         * Check the number of declared subroutines
         */
        result += this.messages.getString("subroutines") + numSubroutines;
        result += "\n";

        /**
         * 9. Check the use of EXIT
         */
        result += this.messages.getString("exit") + useExit;
        result += "\n";

        /**
         * In case the sentece EXIT is used
         */
        if (useExit) {
            assesment += 1.0;
            this.scores.add(1.0);
            this.scoresExit.add(1.0);
        } else {
            this.scores.add(0.0);
            this.scoresExit.add(0.0);
        }

        /**
         * 10. Check the use of CYCLE
         */
        result += this.messages.getString("cycle") + useCycle;
        result += "\n";

        /**
         * in case the sentence CYCLE is used
         */
        if (useCycle) {
            assesment += 1.0;
            this.scores.add(1.0);
            this.scoresCycle.add(1.0);
        } else {
            this.scores.add(0.0);
            this.scoresCycle.add(0.0);
        }

        this.partialCalification += this.assesment * numLines;

        return result;

    }

    /**
     * This method obtains the number of lines of a file
     *
     * @param filePath the path of the file
     * @return the number of lines from file
     * @throws IOException
     */
    public int analyseNumberOfLines(String filePath) throws IOException {

        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while (b.readLine() != null) {
                count++;
            }
        }

        return count;
    }

    /**
     * This method analyse if the sentence implicit none is used in each line
     * from a file
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseUseImplicitNone(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

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

    /**
     * This method analyse the number of functions in file filePath
     *
     * @param filePath The path from file to analyse
     * @return the number of functions in this file
     * @throws IOException
     */
    public int analyseNumFunctions(String filePath) throws IOException {

        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                chain = chain.toUpperCase();
                if (!chain.contains("!")
                        && !chain.contains("END FUNCTION")
                        && chain.contains("FUNCTION")) {
                    count++;
                }
            }
        }

        return (count / 2);
    }

    /**
     * This method analyse the number of subroutines are called in this file
     *
     * @param filePath the absolute path from file
     * @return the number of subroutines calls
     * @throws IOException
     */
    public int analyseNumCalls(String filePath) throws IOException {
        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

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

    /**
     * This method analyse the use of the CYCLE sentence in the file. It is used
     * in loops to avoid making a certain sentence, so that it continues to
     * iterate to the next element. With they use, the code is more efficient.
     *
     * @param filePath
     * @return
     * @throws IOException
     */
    public boolean analyseUseCycle(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);
        int numCycles = 0;
        int numLoops = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                /**
                 * check if there are a loop.
                 */
                if (!chain.contains("!")
                        && !chain.contains("END DO")
                        && chain.contains("DO")) {
                    numLoops++;
                }
                /**
                 * check if the chain is a declaration of a variable and it is
                 * not a comment.
                 */
                if (chain.contains("CYCLE")
                        && !chain.contains("!")) {

                    numCycles++;
                }
            }
        }

        return numLoops == numCycles;
    }

    /**
     * This method check if the EXIT sentence is used in the file. EXIT sentence
     * is used to go out of a loop, so the code is more efficient
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseUseExit(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);
        int numLoops = 0;
        int numExit = 0;
        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();
                /**
                 * check if they are a loop.
                 */
                if (!chain.contains("!")
                        && !chain.contains("END DO")
                        && chain.contains("DO")) {
                    numLoops++;
                }

                /**
                 * check if the chain is a declaration of a variable and it is
                 * not a comment.
                 */
                if ((chain.contains("EXIT"))
                        && !chain.contains("!")) {

                    numExit++;
                }

            }
        }
        return numLoops == numExit;
    }

    /**
     * This method analyse the number of subroutines declared in a file
     *
     * @param filePath
     * @return the number of subroutines
     * @throws IOException
     */
    public int analyseNumberSubroutines(String filePath) throws IOException {

        String chain = "";
        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

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

    /**
     * This method analyse the number of Nested loops there are. If this number
     * is greater than 3 or smaller than 0 AND this line don't have a comment ,
     * it is consider a bad programming practice.
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseNestedLoops(String filePath) throws IOException {

        String chain = "";
        int nestedLoops = 0;
        File file = new File(filePath);
        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                if (!chain.contains("!")
                        && !chain.contains("END DO")
                        && chain.contains("DO")) {

                    nestedLoops++;

                    if (nestedLoops > 3) {
                        return false;
                    }

                }

                if (!chain.contains("!")
                        && chain.contains("END DO")) {
                    nestedLoops--;
                    if (nestedLoops < 0) {
                        return false;
                    }
                }

            }

        }

        /**
         * In case there are not nested loops in file, Else there are nested
         * loops within close sentence.
         */
        return nestedLoops == 0;

    }

    /**
     * This method count the number of declared variables in a file
     *
     * @param filePath
     * @return the number of declared variables
     * @throws IOException
     */
    public int analyseNumberOfDeclaredVariables(String filePath) throws IOException {
        String chain = "";
        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("::")) {
                    count++;
                }
            }
        }

        return count;
    }

    /**
     * This method check if all comments are good. This is: 1.- the functions
     * are commented after or before the declaration. 2.- the variables are
     * commented after or before the declaration. 3.- the subrutines are
     * commented after or befor the declaration. 4.- the three first or more
     * lines of a file are commented.
     *
     * @param filePath
     * @return the paragraph to add to the pdf file
     * @throws IOException
     */
    private String analyseGoodComment(String filePath) throws IOException {

        String sb = "";
        boolean goodCommentFunctions = this.analyseGoodCommentFunctions(filePath);
        boolean goodCommentInitDoc = this.analyseGoodCommentInitDoc(filePath);
        boolean goodCommentVariables = this.analyseGoodCommentedVariables(filePath);
        boolean goodCommentSubroutines = this.analyseGoodCommentSubroutines(filePath);
        boolean goodCommentControlStructures = this.analyseGoodCommentControlStructures(filePath);

        /**
         * good comment in functions
         */
        sb = "\n\t--> " + this.messages.getString("function") + goodCommentFunctions;

        /**
         * good comment at the begining of the document
         */
        sb += "\n\t--> " + this.messages.getString("initDoc") + goodCommentInitDoc;

        /**
         * good comment at variables declaration
         */
        sb += "\n\t--> " + this.messages.getString("variables") + goodCommentVariables;

        /**
         * good comment soubroutines declaration
         */
        sb += "\n\t--> " + this.messages.getString("commentSubroutines") + goodCommentSubroutines;

        /**
         * good comment in control structures
         */
        sb += "\n\t--> " + this.messages.getString("commentControlStructures") + goodCommentControlStructures;

        /**
         * 1. comments in functions
         */
        if (goodCommentFunctions) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsfunction.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsfunction.add(0.0);
        }
        /**
         * 2. comments at the begining of the document
         */
        if (goodCommentInitDoc) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsBeginning.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsBeginning.add(0.0);
        }
        /**
         * 3. comments in variables
         */
        if (goodCommentVariables) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsVariables.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsVariables.add(0.0);
        }
        /**
         * 4. comments in subroutines
         */
        if (goodCommentSubroutines) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsSubroutine.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsSubroutine.add(0.0);
        }
        /**
         * 5. comments in control structures
         */
        if (goodCommentControlStructures) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoresCommentsControlStructures.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoresCommentsControlStructures.add(0.0);
        }

        return sb;

    }

    /**
     * This method analyse if the Control Structures are commented: ifs and
     * switch case
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseGoodCommentControlStructures(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numControlStructures = 0;
        int totalControlStructures = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                //check if it is a if structure declaration
                //or a select case structutre declaration
                if ((!chain.contains("!")
                        && !chain.contains("ENDIF")
                        && chain.contains("IF ("))
                        || (!chain.contains("!")
                        && !chain.contains("END SELECT")
                        && chain.contains("SELECT CASE"))) {
                    totalControlStructures++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numControlStructures++;
                    }
                }
                previousChain = chain;
            }
        }

        return totalControlStructures == numControlStructures;
    }

    /**
     * This method analyse if the declaration of subroutines are commented
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseGoodCommentSubroutines(String filePath) throws IOException {

        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numSubroutines = 0;
        int totalSubroutines = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                //check if it is a declaration of a function
                if (!chain.contains("!")
                        && !chain.contains("END SUBROUTINE")
                        && chain.contains("SUBROUTINE")) {
                    totalSubroutines++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numSubroutines++;
                        this.commentedElements++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalSubroutines == numSubroutines;
    }

    /**
     * This method analyse if for each variable, there is a comment to describe
     * what it done.
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseGoodCommentedVariables(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int variablesCommented = 0;
        int totalVariables = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                //check if the chain is a declaration of a variable and
                //it is not a comment 
                if (chain.contains("::")) {

                    totalVariables++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        variablesCommented++;
                        this.commentedElements++;
                    }

                }
                previousChain = chain;
            }
        }
        return totalVariables == variablesCommented;
    }

    /**
     * This method analyse if there is a good comment at the beginning of a file
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseGoodCommentInitDoc(String filePath) throws IOException {

        String chain = "";
        int count = 0;
        int ite = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

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

    /**
     * This method check if the functions delcared in a file have or not have a
     * comment. The comment can be after or before the declaration of the
     * function. In addition, at the end of functions there are no comments.
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    public boolean analyseGoodCommentFunctions(String filePath) throws IOException {

        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numFunction = 0;
        int totalFunctions = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();
                //check if it is a declaration of a function
                if (!chain.contains("!")
                        && !chain.contains("END FUNCTION")
                        && chain.contains("FUNCTION")) {
                    totalFunctions++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numFunction++;
                        this.commentedElements++;
                    }
                }
                previousChain = chain;
            }
        }
        return totalFunctions == numFunction;
    }

    /**
     * This method transform a time in miliseconds in days, hours, minutes and
     * seconds
     *
     * @param millis the time to transform
     * @return the time in days, hours, minutes and seconds
     */
    public static String getDurationAnalyse(long millis) {

        if (millis < 0) {
            throw new IllegalArgumentException("Duration must be greater than zero!");
        }

        long days = TimeUnit.MILLISECONDS.toDays(millis);
        millis -= TimeUnit.DAYS.toMillis(days);
        long hours = TimeUnit.MILLISECONDS.toHours(millis);
        millis -= TimeUnit.HOURS.toMillis(hours);
        long minutes = TimeUnit.MILLISECONDS.toMinutes(millis);
        millis -= TimeUnit.MINUTES.toMillis(minutes);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(millis);
        millis -= TimeUnit.MILLISECONDS.toMillis(seconds);

        StringBuilder sb = new StringBuilder(64);
        sb.append(days);
        sb.append(" D ");
        sb.append(hours);
        sb.append(" h ");
        sb.append(minutes);
        sb.append(" min ");
        sb.append(seconds);
        sb.append(" s ");
        sb.append(millis);
        sb.append(" ms");

        return sb.toString();
    }

    /**
     * this method calculate the average of the values in a list.
     *
     * @param l
     * @return
     */
    private Double calculateAverage(ArrayList<Double> l) {

        Double aux = 0.0;

        for (int i = 0; i < l.size(); i++) {
            aux += l.get(i);
        }

        return aux / l.size();
    }

}
