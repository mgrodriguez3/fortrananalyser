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
 * @version 1.9
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
     * the final calification obtains by the directory.
     */
    private double finalCalification = 0.0;

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
    private ArrayList<Double> scoreRatio;

    /**
     * the list with all scores obtain by nested loops metric.
     */
    private ArrayList<Double> scoreNestedLoops;

    /**
     * the list with all scores obtain by comments metric at the beginning.
     */
    private ArrayList<Double> scoreCommentsBeginning;

    /**
     * the list with all scores obtain by comments metric in variables.
     */
    private ArrayList<Double> scoreCommentsVariables;

    /**
     * the list with all scores obtain by comments metric in functions.
     */
    private ArrayList<Double> scoreCommentsfunction;

    /**
     * the list with all scores obtain by comments metric in subroutine.
     */
    private ArrayList<Double> scoreCommentsSubroutine;

    /**
     * the list with all scores obtain by control structures metric.
     */
    private ArrayList<Double> scoreCommentsControlStructures;

    /**
     * the list with scores obtain by exit metric.
     */
    private ArrayList<Double> scoreExit;

    /**
     * the list with scores obtain by cycle metric.
     */
    private ArrayList<Double> scoreCycle;

    /**
     * Number of control structure
     */
    private int totalControlStructures = 0;

    /**
     * the string resources i18n.
     */
    ResourceBundle messages;

    TasksBar(Window w, String path, ResourceBundle messages) {

        this.scores = new ArrayList<>();
        this.scoresImplicitNone = new ArrayList<>();
        this.scoreRatio = new ArrayList<>();
        this.scoreNestedLoops = new ArrayList<>();
        this.scoreCommentsBeginning = new ArrayList<>();
        this.scoreCommentsVariables = new ArrayList<>();
        this.scoreCommentsfunction = new ArrayList<>();
        this.scoreCommentsSubroutine = new ArrayList<>();
        this.scoreCommentsControlStructures = new ArrayList<>();
        this.scoreExit = new ArrayList<>();
        this.scoreCycle = new ArrayList<>();

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

    /**
     * Empty constructor
     */
    public TasksBar() {
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    /**
     * In background, this method obtains all the files in an specific directory
     * and subdirectories and analyse them where the extension of the files are
     * ".f90" or ".h90"
     *
     *
     * @return null
     * @throws java.lang.Exception in case a lists can not be cleared
     */
    @Override
    protected Void doInBackground() throws Exception {

        this.scoresImplicitNone.clear();
        this.scoreRatio.clear();
        this.scoreNestedLoops.clear();
        this.scoreCommentsBeginning.clear();
        this.scoreCommentsVariables.clear();
        this.scoreCommentsfunction.clear();
        this.scoreCommentsSubroutine.clear();
        this.scoreCommentsControlStructures.clear();
        this.scoreExit.clear();
        this.scoreCycle.clear();

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
                        pdf.addResult(analyseFile(file.getAbsolutePath()));
                        pdf.addTableScore(scores, this.messages);
                        countNumberOfFiles++;
                        pdf.addScoreResult(this.messages.getString("noteFile") + String.format(Locale.ROOT, "%.2f", assesment));
                        finalCalification += assesment;
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
            this.scores.add(this.calculateAverage(this.scoreNestedLoops));
            this.scores.add(this.calculateAverage(this.scoreCommentsfunction));
            this.scores.add(this.calculateAverage(this.scoreCommentsBeginning));
            this.scores.add(this.calculateAverage(this.scoreCommentsVariables));
            this.scores.add(this.calculateAverage(this.scoreCommentsSubroutine));
            this.scores.add(this.calculateAverage(this.scoreCommentsControlStructures));
            this.scores.add(this.calculateAverage(this.scoreExit));
            this.scores.add(this.calculateAverage(this.scoreCycle));
            this.scores.add(this.calculateAverage(this.scoreRatio));

            if (!this.scores.get(0).isNaN()) {
                pdf.addSection(this.messages.getString("finalTable"));
                pdf.addFinalTableScore(this.scores, this.messages);
                auxNote = finalCalification / countNumberOfFiles;
                pdf.addFinalNote(this.messages.getString("arithmeticAverage") + " " + String.format(Locale.ROOT, "%.2f", auxNote));
            }

            pdf.closePDF();
            finalCalification = 0.0;
            percentage = 100;
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
     * @param chunks the value of the progress bar
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
                + "</span></html>" + "\n"
                + "\n<html> <span style='color:#089650'>" + messages.getString("arithmeticAverage") + String.format(Locale.ROOT, "%.2f", auxNote) + "</span></html>",
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
     * @return String
     */
    private static String getPathFromFile(File file) {

        return file.getAbsolutePath().
                substring(0, file.getAbsolutePath().lastIndexOf(File.separator));

    }

    /**
     * This method obtains the extension of a file
     *
     * @param file the file that we want to check the extension
     * @return String
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
     * @throws IOException when file can not open
     */
    public String analyseFile(String pathFile) throws IOException {

        String result = "";
        assesment = 0.0;
        double ratio = this.analyseRatio(pathFile);
        int numLines = this.analyseNumberOfLines(pathFile);
        boolean useImplicitNone = this.analyseUseImplicitNone(pathFile);
        boolean checkNestedLoops = this.analyseNestedLoops(pathFile);
        boolean useExit = this.analyseUseExit(pathFile);
        boolean useCycle = this.analyseUseCycle(pathFile);

        /**
         * 1.- count the number of lines in the file
         */
        result += this.messages.getString("numberOfLines") + numLines;
        result += "\n";

        /**
         * 2. Use or not use the sentence IMPLICIT NONE
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
         * 3.- count the number of functions declared
         */
        result += this.messages.getString("numFunctions") + this.analyseNumFunctions(pathFile);
        result += "\n";

        /**
         * 4.- count the number of subroutines calls
         */
        result += this.messages.getString("subroutinesCall") + this.analyseNumCalls(pathFile);
        result += "\n";

        /**
         * 5.- count the number of variables declared
         */
        result += this.messages.getString("numVariables") + this.analyseNumberOfDeclaredVariables(pathFile);
        result += "\n";

        /**
         * 6.- check the Nested loops
         */
        result += this.messages.getString("nestedLoops") + checkNestedLoops;
        result += "\n";

        /**
         * in case the nestedLoop are ok
         */
        if (checkNestedLoops) {
            assesment += 2.0;
            this.scores.add(2.0);
            this.scoreNestedLoops.add(2.0);
        } else {
            this.scores.add(0.0);
            this.scoreNestedLoops.add(0.0);
        }

        /**
         * 7.- good comments in file
         */
        result += this.messages.getString("goodComments") + this.analyseGoodComment(pathFile);
        result += "\n";

        /**
         * 8.- check the number of declared subroutines
         */
        result += this.messages.getString("subroutines") + this.analyseNumberSubroutines(pathFile);
        result += "\n";

        /**
         * 9.- check the use of EXIT
         */
        result += this.messages.getString("exit") + useExit;
        result += "\n";

        /**
         * in case the sentece EXIT is used
         */
        if (useExit) {
            assesment += 1.0;
            this.scores.add(1.0);
            this.scoreExit.add(1.0);
        } else {
            this.scores.add(0.0);
            this.scoreExit.add(0.0);
        }

        /**
         * 10.- check the use of CYCLE
         */
        result += this.messages.getString("cycle") + useCycle;
        result += "\n";

        /**
         * in case the sentence CYCLE is used
         */
        if (useCycle) {
            assesment += 1.0;
            this.scores.add(1.0);
            this.scoreCycle.add(1.0);
        } else {
            this.scores.add(0.0);
            this.scoreCycle.add(0.0);
        }

        /**
         * 11.- ratio of number of lines with comments against computable
         * elements
         */
        result += this.messages.getString("ratio") + ((ratio * 100) / 2.0) + " %";
        result += " \n";

        assesment += ratio;
        this.scores.add(ratio);
        this.scoreRatio.add(ratio);

        return result;

    }

    /**
     * This method obtains the number of lines of a file
     *
     * @param filePath the path of the file
     * @return the number of lines from file
     * @throws IOException  when file can not open
     */
    public int analyseNumberOfLines(String filePath) throws IOException {

        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while (b.readLine() != null) {
                count++;
            }

            b.close();
        }

        return count;
    }

    /**
     * This method analyse if the sentence implicit none is used in each line
     * from a file
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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
            b.close();
        }
        return false;
    }

    /**
     * This method analyse the number of functions in file filePath
     *
     * @param filePath The path from file to analyse
     * @return int
     * @throws IOException when file can not open
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
                        && (!chain.contains("END FUNCTION")
                        && chain.contains("FUNCTION"))) {
                    count++;
                }
            }
            b.close();
        }

        return (count / 2);
    }

    /**
     * This method analyse the number of subroutines are called in this file
     *
     * @param filePath the absolute path from file
     * @return int
     * @throws IOException when file can not open
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
            b.close();
        }

        return count;
    }

    /**
     * This methos analyse the number of comments are in a file
     *
     * @param filePath the absolute path of the file
     * @return int
     * @throws IOException when file can not open
     */
    public int analyseNumComments(String filePath) throws IOException {
        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("!")) {
                    count++;
                }
            }
            b.close();
        }

        return count;
    }

    /**
     * This method analyse the use of the CYCLE sentence in the file. It is used
     * in loops to avoid making a certain sentence, so that it continues to
     * iterate to the next element. With they use, the code is more efficient.
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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
            b.close();
        }

        return numLoops == numCycles;
    }

    /**
     * This method check if the EXIT sentence is used in the file. EXIT sentence
     * is used to go out of a loop, so the code is more efficient
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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
            b.close();
        }
        return numLoops == numExit;
    }

    /**
     * This method analyse the number of subroutines declared in a file
     *
     * @param filePath is the path from file to analyse
     * @return int
     * @throws IOException when file can not open
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
            b.close();
        }

        return count;
    }

    /**
     * This method analyse the number of Nested loops there are. If this number
     * is greater than 3 or smaller than 0 AND this line don't have a comment ,
     * it is consider a bad programming practice.
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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
            b.close();
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
     * @param filePath is the path from file to analyse
     * @return int
     * @throws IOException when file can not open
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
            b.close();
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
     * @param filePath is the path from file to analyse
     * @return String
     * @throws IOException when file can not open
     */
    private String analyseGoodComment(String filePath) throws IOException {

        String sb = "";
        boolean goodCommentFunctions = this.analyseGoodCommentFunctions(filePath);
        boolean goodCommentInitDoc = this.analyseGoodCommentInitDoc(filePath);
        boolean goodCommentVariables = this.analyseGoodCommentedVariables(filePath);
        boolean goodCommentSubroutines = this.analyseGoodCommentSubroutines(filePath);
        boolean goodCommentControlStructures = this.analyseGoodCommentControlStructures(filePath);

        sb = "\n\t--> " + this.messages.getString("function") + goodCommentFunctions;
        sb += "\n\t--> " + this.messages.getString("initDoc") + goodCommentInitDoc;
        sb += "\n\t--> " + this.messages.getString("variables") + goodCommentVariables;
        sb += "\n\t--> " + this.messages.getString("commentSubroutines") + goodCommentSubroutines;
        sb += "\n\t--> " + this.messages.getString("commentControlStructures") + goodCommentControlStructures;

        if (goodCommentFunctions) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoreCommentsfunction.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoreCommentsfunction.add(0.0);
        }
        if (goodCommentInitDoc) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoreCommentsBeginning.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoreCommentsBeginning.add(0.0);
        }
        if (goodCommentVariables) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoreCommentsVariables.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoreCommentsVariables.add(0.0);
        }
        if (goodCommentSubroutines) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoreCommentsSubroutine.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoreCommentsSubroutine.add(0.0);
        }
        if (goodCommentControlStructures) {
            assesment += 0.4;
            this.scores.add(0.4);
            this.scoreCommentsControlStructures.add(0.4);
        } else {
            this.scores.add(0.0);
            this.scoreCommentsControlStructures.add(0.0);
        }
        return sb;

    }

    /**
     * This method analyse if the Control Structures are commented: ifs and
     * switch case
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
     */
    public boolean analyseGoodCommentControlStructures(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numControlStructures = 0;

        FileReader fr = new FileReader(file);

        this.totalControlStructures = 0;

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
                    this.totalControlStructures++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numControlStructures++;
                    }
                }
                previousChain = chain;
            }
            b.close();
        }

        return this.totalControlStructures == numControlStructures;
    }

    /**
     * This method analyse if the declaration of subroutines are commented
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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
                    }
                }
                previousChain = chain;
            }
            b.close();
        }
        return totalSubroutines == numSubroutines;
    }

    /**
     * This method analyse if for each variable, there is a comment to describe
     * what it done.
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
     */
    public boolean analyseGoodCommentedVariables(String filePath) throws IOException {
        String chain = "";
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

                    if (chain.contains("!")) {
                        variablesCommented++;
                    }

                }
            }
            b.close();
        }
        return totalVariables == variablesCommented;
    }

    /**
     * This method analyse if there is a good comment at the beginning of a file
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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
            b.close();
        }

        return count > 1;
    }

    /**
     * This method check if the functions delcared in a file have or not have a
     * comment. The comment can be after or before the declaration of the
     * function. In addition, at the end of functions there are no comments.
     *
     * @param filePath is the path from file to analyse
     * @return boolean
     * @throws IOException when file can not open
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

                    //check if the previous line is a comment
                    if (previousChain.contains("!")) {
                        numFunction++;
                    }
                }
                previousChain = chain;
            }
            b.close();
        }
        return totalFunctions == numFunction;
    }

    /**
     * This method transform a time in miliseconds in days, hours, minutes and
     * seconds
     *
     * @param millis the time to transform
     * @return String
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
     * @param l list of scores to calculate the average
     * @return double 
     */
    private double calculateAverage(ArrayList<Double> l) {

        double aux = 0.0;

        for (int i = 0; i < l.size(); i++) {
            aux += l.get(i);
        }

        return aux / l.size();
    }

    /**
     * this method analyse the relation between the number of coments in file
     * and the number of comentable elements.
     *
     * @param filePath is the path of file to analyse the ratio
     * @return double
     * @throws IOException when file can not open
     */
    public double analyseRatio(String filePath) throws IOException {

        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numElementscomentable = 0;
        int comentableElements = 0;

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {

            while ((chain = b.readLine()) != null) {

                chain = chain.toUpperCase();

                //check if it is a commentable element
                if (!chain.contains("!")
                        && (chain.contains("::"))) {

                    comentableElements++;

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (previousChain.contains("!")) {
                        numElementscomentable++;
                    }

                }

                previousChain = chain;

            }
            b.close();
        }
        return (numElementscomentable / comentableElements) * 2.0;

    }

}
