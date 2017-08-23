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
     * the path of the destination file.
     */
    public static final String DEST = System.getProperty("user.home") + "/temp/QualityReport.pdf";

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
    private ArrayList<Double> scores = new ArrayList<>();

    /**
     * the string resources i18n.
     */
    ResourceBundle messages;

    TasksBar(Window w, String path, ResourceBundle messages) {

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
        //this.taskbar.setUndecorated(true);
        this.taskbar.setVisible(true);

    }

    /**
     * In background, this method obtains all the files in an specific directory
     * and subdirectories and analyse them where the extension of the files are
     * ".f90" or ".h90"
     *
     *
     * @return the note of the
     * @throws java.lang.Exception
     */
    @Override
    protected Void doInBackground() throws Exception {

        PDF pdf;
        int countNumberOfFiles = 0;
        auxNote = 0.0;
        double percentage = 0.0;

        try {

            List<File> filesInFolder;
            String auxDir = "";
            pdf = new PDF();

            //start the duration of the analysis
            timeStart = System.currentTimeMillis();

            percentage += 1.0;
            publish((int) percentage);

            pdf.createPdf(TasksBar.DEST);

            filesInFolder = Files.walk(Paths.get(this.path))
                    .filter(Files::isRegularFile)
                    .map(java.nio.file.Path::toFile)
                    .collect(Collectors.toList());

            /**
             * for each file of the directory and subdirectory
             */
            for (File file : filesInFolder) {

                /**
                 * If it is a new directory, the path is added into the report.
                 */
                if (!auxDir.equals(getPathFromFile(file))
                        && (getFileExtension(file).equals(TasksBar.EXTENSION)
                        || getFileExtension(file).equals(TasksBar.EXTENSION2)
                        || getFileExtension(file).equals(TasksBar.EXTENSION.toUpperCase())
                        || getFileExtension(file).equals(TasksBar.EXTENSION2.toUpperCase()))) {
                    auxDir = getPathFromFile(file);
                    pdf.addSection(auxDir);
                }

                /**
                 * If it is a new file of fortran code, the path is added into
                 * the report.
                 */
                if (getFileExtension(file).equals(TasksBar.EXTENSION)
                        || getFileExtension(file).equals(TasksBar.EXTENSION2)
                        || getFileExtension(file).equals(TasksBar.EXTENSION.toUpperCase())
                        || getFileExtension(file).equals(TasksBar.EXTENSION2.toUpperCase())) {
                    pdf.addSubSection(file.getName());
                    pdf.addResult(analyseFile(file.getAbsolutePath()));
                    pdf.addTableScore(scores, this.messages);

                    countNumberOfFiles++;
                    pdf.addResult(this.messages.getString("noteFile") + String.format("%.2f", assesment));
                    finalCalification += assesment;
                }

                percentage += 98.0 / filesInFolder.size();
                publish((int) percentage);
            }
            auxNote = finalCalification / countNumberOfFiles;
            pdf.addFinalNote(this.messages.getString("arithmeticAverage") + String.format("%.2f", auxNote));
            pdf.closePDF();
            finalCalification = 0.0;

            percentage = 100;
            publish((int) percentage);

        } catch (IOException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }

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
                + messages.getString("timeMessage") + TasksBar.getDurationAnalyse(timeStop)
                + "\n<html> <span style='color:#089650'>" + messages.getString("arithmeticAverage") + String.format("%.2f", auxNote) + "</span></html>",
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
        double percentage;
        int numLines = this.analyseNumberOfLines(pathFile);
        boolean useImplicitNone = this.analyseUseImplicitNone(pathFile);
        int numComments = this.analyseNumComments(pathFile);
        boolean checkNestedLoops = this.analyseNestedLoops(pathFile);
        boolean useExit = this.analyseUseExit(pathFile);
        boolean useCycle = this.analyseUseCycle(pathFile);

        /**
         * 1.- count the number of lines in the file
         */
        result += this.messages.getString("numberOfLines") + numLines;
        result += "\n";

        /**
         * Use or not use the sentence IMPLICIT NONE
         */
        result += this.messages.getString("numberOfLines") + useImplicitNone;
        result += "\n";

        /**
         * in case the sentente IMPLICIT NONE is used
         */
        if (useImplicitNone) {
            assesment += 2.0;
            scores.add(2.0);
        } else {
            scores.add(0.0);
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
         * 5.- count the number of comments
         */
        result += this.messages.getString("comments") + numComments;
        result += " \n";

        percentage = (numComments * 100) / numLines;

        if (percentage > 20) {
            assesment += 2.0;
            scores.add(2.0);
        } else {
            double auxNum = (95.23 * percentage) / 20;
            scores.add(auxNum);
        }

        /**
         * 6.- count the number of variables declared
         */
        result += this.messages.getString("numVariables") + this.analyseNumberOfDeclaredVariables(pathFile);
        result += "\n";

        /**
         * 7.- good comments in file
         */
        result += this.messages.getString("goodComments") + this.analyseGoodComment(pathFile);
        result += "\n";

        /**
         * 8.- check the Nested loops
         */
        result += this.messages.getString("nestedLoops") + checkNestedLoops;
        result += "\n";

        /**
         * in case the nestedLoop are ok
         */
        if (checkNestedLoops) {
            assesment += 2.0;
            scores.add(2.0);
        } else {
            scores.add(0.0);
        }

        /**
         * 9.- check the number of declared subroutines
         */
        result += this.messages.getString("subroutines") + this.analyseNumberSubroutines(pathFile);
        result += "\n";

        /**
         * 10.- check the use of EXIT
         */
        result += this.messages.getString("exit") + useExit;
        result += "\n";

        /**
         * in case the sentece EXIT is used
         */
        if (useExit) {
            assesment += 1.0;
            scores.add(1.0);
        } else {
            scores.add(0.0);
        }

        /**
         * 11.- check the use of CYCLE
         */
        result += this.messages.getString("cycle") + useCycle;
        result += "\n";

        /**
         * in case the sentence CYCLE is used
         */
        if (useCycle) {
            assesment += 1.0;
            scores.add(1.0);
        } else {
            scores.add(0.0);
        }

        return result;

    }

    /**
     * This method obtains the number of lines of a file
     *
     * @param filePath the path of the file
     * @return the number of lines from file
     * @throws IOException
     */
    private int analyseNumberOfLines(String filePath) throws IOException {

        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
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
    private boolean analyseUseImplicitNone(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (!chain.contains("!") && (chain.contains("IMPLICIT NONE")) || (chain.contains("implicit none"))) {
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
    private int analyseNumFunctions(String filePath) throws IOException {

        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (!chain.contains("!")
                        && (!chain.contains("END FUNCTION") || !chain.contains("end function"))
                        && (chain.contains("FUNCTION") || chain.contains("function"))) {
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
    private int analyseNumCalls(String filePath) throws IOException {
        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (!chain.contains("!") && (chain.contains("CALL") || chain.contains("call"))) {
                    count++;
                }
            }
        }

        return count;
    }

    /**
     * This methos analyse the number of comments are in a file
     *
     * @param filePath the absolute path of the file
     * @return the number of comments
     * @throws IOException
     */
    private int analyseNumComments(String filePath) throws IOException {
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
    private boolean analyseUseCycle(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                /**
                 * check if the chain is a declaration of a variable and it is
                 * not a comment
                 */
                if ((chain.contains("CYCLE") || chain.contains("cycle"))
                        && !chain.contains("!")) {

                    return true;
                }
            }
        }
        return false;
    }

    /**
     * This method check if the EXIT sentence is used in the file. EXIT sentence
     * is used to go out of a loop, so the code is more efficient
     *
     * @param filePath
     * @return boolean
     * @throws IOException
     */
    private boolean analyseUseExit(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                //check if the chain is a declaration of a variable and
                //it is not a comment 
                if ((chain.contains("EXIT") || chain.contains("exit"))
                        && !chain.contains("!")) {

                    return true;
                }
            }
        }
        return false;
    }

    /**
     * This method analyse the number of subroutines declared in a file
     *
     * @param filePath
     * @return the number of subroutines
     * @throws IOException
     */
    private int analyseNumberSubroutines(String filePath) throws IOException {

        String chain = "";
        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if ((chain.contains("subroutine") || chain.contains("SUBROUTINE"))
                        && !chain.contains("!")
                        && (chain.contains("end subroutine") || chain.contains("END SUBROUTINE"))) {
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
    private boolean analyseNestedLoops(String filePath) throws IOException {

        String chain = "";
        int nestedLoops = 0;
        File file = new File(filePath);
        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

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

        return true;
    }

    /**
     * This method count the number of declared variables in a file
     *
     * @param filePath
     * @return the number of declared variables
     * @throws IOException
     */
    private int analyseNumberOfDeclaredVariables(String filePath) throws IOException {
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

        sb = "\n\t--> " + this.messages.getString("function") + goodCommentFunctions;
        sb += "\n\t--> " + this.messages.getString("initDoc") + goodCommentInitDoc;
        sb += "\n\t--> " + this.messages.getString("variables") + goodCommentVariables;
        sb += "\n\t--> " + this.messages.getString("commentSubroutines") + goodCommentSubroutines;
        sb += "\n\t--> " + this.messages.getString("commentControlStructures") + goodCommentControlStructures;

        if (goodCommentFunctions) {
            assesment += 0.4;
            scores.add(0.4);
        } else {
            scores.add(0.0);
        }
        if (goodCommentInitDoc) {
            assesment += 0.4;
            scores.add(0.4);
        } else {
            scores.add(0.0);
        }
        if (goodCommentVariables) {
            assesment += 0.4;
            scores.add(0.4);
        } else {
            scores.add(0.0);
        }
        if (goodCommentSubroutines) {
            assesment += 0.4;
            scores.add(0.4);
        } else {
            scores.add(0.0);
        }
        if (goodCommentControlStructures) {
            assesment += 0.4;
            scores.add(0.4);
        } else {
            scores.add(0.0);
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
    private boolean analyseGoodCommentControlStructures(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numControlStructures = 0;
        int totalControlStructures = 0;
        String nextLine = "";

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                //check if it is a if structure declaration
                //or a select case structutre declaration
                if ((!chain.contains("!")
                        && !chain.contains("endif")
                        && !chain.contains("ENDIF")
                        && (chain.contains("if (")
                        || chain.contains("IF (")))
                        || (!chain.contains("!")
                        && !chain.contains("end select")
                        && !chain.contains("END SELECT")
                        && (chain.contains("select case")
                        || chain.contains("SELECT CASE")))) {
                    totalControlStructures++;

                    if (b.readLine() == null) {
                        nextLine = "";
                    }

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (nextLine.contains("!") || previousChain.contains("!")) {
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
    private boolean analyseGoodCommentSubroutines(String filePath) throws IOException {
        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numSubroutines = 0;
        int totalSubroutines = 0;
        String nextLine = "";

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                //check if it is a declaration of a function
                if (!chain.contains("!")
                        && !chain.contains(" end subroutine")
                        && !chain.contains("END SUBROUTINE")
                        && (chain.contains("subroutine")
                        || chain.contains("SUBROUTINE"))) {
                    totalSubroutines++;

                    if (b.readLine() == null) {
                        nextLine = "";
                    }

                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (nextLine.contains("!") || previousChain.contains("!")) {
                        numSubroutines++;
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
    private boolean analyseGoodCommentedVariables(String filePath) throws IOException {
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
    private boolean analyseGoodCommentInitDoc(String filePath) throws IOException {

        String chain = "";
        int count = 0;
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null && count < 2) {

                if (chain.contains("!")) {
                    count++;
                }
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
    private boolean analyseGoodCommentFunctions(String filePath) throws IOException {

        String chain = "";
        String previousChain = "";
        File file = new File(filePath);
        int numFunction = 0;
        int totalFunctions = 0;
        String nextLine = "";

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {

                //check if it is a declaration of a function
                if (!chain.contains("!")
                        && !chain.contains(" end function")
                        && !chain.contains("END FUNCTION")
                        && (chain.contains("function")
                        || chain.contains("FUNCTION"))) {
                    totalFunctions++;

                    if (b.readLine() == null) {
                        nextLine = "";
                    }
                    //check if the next line is a comment or the previous line
                    //is a comment
                    if (nextLine.contains("!") || previousChain.contains("!")) {
                        numFunction++;
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

        StringBuilder sb = new StringBuilder(64);
        sb.append(days);
        sb.append(" D ");
        sb.append(hours);
        sb.append(" h ");
        sb.append(minutes);
        sb.append(" min ");
        sb.append(seconds);
        sb.append(" s");

        return sb.toString();
    }

}
