/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import java.awt.Color;
import java.awt.Desktop;
import java.util.List;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.UIManager;

/**
 * Class that creates Jframe windows
 *
 * @author Michael García Rodríguez
 * @version 1.0
 */
public class Window extends JFrame implements ActionListener {

    /**
     * Variables from the class Window
     */
    private final static ImageIcon ICON_EPHYSLAB
            = new ImageIcon(Window.class.getResource("ephysLab.png"));
    private final static String APP_NAME = "Fortran Analyser tool";
    private final JFileChooser fc = new JFileChooser();
    private JLabel text;
    private JTextField box;
    private JButton buttonanalyse;
    private JButton buttonExit;
    private JButton buttonFileExplorer;
    private JMenuBar mb;
    private JMenu menuLanguages;
    private JMenuItem spanish, galician, english, french;
    private static final String EXTENSION = "f90";
    private static final String EXTENSION2 = "h90";
    private static final String DEST = System.getProperty("user.home") + "/temp/QualityReport.pdf";
    private double assesment = 0.0;
    private double finalCalification = 0.0;

    /**
     * By default, the selected language is Spanish
     */
    private String selectedLanguage = "es";
    private String selectDirectory = "Seleccione un directorio: ";
    private String nameButtonAnalyse = "Analizar";
    private String nameButtonExit = "Salir";
    private final String nameButtonSpanish = "Castellano";
    private final String nameButtonEnglish = "English";
    private final String nameButtonFrench = "Français";
    private final String nameButtonGalician = "Galego";
    private String nameMenu = "Idioma";
    private String errorDirectoryEmpty = "Seleccione un directorio";
    private String exitMessage = "Análisis realizado.";
    private String directoryMessage = "\nDocumento guardado en:\n" + Window.DEST;
    private String timeMessage = "\nTiempo: ";
    private String numberOfLines = "Número de líneas: ";
    private final String implicitNone = "Implicit none: ";
    private String funtions = "Número de funciones: ";
    private String comments = "Número de comentarios: ";
    private String subroutinesCall = "Número de llamadas a subrutinas: ";
    private String subroutines = "Número de subrutinas: ";
    private String goodComments = "Bien comentado en: ";
    private String function = "funciones declaradas: ";
    private String numVariables = "Número de variables declaradas: ";
    private String initDoc = "el inicio del archivo: ";
    private String variables = "declaración de variables:";
    private String nestedLoops = "Cumple con la complejidad máxima de anidamiento de los bucles y formato de los comentarios: ";
    private String commentSubroutines = "declaración de subrutinas: ";
    private String commentControlStructures = "Estructuras de control: ";
    private String exit = "Utiliza la sentencia EXIT para salir de los bucles antes de tiempo: ";
    private String cycle = "Utiliza la sentencia CYCLE para evitar realizar determinadas sentencias, iterando al siguiente elemento: ";
    private String arithmeticAverage = "Nota final: ";
    private String headMessageDialog = "Información del análisis";
    private String noteFile = "Nota del archivo: ";

    /**
     * Constructor from Class
     *
     * @throws IOException
     */
    public Window() throws IOException {

        super();
        configureWindow();
        initialiceComponents();
    }

    /**
     * This method set settings of the main windows
     *
     * @throws IOException
     */
    private void configureWindow() throws IOException {

        this.setTitle(APP_NAME);
        this.setSize(400, 250);
        this.setContentPane(new JLabel(ICON_EPHYSLAB));
        this.setLocationRelativeTo(null);
        this.setResizable(false);
        this.setLayout(null);
        this.setResizable(false);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setIconImage(new ImageIcon(Window.class.getResource("fortranAnalyserIcon.png")).getImage());
        
    }

    /**
     * initialice all the components that form the main window
     */
    private void initialiceComponents() {

        // create components
        this.text = new JLabel();
        this.box = new JTextField();
        this.buttonanalyse = new JButton();
        this.buttonExit = new JButton();
        this.buttonFileExplorer = new JButton();

        setLayout(null);
        this.mb = new JMenuBar();
        setJMenuBar(mb);

        // configure components
        this.text.setText(this.getSelectDirectory());
        this.text.setBounds(25, 25, 300, 25);

        this.box.setBounds(25, 50, 250, 25);
        this.box.setEditable(false);

        this.buttonanalyse.setText(this.getNameButtonAnalyse());
        this.buttonanalyse.setBounds(50, 100, 200, 30);
        this.buttonanalyse.addActionListener(this);

        this.buttonExit.setText(this.getNameButtonExit());
        this.buttonExit.setBounds(50, 150, 200, 30);
        this.buttonExit.addActionListener(this);

        this.buttonFileExplorer.setText("...");
        this.buttonFileExplorer.setBounds(300, 50, 50, 25);
        this.buttonFileExplorer.addActionListener(this);

        this.fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        //Configure MenuBar
        this.menuLanguages = new JMenu(this.getNameMenu());
        this.mb.add(menuLanguages);

        this.spanish = new JMenuItem(this.getNameButtonSpanish());
        this.spanish.addActionListener(this);
        this.menuLanguages.add(spanish);

        this.galician = new JMenuItem(this.getNameButtonGalician());
        this.galician.addActionListener(this);
        this.menuLanguages.add(galician);

        this.english = new JMenuItem(this.getNameButtonEnglish());
        this.english.addActionListener(this);
        this.menuLanguages.add(english);

        this.french = new JMenuItem(this.getNameButtonFrench());
        this.french.addActionListener(this);
        this.menuLanguages.add(french);

        // add all components in the JFrame
        this.add(text);
        this.add(box);
        this.add(buttonanalyse);
        this.add(buttonExit);
        this.add(buttonFileExplorer);

    }

    /**
     * Override of the method actionPerformed. it define the action to do when
     * an event button happens
     *
     * @param e the event to push a button
     */
    @Override
    public void actionPerformed(ActionEvent e) {

        //button analyse is pulsed
        if (e.getSource().equals(buttonanalyse)) {
            if (!this.box.getText().isEmpty()) {
                analizeDirectories(this.box.getText());

                if (Desktop.isDesktopSupported()) {
                    try {
                        File myFile = new File(Window.DEST);
                        Desktop.getDesktop().open(myFile);
                    } catch (IOException ex) {
                        Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            } else {
                JOptionPane.showMessageDialog(this, this.getErrorDirectoryEmpty());
            }

        }

        //button exit is pulsed
        if (e.getSource().equals(buttonExit)) {
            System.exit(0);
        }

        //button file explorer is pulsed
        if (e.getSource().equals(buttonFileExplorer)) {
            int returnVal = this.fc.showOpenDialog(Window.this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = this.fc.getSelectedFile();
                this.box.setText(file.getAbsolutePath());
            }
        }

        //In the language menu, a language is selected
        if (e.getSource().equals(spanish)) {
            this.changeLanguage("es");
        }

        if (e.getSource().equals(french)) {
            this.changeLanguage("fr");
        }

        if (e.getSource().equals(english)) {
            this.changeLanguage("en");
        }

        if (e.getSource().equals(galician)) {
            this.changeLanguage("gl");
        }

    }

    /**
     * Obtains all the files in an specific directory and subdirectories and
     * analyse them where the extension of the files are ".f90" or ".h90"
     *
     * @param directory
     */
    private void analizeDirectories(String directory) {

        PDF pdf;
        int countNumberOfFiles = 0;
        double auxNote = 0.0;

        try {

            List<File> filesInFolder;
            String auxDir = "";
            pdf = new PDF();

            //start the duration of the analysis
            long timeStart = System.currentTimeMillis();

            pdf.createPdf(Window.DEST);

            filesInFolder = Files.walk(Paths.get(directory))
                    .filter(Files::isRegularFile)
                    .map(java.nio.file.Path::toFile)
                    .collect(Collectors.toList());

            for (File file : filesInFolder) {

                if (!auxDir.equals(getPathFromFile(file))
                        && (getFileExtension(file).equals(Window.EXTENSION)
                        || getFileExtension(file).equals(Window.EXTENSION2)
                        || getFileExtension(file).equals(Window.EXTENSION.toUpperCase())
                        || getFileExtension(file).equals(Window.EXTENSION2.toUpperCase()))) {
                    auxDir = getPathFromFile(file);
                    pdf.addSection(auxDir);
                }

                if (getFileExtension(file).equals(Window.EXTENSION)
                        || getFileExtension(file).equals(Window.EXTENSION2)
                        || getFileExtension(file).equals(Window.EXTENSION.toUpperCase())
                        || getFileExtension(file).equals(Window.EXTENSION2.toUpperCase())) {
                    pdf.addSubSection(file.getName());
                    pdf.addResult(analyseFile(file.getAbsolutePath()));
                    countNumberOfFiles++;
                    pdf.addResult(this.getNoteFile() + assesment);
                    finalCalification += assesment;
                }
            }
            auxNote = finalCalification / countNumberOfFiles;
            pdf.addFinalNote(this.getArithmeticAverage() + auxNote);
            pdf.closePDF();
            finalCalification = 0.0;
            long timeStop = System.currentTimeMillis();
            timeStop = timeStop - timeStart;

            UIManager.put("OptionPane.background", Color.white);
            UIManager.put("Panel.background", Color.white);
            ImageIcon icon = new ImageIcon(Window.class.getResource("fortranAnalyserIcon.png"));
            
            
            JOptionPane.showMessageDialog(this, "<html> <span style='color:#007A82'>" + this.getExitMessage() + "</span></html>"
                    + this.getDirectoryMessage()
                    + this.getTimeMessage() + Window.getDurationAnalyse(timeStop)
                    + "\n<html> <span style='color:#089650'>" + this.getArithmeticAverage() + auxNote + "</span></html>",
                    this.getHeadMessageDialog(), JOptionPane.INFORMATION_MESSAGE, icon);

        } catch (IOException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
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
        int numLines = this.analyseNumberOfLines(pathFile);
        boolean useImplicitNone = this.analyseUseImplicitNone(pathFile);
        int numComments = this.analyseNumComments(pathFile);
        boolean checkNestedLoops = this.analyseNestedLoops(pathFile);
        boolean useExit = this.analyseUseExit(pathFile);
        boolean useCycle = this.analyseUseCycle(pathFile);

        //1.- count the number of lines in the file
        result += this.getNumberOfLines() + numLines;
        result += "\n";

        //2.- Use or not use  the sentence IMPLICIT NONE
        result += this.implicitNone + useImplicitNone;
        result += "\n";

        if (useImplicitNone) {
            assesment += 2.0;
        }

        //3.- count the number of functions declared
        result += this.getMethod() + this.analyseNumFunctions(pathFile);
        result += "\n";

        //4.- count the number of subroutines calls
        result += this.getSubroutinesCall() + this.analyseNumCalls(pathFile);
        result += "\n";

        //5.- count the number of comments
        result += this.getComments() + numComments;
        result += " \n";

        if ((numComments * 100) / numLines > 20) {
            assesment += 2.0;
        }

        //6.- count the number of variables declared
        result += this.getNumVariable() + this.analyseNumberOfDeclaredVariables(pathFile);
        result += "\n";

        //7.- good comments in file
        result += this.getGoodComments() + this.analyseGoodComment(pathFile);
        result += "\n";

        //8.- check the Nested loops
        result += this.getNestedLoops() + checkNestedLoops;
        result += "\n";

        if (checkNestedLoops) {
            assesment += 2.0;
        }

        //9.- check the number of declared subroutines
        result += this.getSubroutines() + this.analyseNumberSubroutines(pathFile);
        result += "\n";

        //10.- check the use of EXIT
        result += this.getExit() + useExit;
        result += "\n";

        if (useExit) {
            assesment += 1.0;
        }

        //11.- check the use of CYCLE
        result += this.getCycle() + useCycle;
        result += "\n";

        if (useCycle) {
            assesment += 1.0;
        }

        return result;

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

                //check if the chain is a declaration of a variable and
                //it is not a comment 
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

        sb = "\n\t--> " + this.getFunction() + goodCommentFunctions;
        sb += "\n\t--> " + this.getInitDoc() + goodCommentInitDoc;
        sb += "\n\t--> " + this.getVariables() + goodCommentVariables;
        sb += "\n\t--> " + this.getCommentSubroutines() + goodCommentSubroutines;
        sb += "\n\t--> " + this.getCommentControlStructures() + goodCommentControlStructures;

        if (goodCommentFunctions) {
            assesment += 0.5;
        }
        if (goodCommentInitDoc) {
            assesment += 0.5;
        }
        if (goodCommentVariables) {
            assesment += 0.5;
        }
        if (goodCommentSubroutines) {
            assesment += 0.5;
        }
        if (goodCommentControlStructures) {
            assesment += 0.5;
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
     * This method analyse if the sentence implicit none is used
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
     * This method transform a time in miliseconds in days, hours, minutes and
     * seconds
     *
     * @param millis the time to transform
     * @return the time in days, hours, minutes and seconds
     */
    private static String getDurationAnalyse(long millis) {

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

    /**
     * This method update the parameters in the lenguage selected by the user.
     * es: spanish, fr: french, gl: galician, en: english
     *
     * @param lang
     */
    private void changeLanguage(String lang) {

        switch (lang) {
            case "es":
                if (!this.selectedLanguage.equals("es")) {

                    //translate the text
                    this.selectedLanguage = "es";
                    setNameButtonAnalyse("Analizar");
                    setNameButtonExit("Salir");
                    setNameMenu("Idioma");
                    setErrorDirectoryEmpty("Seleccione un directorio");
                    setExitMessage("Análisis realizado.");
                    setDirectoryMessage("\nDocumento guardado en:\n" + Window.DEST);
                    setTimeMessage("\nTiempo: ");
                    setNumberOfLines("Número de líneas: ");
                    setMethod("Número de funciones: ");
                    setSelectDirectory("Seleccione un directorio: ");
                    setComments("Número de comentarios: ");
                    setFuntions("Número de funciones: ");
                    setSubroutinesCall("Número de llamadas a subrutinas: ");
                    setGoodComments("Bien comentado en: ");
                    setFunction("las funciones: ");
                    setInitDoc("el inicio del archivo: ");
                    setNumVariable("la declaración de cada variable: ");
                    setNestedLoops("Cumple con la complejidad máxima de anidamiento de los bucles y formato de los comentarios: ");
                    setSubroutines("Número de subrutinas: ");
                    setVariables("declaración de variables: ");
                    setCommentSubroutines("declaración de subrutinas: ");
                    setCommentControlStructures("estructuras de control: ");
                    setExit("Utiliza la sentencia EXIT para salir de los bucles antes de tiempo: ");
                    setCycle("Utiliza la sentencia CYCLE para evitar realizar determinadas sentencias, iterando al siguiente elemento: ");
                    setArithmeticAverage("Nota final: ");
                    setHeadMessageDialog("Información del análisis");
                    setNoteFile("Nota del archivo: ");

                    //configure buttons
                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.text.setText(this.getSelectDirectory());

                    //Configure MenuBar
                    this.mb.remove(menuLanguages);
                    this.menuLanguages.setName(this.getNameMenu());
                    this.menuLanguages = new JMenu(this.getNameMenu());
                    this.mb.add(menuLanguages);

                    this.spanish = new JMenuItem(this.getNameButtonSpanish());
                    this.spanish.addActionListener(this);
                    this.menuLanguages.add(spanish);

                    this.galician = new JMenuItem(this.getNameButtonGalician());
                    this.galician.addActionListener(this);
                    this.menuLanguages.add(galician);

                    this.english = new JMenuItem(this.getNameButtonEnglish());
                    this.english.addActionListener(this);
                    this.menuLanguages.add(english);

                    this.french = new JMenuItem(this.getNameButtonFrench());
                    this.french.addActionListener(this);
                    this.menuLanguages.add(french);

                }
                break;

            case "fr":
                if (!this.selectedLanguage.equals("fr")) {

                    //translate the text
                    this.selectedLanguage = "fr";
                    setNameButtonAnalyse("Analyzer");
                    setNameButtonExit("Sortir");
                    setNameMenu("Langage");
                    setErrorDirectoryEmpty("Seleccioné un répertoire");
                    setExitMessage("Analyse effectuée.");
                    setDirectoryMessage("\nDocument gardé dans la route:\n" + Window.DEST);
                    setTimeMessage("\nTemps: ");
                    setNumberOfLines("Nombre de lignes: ");
                    setMethod("Nombre de fonctions: ");
                    setSelectDirectory("Seleccioné un répertoire: ");
                    setComments("Nombre de commentaires: ");
                    setFuntions("Nombre de fonctions: ");
                    setSubroutinesCall("Nombre d'apelles à des sous-routines: ");
                    setGoodComments("Commentaires valides dans: ");
                    setFunction("les fonctions: ");
                    setInitDoc("le début du document: ");
                    setNumVariable("les variables déclarées: ");
                    setNestedLoops("il n'a pas l'imbrication très complexe et le format des commentaires est correct:");
                    setSubroutines("Nombre de sous-routines: ");
                    setVariables("déclaration des variables: ");
                    setCommentSubroutines("déclaration des sous-routines: ");
                    setCommentControlStructures("déclaration des structures de contrôle: ");
                    setExit("Utilization de EXIT pour sortir des boucles: ");
                    setCycle("Utilization de CYCLE pour eviter de realizer certains contrôles et itérer au element suivant: ");
                    setArithmeticAverage("Note final: ");
                    setHeadMessageDialog("information de  l`analyse");
                    setNoteFile("Note du fichier: ");

                    //Configure buttons
                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.text.setText(this.getSelectDirectory());

                    //Configure MenuBar
                    this.mb.remove(menuLanguages);
                    this.menuLanguages.setName(this.getNameMenu());
                    this.menuLanguages = new JMenu(this.getNameMenu());
                    this.mb.add(menuLanguages);

                    this.spanish = new JMenuItem(this.getNameButtonSpanish());
                    this.spanish.addActionListener(this);
                    this.menuLanguages.add(spanish);

                    this.galician = new JMenuItem(this.getNameButtonGalician());
                    this.galician.addActionListener(this);
                    this.menuLanguages.add(galician);

                    this.english = new JMenuItem(this.getNameButtonEnglish());
                    this.english.addActionListener(this);
                    this.menuLanguages.add(english);

                    this.french = new JMenuItem(this.getNameButtonFrench());
                    this.french.addActionListener(this);
                    this.menuLanguages.add(french);

                }
                break;

            case "gl":
                if (!this.selectedLanguage.equals("gl")) {

                    //translate the text
                    this.selectedLanguage = "gl";
                    setNameButtonAnalyse("Análise");
                    setNameButtonExit("Saír");
                    setNameMenu("Idioma");
                    setErrorDirectoryEmpty("Seleccione un directorio");
                    setExitMessage("Análise feito.");
                    setDirectoryMessage("\nGardado no directorio:\n" + Window.DEST);
                    setTimeMessage("\nTempo: ");
                    setNumberOfLines("Número de liñas: ");
                    setMethod("Número de funcións: ");
                    setSelectDirectory("Seleccione un directorio: ");
                    setComments("Número de comentarios: ");
                    setFuntions("Número de funcións: ");
                    setSubroutinesCall("Número de chamadas a subrutinas: ");
                    setGoodComments("Bos comentarios: ");
                    setFunction("nas funcións: ");
                    setInitDoc("ao comezo do arquivo: ");
                    setNumVariable("na declaración das variables: ");
                    setNestedLoops("Cumple coa complexidade máxima de anidamento dos bucles e o formato dos comentarios é o correcto: ");
                    setSubroutines("Número de subrutinas: ");
                    setVariables("declaración das variables: ");
                    setCommentSubroutines("declaración das subrutinas: ");
                    setCommentControlStructures("Estructuras de control: ");
                    setExit("Uso da sentencia EXIT para saír dos bucles: ");
                    setCycle("Uso da sentencia CYCLE para evitar realizar determinadas sentencias, iterando ao seguinte elemento: ");
                    setArithmeticAverage("Nota final: ");
                    setHeadMessageDialog("Información do análise");
                    setNoteFile("Nota do arquivo: ");

                    //configure buttons
                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.text.setText(this.getSelectDirectory());

                    //Configure MenuBar
                    this.mb.remove(menuLanguages);
                    this.menuLanguages.setName(this.getNameMenu());
                    this.menuLanguages = new JMenu(this.getNameMenu());
                    this.mb.add(menuLanguages);

                    this.spanish = new JMenuItem(this.getNameButtonSpanish());
                    this.spanish.addActionListener(this);
                    this.menuLanguages.add(spanish);

                    this.galician = new JMenuItem(this.getNameButtonGalician());
                    this.galician.addActionListener(this);
                    this.menuLanguages.add(galician);

                    this.english = new JMenuItem(this.getNameButtonEnglish());
                    this.english.addActionListener(this);
                    this.menuLanguages.add(english);

                    this.french = new JMenuItem(this.getNameButtonFrench());
                    this.french.addActionListener(this);
                    this.menuLanguages.add(french);

                }
                break;

            case "en":
                if (!this.selectedLanguage.equals("en")) {

                    //translate the text
                    this.selectedLanguage = "en";
                    setNameButtonAnalyse("Analyse");
                    setNameButtonExit("Exit");
                    setNameMenu("Language");
                    setErrorDirectoryEmpty("Select a directory");
                    setExitMessage("Analyse done.");
                    setDirectoryMessage("\nFile saved in:\n" + Window.DEST);
                    setTimeMessage("\nTime: ");
                    setNumberOfLines("Number of lines: ");
                    setMethod("Number of functions: ");
                    setSelectDirectory("Select a directory: ");
                    setComments("Number of comments: ");
                    setFuntions("Number of functions: ");
                    setSubroutinesCall("Number of subroutines calls: ");
                    setGoodComments("Good comments at: ");
                    setFunction("functions: ");
                    setInitDoc("the beginning of the file: ");
                    setNumVariable("the declarations of the variables: ");
                    setNestedLoops("The complexity of the loops is not hight and the format of comments is correct: ");
                    setSubroutines("Number of subroutines: ");
                    setVariables("declared variables: ");
                    setCommentSubroutines("declared subroutines: ");
                    setCommentControlStructures("control structures: ");
                    setExit("Use the sentence EXIT to go out of loop: ");
                    setCycle("Use the sentence CYCLE to avoid making certain judgments, iterating to the next element: ");
                    setArithmeticAverage("Final note: ");
                    setHeadMessageDialog("Analysis information");
                    setNoteFile("File note: ");

                    //configure buttons
                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.text.setText(this.getSelectDirectory());

                    //Configure MenuBar
                    this.mb.remove(menuLanguages);
                    this.menuLanguages.setName(this.getNameMenu());
                    this.menuLanguages = new JMenu(this.getNameMenu());
                    this.mb.add(menuLanguages);

                    this.spanish = new JMenuItem(this.getNameButtonSpanish());
                    this.spanish.addActionListener(this);
                    this.menuLanguages.add(spanish);

                    this.galician = new JMenuItem(this.getNameButtonGalician());
                    this.galician.addActionListener(this);
                    this.menuLanguages.add(galician);

                    this.english = new JMenuItem(this.getNameButtonEnglish());
                    this.english.addActionListener(this);
                    this.menuLanguages.add(english);

                    this.french = new JMenuItem(this.getNameButtonFrench());
                    this.french.addActionListener(this);
                    this.menuLanguages.add(french);
                }
                break;
        }
    }

    /**
     * Getter from NameButtonAnalyse
     *
     * @return nameButtonAnalyse
     */
    public String getNameButtonAnalyse() {
        return nameButtonAnalyse;
    }

    /**
     * Setter from NameButtonAnalyse
     *
     * @param nameButtonAnalyse the new name of nameByuttonAnalyse
     */
    public void setNameButtonAnalyse(String nameButtonAnalyse) {
        this.nameButtonAnalyse = nameButtonAnalyse;
    }

    /**
     * Getter from NameButtonExit
     *
     * @return nameButtonExit
     */
    public String getNameButtonExit() {
        return nameButtonExit;
    }

    /**
     * Setter from the NameButtonExit
     *
     * @param nameButtonExit the new name of nameButtonExit
     */
    public void setNameButtonExit(String nameButtonExit) {
        this.nameButtonExit = nameButtonExit;
    }

    /**
     * Getter from NameButtonSpanish
     *
     * @return nameButtonSpanish
     */
    public String getNameButtonSpanish() {
        return nameButtonSpanish;
    }

    /**
     * Getter from NameButtonEnglish
     *
     * @return nameButtonEnglish
     */
    public String getNameButtonEnglish() {
        return nameButtonEnglish;
    }

    /**
     * Getter from NameButtonFrench
     *
     * @return nameButtonFrench
     */
    public String getNameButtonFrench() {
        return nameButtonFrench;
    }

    /**
     * Getter from NameButtonGalician
     *
     * @return nameButtonGalician
     */
    public String getNameButtonGalician() {
        return nameButtonGalician;
    }

    /**
     * Getter from NameMenu
     *
     * @return nameMenu
     */
    public String getNameMenu() {
        return nameMenu;
    }

    /**
     * Setter from NameMenu
     *
     * @param nameMenu the new name of the menu
     */
    public void setNameMenu(String nameMenu) {
        this.nameMenu = nameMenu;
    }

    /**
     * Getter from ErrorDirectoryEmpty
     *
     * @return errorDirectory
     */
    public String getErrorDirectoryEmpty() {
        return errorDirectoryEmpty;
    }

    /**
     * Setter from ErrorDirectoryEmpty
     *
     * @param errorDirectoryEmpty
     */
    public void setErrorDirectoryEmpty(String errorDirectoryEmpty) {
        this.errorDirectoryEmpty = errorDirectoryEmpty;
    }

    /**
     * Getter from ExitMessage
     *
     * @return exitMessage
     */
    public String getExitMessage() {
        return exitMessage;
    }

    /**
     * Setter from ExitMessage
     *
     * @param exitMessage
     */
    public void setExitMessage(String exitMessage) {
        this.exitMessage = exitMessage;
    }

    /**
     * Getter from NumberOfLines
     *
     * @return numberOfLines
     */
    public String getNumberOfLines() {
        return numberOfLines;
    }

    /**
     * Setter from NumberOfLines
     *
     * @param numberOfLines
     */
    public void setNumberOfLines(String numberOfLines) {
        this.numberOfLines = numberOfLines;
    }

    /**
     * Getter from number of functions
     *
     * @return method
     */
    public String getMethod() {
        return funtions;
    }

    /**
     * Setter from numer of functions
     *
     * @param method
     */
    public void setMethod(String method) {
        this.funtions = method;
    }

    /**
     * Getter from selectDirectory
     *
     * @return directory selected
     */
    public String getSelectDirectory() {
        return selectDirectory;
    }

    /**
     * Setter from selectDirectory
     *
     * @param selectDirectory
     */
    public void setSelectDirectory(String selectDirectory) {
        this.selectDirectory = selectDirectory;
    }

    /**
     * Getter from funcitions
     *
     * @return functions
     */
    public String getFuntions() {
        return funtions;
    }

    /**
     * Setter of functions
     *
     * @param funtions
     */
    public void setFuntions(String funtions) {
        this.funtions = funtions;
    }

    /**
     * Getter of Comments
     *
     * @return comments
     */
    public String getComments() {
        return comments;
    }

    /**
     * Setter of Comments
     *
     * @param comments
     */
    public void setComments(String comments) {
        this.comments = comments;
    }

    /**
     * Getter of Subroutines
     *
     * @return subroutines
     */
    public String getSubroutines() {
        return subroutines;
    }

    /**
     * Setter of Subroutines
     *
     * @param subroutines
     */
    public void setSubroutines(String subroutines) {
        this.subroutines = subroutines;
    }

    /**
     * Getter of goodComments
     *
     * @return goodComments
     */
    public String getGoodComments() {
        return goodComments;
    }

    /**
     * Setter of goodComments
     *
     * @param goodComments
     */
    public void setGoodComments(String goodComments) {
        this.goodComments = goodComments;
    }

    /**
     * Getter of function
     *
     * @return functions
     */
    public String getFunction() {
        return function;
    }

    /**
     * Setter of function
     *
     * @param function
     */
    public void setFunction(String function) {
        this.function = function;
    }

    /**
     * Getter of variable
     *
     * @return numVariable
     */
    public String getNumVariable() {
        return numVariables;
    }

    /**
     * Setter of variable
     *
     * @param variable
     */
    public void setNumVariable(String variable) {
        this.numVariables = variable;
    }

    /**
     * Getter of initDoc
     *
     * @return InitDoc
     */
    public String getInitDoc() {
        return initDoc;
    }

    /**
     * Setter of initDoc
     *
     * @param initDoc
     */
    public void setInitDoc(String initDoc) {
        this.initDoc = initDoc;
    }

    /**
     * Getter of variables
     *
     * @return variables
     */
    public String getVariables() {
        return variables;
    }

    /**
     * Setter of variables
     *
     * @param variables
     */
    public void setVariables(String variables) {
        this.variables = variables;
    }

    /**
     * Getter NestedLoops
     *
     * @return nestedLoops
     */
    public String getNestedLoops() {
        return nestedLoops;
    }

    /**
     * Setter nestedLoops
     *
     * @param nestedLoops
     */
    public void setNestedLoops(String nestedLoops) {
        this.nestedLoops = nestedLoops;
    }

    /**
     * Getter subroutinesCall
     *
     * @return subroutinesCall
     */
    public String getSubroutinesCall() {
        return subroutinesCall;
    }

    /**
     * Setter subroutinesCall
     *
     * @param subroutinesCall
     */
    public void setSubroutinesCall(String subroutinesCall) {
        this.subroutinesCall = subroutinesCall;
    }

    /**
     * Getter commentSubroutinescommentControlStructures
     *
     * @return commentSubroutines
     */
    public String getCommentSubroutines() {
        return commentSubroutines;
    }

    /**
     * Setter commentSubroutines
     *
     * @param commentSubroutines
     */
    public void setCommentSubroutines(String commentSubroutines) {
        this.commentSubroutines = commentSubroutines;
    }

    /**
     * Getter commentStructureControl
     *
     * @return commentControlStructures
     */
    public String getCommentControlStructures() {
        return commentControlStructures;
    }

    /**
     * Setter commentStructureControl
     *
     * @param commentControlStructures
     */
    public void setCommentControlStructures(String commentControlStructures) {
        this.commentControlStructures = commentControlStructures;
    }

    /**
     * Getter of Exit
     *
     * @return exit
     */
    public String getExit() {
        return exit;
    }

    /**
     * Setter of Exit
     *
     * @param exit
     */
    public void setExit(String exit) {
        this.exit = exit;
    }

    /**
     * Getter of Cycle
     *
     * @return cycle
     */
    public String getCycle() {
        return cycle;
    }

    /**
     * Setter of Cycle
     *
     * @param cycle
     */
    public void setCycle(String cycle) {
        this.cycle = cycle;
    }

    /**
     * Getter of ArithmeticAverage
     *
     * @return arithmeticAverage
     */
    public String getArithmeticAverage() {
        return arithmeticAverage;
    }

    /**
     * Setter of arithmeticAverage
     *
     * @param arithmeticAverage
     */
    public void setArithmeticAverage(String arithmeticAverage) {
        this.arithmeticAverage = arithmeticAverage;
    }

    /**
     * Getter of HeadMessageDialog
     *
     * @return headMessageDialog
     */
    public String getHeadMessageDialog() {
        return headMessageDialog;
    }

    /**
     * Setter of headMessageDialog
     *
     * @param headMessageDialog
     */
    public void setHeadMessageDialog(String headMessageDialog) {
        this.headMessageDialog = headMessageDialog;
    }

    /**
     * Getter of DirectoryMessage
     *
     * @return directoryMessage
     */
    public String getDirectoryMessage() {
        return directoryMessage;
    }

    /**
     * Setter of directoryMessage
     *
     * @param directoryMessage
     */
    public void setDirectoryMessage(String directoryMessage) {
        this.directoryMessage = directoryMessage;
    }

    /**
     * Getter of TimeMessage
     *
     * @return timeMessage
     */
    public String getTimeMessage() {
        return timeMessage;
    }

    /**
     * Setter of TimeMessage
     *
     * @param timeMessage
     */
    public void setTimeMessage(String timeMessage) {
        this.timeMessage = timeMessage;
    }

    /**
     * Getter of noteFile
     * 
     * @return noteFile
     */
    public String getNoteFile()
    {
        return noteFile;
    }
    
    /**
     * Setter of noteFile
     * 
     * @param noteFile 
     */
    public void setNoteFile(String noteFile){
        this.noteFile = noteFile;
    }
}
