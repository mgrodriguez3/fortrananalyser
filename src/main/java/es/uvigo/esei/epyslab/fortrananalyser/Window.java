/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.epyslab.fortrananalyser;

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
import javax.imageio.ImageIO;
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
    private final JFileChooser fc = new JFileChooser();
    private JLabel text;
    private JTextField box;
    private JButton buttonanalyse;
    private JButton buttonExit;
    private JButton buttonFileExplorer;
    private JMenuBar mb;
    private JMenu menuLanguages;
    private JMenuItem spanish, galician, english, french;
    private static final String EXTENSION = "txt";
    private static final String DEST = System.getProperty("user.home") + "/temp/QualityInform.pdf";

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
    private String exitMessage = "Análisis realizado.\nDocumento guardado en:\n" + Window.DEST + "\nTiempo: ";
    private String numberOfLines = "Número de líneas: ";
    private final String implicitNone = "Implicit none: ";
    private String funtions = "Número de funciones: ";
    private String comments = "Número de comentarios: ";
    private String subroutines = "Número de llamadas a subrutinas: ";

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

        this.setTitle("FortranAnalyser Tool");
        this.setSize(400, 250);
        this.setContentPane(new JLabel(new ImageIcon(ImageIO.read(new File("./img/ephyslab.png")))));
        this.setLocationRelativeTo(null);
        this.setResizable(false);
        this.setLayout(null);
        this.setResizable(false);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
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

        try {

            pdf = new PDF();
            pdf.createPdf(Window.DEST);

            List<File> filesInFolder;
            String auxDir = "";

            //start the duration of the analysis
            long timeStart = System.currentTimeMillis();

            filesInFolder = Files.walk(Paths.get(directory))
                    .filter(Files::isRegularFile)
                    .map(java.nio.file.Path::toFile)
                    .collect(Collectors.toList());

            for (File file : filesInFolder) {

                if (!auxDir.equals(getPathFromFile(file)) && getFileExtension(file).equals(Window.EXTENSION)) {
                    auxDir = getPathFromFile(file);
                    pdf.addSection(auxDir);
                }

                if (getFileExtension(file).equals(Window.EXTENSION)) {
                    pdf.addSubSection(file.getName());
                    pdf.addResult(analyseFile(file.getAbsolutePath()));
                }
            }
            pdf.closePDF();
            long timeStop = System.currentTimeMillis();
            timeStop = timeStop - timeStart;
            JOptionPane.showMessageDialog(this, this.getExitMessage() + getDurationAnalyse(timeStop));
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

        //1.- count the number of lines in the file
        result += this.getNumberOfLines() + analyseNumberOfLines(pathFile);
        result += "\n";

        //2.- Use or not use  the sentence IMPLICIT NONE
        result += this.implicitNone + this.analyseUseImplicitNone(pathFile);
        result += "\n";

        //3.- count the number of functions declared
        result += this.getMethod() + this.analyseNumFunctions(pathFile);
        result += "\n";

        //4.- count the number of subroutines calls
        result += this.getSubroutines() + this.analyseNumCalls(pathFile);
        result += "\n";

        //5.- count the number of comments
        result += this.getComments() + this.analyseNumComments(pathFile);
        result += " \n";

        //6.- good comments in file
        
        //7.- check the number of Nested loops
        
        
        return result;

    }

    
    private String analyseNestedLoops(String filePath) throws IOException{
        
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("FOR") || chain.contains("for")) {

                }
            }
        }

        return "";
    }
    
    /**
     * This method check if all comments are good. This is: 1.- the functions are
     * commented after or before the declaration. 2.- the variables are commented
     * after or before the declaration. 3.- the subrutines are commented after or
     * befor the declaration. 4.- the three first or more lines of a file are
     * commented.
     *
     * @param filePath
     * @return
     * @throws IOException
     */
    private String analyseGoodComment(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("!")) {

                }
            }
        }

        return "";
    }

    /**
     * This method obtains the number of lines of a file
     *
     * @param filePath the path of the file
     * @return the number of lines from file
     * @throws IOException
     */
    private String analyseNumberOfLines(String filePath) throws IOException {

        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                count++;
            }
        }

        return Integer.toString(count);
    }

    /**
     * This method analyse if the sentence implicit none is used
     *
     * @param filePath
     * @return
     * @throws IOException
     */
    private boolean analyseUseImplicitNone(String filePath) throws IOException {

        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if ((chain.contains("IMPLICIT NONE")) || (chain.contains("implicit none"))) {
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
    private String analyseNumFunctions(String filePath) throws IOException {

        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("FUNCTION") || chain.contains("function")) {
                    count++;
                }
            }
        }

        return Integer.toString(count / 2);
    }

    /**
     * This method analyse the number of subroutines are called in this file
     *
     * @param filePath the absolute path from file
     * @return the number of subroutines calls
     * @throws IOException
     */
    private String analyseNumCalls(String filePath) throws IOException {
        int count = 0;
        String chain = "";
        File file = new File(filePath);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((chain = b.readLine()) != null) {
                if (chain.contains("CALL") || chain.contains("call")) {
                    count++;
                }
            }
        }

        return Integer.toString(count);
    }

    /**
     * This methos analyse the number of comments are in a file
     *
     * @param filePath the absolute path of the file
     * @return the number of comments
     * @throws IOException
     */
    private String analyseNumComments(String filePath) throws IOException {
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

        return Integer.toString(count);
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

    private void changeLanguage(String lang) {

        switch (lang) {
            case "es":
                if (!this.selectedLanguage.equals("es")) {

                    this.selectedLanguage = "es";
                    setNameButtonAnalyse("Analizar");
                    setNameButtonExit("Salir");
                    setNameMenu("Idioma");
                    setErrorDirectoryEmpty("Seleccione un directorio");
                    setExitMessage("Análisis realizado.\nDocumento guardado en:\n" + Window.DEST + "\nTiempo: ");
                    setNumberOfLines("Número de líneas: ");
                    setMethod("Número de funciones: ");
                    setSelectDirectory("Seleccione un directorio: ");
                    setComments("Número de comentarios: ");
                    setFuntions("Número de funciones: ");
                    setSubroutines("Número de llamadas a subrutinas: ");

                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.menuLanguages.setName(this.getNameMenu());
                    this.text.setText(this.getSelectDirectory());

                }
                break;

            case "fr":
                if (!this.selectedLanguage.equals("fr")) {

                    this.selectedLanguage = "fr";
                    setNameButtonAnalyse("Analyzer");
                    setNameButtonExit("Sortir");
                    setNameMenu("Langage");
                    setErrorDirectoryEmpty("Seleccioné un répertoire");
                    setExitMessage("Analyse effectuée.\nDocument gardé dans la route:\n" + Window.DEST + "\nTemps: ");
                    setNumberOfLines("Nombre de lignes: ");
                    setMethod("Nombre de fonctions: ");
                    setSelectDirectory("Seleccioné un répertoire: ");
                    setComments("Nombre de commentaires: ");
                    setFuntions("Nombre de fonctions: ");
                    setSubroutines("Nombre d'apelles à des sous-routines: ");

                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.menuLanguages.setName(this.getNameMenu());
                    this.text.setText(this.getSelectDirectory());

                }
                break;

            case "gl":
                if (!this.selectedLanguage.equals("gl")) {

                    this.selectedLanguage = "gl";
                    setNameButtonAnalyse("Análise");
                    setNameButtonExit("Saír");
                    setNameMenu("Idioma");
                    setErrorDirectoryEmpty("Seleccione un directorio");
                    setExitMessage("Análise feito.\nGardado no directorio:\n" + Window.DEST + "\nTempo: ");
                    setNumberOfLines("Número de liñas: ");
                    setMethod("Número de funcións: ");
                    setSelectDirectory("Seleccione un directorio: ");
                    setComments("Número de comentarios: ");
                    setFuntions("Número de funcións: ");
                    setSubroutines("Número de chamadas a subrutinas: ");

                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.menuLanguages.setName(this.getNameMenu());
                    this.text.setText(this.getSelectDirectory());

                }
                break;

            case "en":
                if (!this.selectedLanguage.equals("en")) {

                    this.selectedLanguage = "es";
                    setNameButtonAnalyse("Analyse");
                    setNameButtonExit("Exit");
                    setNameMenu("Language");
                    setErrorDirectoryEmpty("Select a directory");
                    setExitMessage("Analyse done\nFile saved in:\n" + Window.DEST + "\nTime: ");
                    setNumberOfLines("Number of lines: ");
                    setMethod("Number of functions: ");
                    setSelectDirectory("Select a directory: ");
                    setComments("Number of comments: ");
                    setFuntions("Number of functions: ");
                    setSubroutines("Number of subroutines calls: ");

                    this.buttonanalyse.setText(this.getNameButtonAnalyse());
                    this.buttonExit.setText(this.getNameButtonExit());
                    this.menuLanguages.setName(this.getNameMenu());
                    this.text.setText(this.getSelectDirectory());
                }
                break;
        }
    }

    /**
     * Getter from NameButtonAnalyse
     *
     * @return
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
     * @return
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
     * @return
     */
    public String getNameButtonSpanish() {
        return nameButtonSpanish;
    }

    /**
     * Getter from NameButtonEnglish
     *
     * @return
     */
    public String getNameButtonEnglish() {
        return nameButtonEnglish;
    }

    /**
     * Getter from NameButtonFrench
     *
     * @return
     */
    public String getNameButtonFrench() {
        return nameButtonFrench;
    }

    /**
     * Getter from NameButtonGalician
     *
     * @return
     */
    public String getNameButtonGalician() {
        return nameButtonGalician;
    }

    /**
     * Getter from NameMenu
     *
     * @return
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
     * @return
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
     * @return
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
     * @return
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
     * @return
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
     * @return
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
     * @return
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
     * @return
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
     * @return
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

}
