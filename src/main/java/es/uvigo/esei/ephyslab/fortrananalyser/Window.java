/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.Locale;
import java.util.ResourceBundle;
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
 * @version 1.9
 */
public class Window extends JFrame implements ActionListener {

    /**
     * application icon.
     */
    private final static ImageIcon ICON_EPHYSLAB
            = new ImageIcon(Window.class.getResource("ephysLab.png"));

    /**
     * name of the application.
     */
    private final static String APP_NAME = "Fortran Analyser tool";

    /**
     * default country to initialice the user interface.
     */
    private final static String DEFAULT_COUNTRY = "ES";

    /**
     * default language to initialice the user interface.
     */
    private final static String DEFAULT_LANGUAGE = "es";

    /**
     * the name of the package where the messagesBundle i18n are.
     */
    private final static String BUNDLE = "es.uvigo.esei.ephyslab.i18n.bundle.MessagesBundle";

    /**
     * the file choser from the computer where the application is open.
     */
    private final JFileChooser fc = new JFileChooser();

    /**
     * the text that is show to select a directory.
     */
    private JLabel text;

    /**
     * the box where is the path of the directory to analyse.
     */
    private JTextField box;

    /**
     * define the button analyse.
     */
    private JButton buttonanalyse;

    /**
     * define the button exit.
     */
    private JButton buttonExit;

    /**
     * define the button file explorer.
     */
    private JButton buttonFileExplorer;

    /**
     * define the menu bar in a window.
     */
    private JMenuBar mb;

    /**
     * define the menu to select the language.
     */
    private JMenu menuLanguages;

    /**
     * the posible language to select.
     */
    private JMenuItem spanish, galician, english, french;

    /**
     * By default, the selected language is Spanish.
     */
    Locale currentLocale;

    /**
     * the string resource of the application.
     */
    ResourceBundle messages;

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

        this.currentLocale = new Locale(Window.DEFAULT_LANGUAGE, Window.DEFAULT_COUNTRY);
        Locale.setDefault(currentLocale);
        this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);

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
        this.text.setText(this.messages.getString("selectDirectory"));
        this.text.setBounds(25, 25, 300, 25);

        this.box.setBounds(25, 50, 250, 25);
        this.box.setEditable(false);

        this.buttonanalyse.setText(this.messages.getString("nameButtonAnalyse"));
        this.buttonanalyse.setBounds(50, 100, 200, 30);
        this.buttonanalyse.addActionListener(this);

        this.buttonExit.setText(this.messages.getString("nameButtonExit"));
        this.buttonExit.setBounds(50, 150, 200, 30);
        this.buttonExit.addActionListener(this);

        this.buttonFileExplorer.setText("...");
        this.buttonFileExplorer.setBounds(300, 50, 50, 25);
        this.buttonFileExplorer.addActionListener(this);

        this.fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        //Configure MenuBar
        this.menuLanguages = new JMenu(this.messages.getString("nameMenu"));
        this.mb.add(menuLanguages);

        this.spanish = new JMenuItem(this.messages.getString("nameButtonSpanish"));
        this.spanish.addActionListener(this);
        this.menuLanguages.add(spanish);

        this.galician = new JMenuItem(this.messages.getString("nameButtonGalician"));
        this.galician.addActionListener(this);
        this.menuLanguages.add(galician);

        this.english = new JMenuItem(this.messages.getString("nameButtonEnglish"));
        this.english.addActionListener(this);
        this.menuLanguages.add(english);

        this.french = new JMenuItem(this.messages.getString("nameButtonFrench"));
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

                executeAnalyse(this.box.getText());

            } else {
                JOptionPane.showMessageDialog(this, this.messages.getString("errorDirectoryEmpty"));
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
        if (e.getSource().equals(spanish) && !this.currentLocale.getLanguage().equals("es")) {
            this.changeLanguage("es");
        }

        if (e.getSource().equals(french) && !this.currentLocale.getLanguage().equals("fr")) {
            this.changeLanguage("fr");
        }

        if (e.getSource().equals(english) && !this.currentLocale.getLanguage().equals("en")) {
            this.changeLanguage("en");
        }

        if (e.getSource().equals(galician) && !this.currentLocale.getLanguage().equals("gl")) {
            this.changeLanguage("gl");
        }

    }

    /**
     * call the execution of the analisis in a new Thread
     *
     * @param pathFile
     */
    private void executeAnalyse(String pathFile) {

        TasksBar t = new TasksBar(Window.this, pathFile, Window.this.messages);
        t.execute();

    }

    /**
     * This method update the parameters in the lenguage selected by the user.
     * es: spanish, fr: french, gl: galician, en: english
     *
     * @param lang
     */
    private void changeLanguage(String lang) {

        /**
         * configuring the language
         */
        switch (lang) {

            /**
             * spanish from Spain
             */
            case "es":
                if (!this.currentLocale.getLanguage().equals("es")) {

                    //translate the text and update the value of strings in the messages variable
                    currentLocale = new Locale("es", "ES");
                    this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);
                }

                break;
            /**
             * french from France
             */
            case "fr":
                if (!this.currentLocale.getLanguage().equals("fr")) {

                    //translate the text and update the value of strings in the messages variable
                    this.currentLocale = new Locale("fr", "FR");
                    this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);
                }
                break;

            /**
             * Galician from Spain
             */
            case "gl":
                if (!this.currentLocale.getLanguage().equals("gl")) {

                    //translate the text and update the value of strings in the messages variable
                    this.currentLocale = new Locale("gl", "ES");
                    this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);
                }
                break;

            /**
             * english from United Kingdom
             */
            case "en":
                if (!this.currentLocale.getLanguage().equals("en")) {

                    //translate the text and update the value of strings in the messages variable
                    this.currentLocale = new Locale("en", "GB");
                    this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);
                }
                break;
        }

        //re-configure buttons
        this.buttonanalyse.setText(this.messages.getString("nameButtonAnalyse"));
        this.buttonExit.setText(this.messages.getString("nameButtonExit"));
        this.text.setText(this.messages.getString("selectDirectory"));

        //re-configure MenuBar
        this.mb.remove(menuLanguages);
        this.menuLanguages.setName(this.messages.getString("nameMenu"));
        this.menuLanguages = new JMenu(this.messages.getString("nameMenu"));
        this.mb.add(menuLanguages);

        this.spanish = new JMenuItem(this.messages.getString("nameButtonSpanish"));
        this.spanish.addActionListener(this);
        this.menuLanguages.add(spanish);

        this.galician = new JMenuItem(this.messages.getString("nameButtonGalician"));
        this.galician.addActionListener(this);
        this.menuLanguages.add(galician);

        this.english = new JMenuItem(this.messages.getString("nameButtonEnglish"));
        this.english.addActionListener(this);
        this.menuLanguages.add(english);

        this.french = new JMenuItem(this.messages.getString("nameButtonFrench"));
        this.french.addActionListener(this);
        this.menuLanguages.add(french);

    }

}
