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
 * @version 1.9.8
 */
public class Window extends JFrame implements ActionListener {

    /**
     * application icon.
     */
    private static final ImageIcon ICON_EPHYSLAB
            = new ImageIcon(Window.class.getResource("ephysLab.png"));

    /**
     * name of the application.
     */
    private static final String APP_NAME = "Fortran Analyser tool";

    /**
     * default country to initialice the user interface.
     */
    static final String DEFAULT_COUNTRY = "GB";

    /**
     * default language to initialice the user interface.
     */
    static final String DEFAULT_LANGUAGE = "en";

    /**
     * other available languages to the user interface.
     */
    static final String[] AVAILABLE_LANGUAGES = {"es", "fr", "gl", "en"};

    /**
     * other available countries
     */
    static final String[] AVAILABLE_COUNTRIES = {"ES", "FR", "ES", "GB"};

    /**
     * the name of the package where the messagesBundle i18n are.
     */
    static final String BUNDLE = "es.uvigo.esei.ephyslab.i18n.bundle.MessagesBundle";

    /**
     * the file choser from the computer where the application is open.
     */
    private final JFileChooser fc;

    private static final String NAME_MENU = "nameMenu";

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
     * Spanish is one of possible languages to select
     */
    private JMenuItem spanish;

    /**
     * Galician is one of possible languages to select
     */
    private JMenuItem galician;

    /**
     * English is one of possible languages to select and the default language
     */
    private JMenuItem english;

    /**
     * French is one of possible languages to select
     */
    private JMenuItem french;

    /**
     * By default, the selected language is Enslish.
     */
    Locale currentLocale;

    /**
     * the string resource of the application.
     */
    ResourceBundle messages;

    /**
     * Constructor from Class
     *
     * @throws IOException in case something wrong with intput/output file
     */
    public Window() throws IOException {

        super();
        fc = new JFileChooser();
        configureWindow();
        initialiceComponents();

    }

    /**
     * Constructor for NoGUI usage
     *
     * @param language selected
     * @param pathToAnalyse path to analyse
     * @param fileName name of the output file
     */
    public Window(String language, String pathToAnalyse, String fileName) {
        fc = null;
        initialiceComponentsNoGUI();
        this.changeLanguage(language);
        new NoGUI(pathToAnalyse, fileName, Window.this.messages);
    }

    /**
     * This method set settings of the main windows.
     */
    private void configureWindow() {

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
     * Components to use in case the programme is lanched by console
     */
    private void initialiceComponentsNoGUI() {

        this.currentLocale = new Locale(Window.DEFAULT_LANGUAGE, Window.DEFAULT_COUNTRY);
        Locale.setDefault(currentLocale);
        this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);

    }

    /**
     * initialice all the components that form the main window
     */
    private void initialiceComponents() {

        initialiceComponentsNoGUI();

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
        this.configureMenuBar();

        this.setUndecorated(true);

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
        if (e.getSource().equals(buttonFileExplorer) && this.fc.showOpenDialog(Window.this)==JFileChooser.APPROVE_OPTION) {
            
                File file = this.fc.getSelectedFile();
                this.box.setText(file.getAbsolutePath());
            
        }
        
        //In the language menu, a language is selected
        if (e.getSource().equals(spanish) && !this.currentLocale.getLanguage().equals("es")) {

            this.changeLanguage("es");
            this.reConfigureButtons();
            this.reConfigureMenuBar();

        }

        if (e.getSource().equals(french) && !this.currentLocale.getLanguage().equals("fr")) {

            this.changeLanguage("fr");
            this.reConfigureButtons();
            this.reConfigureMenuBar();

        }

        if (e.getSource().equals(english) && !this.currentLocale.getLanguage().equals("en")) {

            this.changeLanguage("en");
            this.reConfigureButtons();
            this.reConfigureMenuBar();

        }

        if (e.getSource().equals(galician) && !this.currentLocale.getLanguage().equals("gl")) {

            this.changeLanguage("gl");
            this.reConfigureButtons();
            this.reConfigureMenuBar();

        }

    }

    /**
     * call the execution of the analisis in a new Thread
     *
     * @param pathFile the path of the file
     */
    private void executeAnalyse(String pathFile) {

        TasksBar t = new TasksBar(Window.this, pathFile, Window.this.messages);
        this.setEnabled(false);
        t.execute();

    }

    /**
     * This method update the parameters in the lenguage selected by the user.
     * es: spanish, fr: french, gl: galician, en: english
     *
     * @param lang the lenguage selected
     */
    private void changeLanguage(String lang) {

        for (int i = 0; i < AVAILABLE_LANGUAGES.length; i++) {

            if (AVAILABLE_LANGUAGES[i].equals(lang)) {
                currentLocale = new Locale(AVAILABLE_LANGUAGES[i], AVAILABLE_COUNTRIES[i]);
                this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);
                break;
            }
        }
    }

    /**
     * Reconfiguration of the buttons in case the language is switched.
     */
    private void reConfigureButtons() {
        this.buttonanalyse.setText(this.messages.getString("nameButtonAnalyse"));
        this.buttonExit.setText(this.messages.getString("nameButtonExit"));
        this.text.setText(this.messages.getString("selectDirectory"));
    }

    /**
     * reconfiguration of the menuBar buttons in case the language is switched.
     */
    private void reConfigureMenuBar() {

        this.mb.remove(menuLanguages);
        this.configureMenuBar();
    }

    /**
     * Configuration of the menu bar.
     */
    private void configureMenuBar() {

        this.menuLanguages = new JMenu(this.messages.getString(Window.NAME_MENU));
        this.menuLanguages.setName(this.messages.getString(Window.NAME_MENU));
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
