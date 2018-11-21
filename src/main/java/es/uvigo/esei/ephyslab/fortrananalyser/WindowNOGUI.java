package es.uvigo.esei.ephyslab.fortrananalyser;

import java.util.Locale;
import java.util.ResourceBundle;

/**
 *
 * @author Michael García Rodríguez
 * @version 1.9.2
 */
public class WindowNOGUI {
    
    
    /**
     * By default, the selected language is Enslish.
     */
    Locale currentLocale;

    /**
     * the string resource of the application.
     */
    ResourceBundle messages;
    
    public WindowNOGUI(String language, String path){
        initialiceComponentsNoGUI();
        this.changeLanguageNoGUI(language);
        NoGUI noGUI = new NoGUI(path, WindowNOGUI.this.messages);
    }
    
    private void initialiceComponentsNoGUI() {

        this.currentLocale = new Locale(Window.DEFAULT_LANGUAGE, Window.DEFAULT_COUNTRY);
        Locale.setDefault(currentLocale);
        this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);


    }
    
    private void changeLanguageNoGUI(String lang) {

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
                
            /**
             * Enslish is de default language
             */
            default: 
                //translate the text and update the value of strings in the messages variable
                    this.currentLocale = new Locale("en", "GB");
                    this.messages = ResourceBundle.getBundle(Window.BUNDLE, currentLocale);
                
        }

    }
    
}
