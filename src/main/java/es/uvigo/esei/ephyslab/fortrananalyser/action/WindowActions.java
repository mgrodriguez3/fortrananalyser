package es.uvigo.esei.ephyslab.fortrananalyser.action;

import es.uvigo.esei.ephyslab.fortrananalyser.GuiComponent.MainWindow;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

public class WindowActions {
    private static final String REPORT_PATH = System.getProperty("user.home") + "/temp/QualityReport.pdf";
    private static final String USER_MANUAL = "userManual";
    private static final String LICENSE_PDF = "license.pdf";
    private static final String ERROR_PDF = "errorpdf.pdf";
    private static final String[] AVAILABLE_LANGUAGES = {"es", "fr", "gl", "en", "pt"};
    private static final String[] AVAILABLE_COUNTRIES = {"ES", "FR", "ES", "GB", "PT"};
    private static final String MESSAGE_BUNDLE = "es.uvigo.esei.ephyslab.i18n.bundle.MessagesBundle";
    private Locale currentLocale;

    public WindowActions() {
        currentLocale = new Locale(AVAILABLE_LANGUAGES[3], AVAILABLE_COUNTRIES[3]);
    }

    public ResourceBundle changeLanguage(String lang) {
        for (int i = 0; i < AVAILABLE_LANGUAGES.length; i++) {
            if (AVAILABLE_LANGUAGES[i].equals(lang)) {
                currentLocale = new Locale(AVAILABLE_LANGUAGES[i], AVAILABLE_COUNTRIES[i]);
            }
        }
        return ResourceBundle.getBundle(MESSAGE_BUNDLE, currentLocale);
    }

    public void openLink(int num) {
        Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
        String web = "http://fortrananalyser.ephyslab.uvigo.es/docs/";

        switch (num) {
            case 0:
                web += USER_MANUAL + "_" + currentLocale.getLanguage() + ".pdf";
                break;

            case 1:
                web += LICENSE_PDF;
                break;

            case 2:
                web = "http://fortrananalyser.ephyslab.uvigo.es/";
                break;

            default:
                web += ERROR_PDF;
                break;
        }

        if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) {

            try {
                desktop.browse(new URI(web));
            } catch (URISyntaxException | IOException ex) {
                Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
            }

        }
    }

    public void openReport() {
        URL url;
        File myFile;

        try {
            url = Paths.get(REPORT_PATH).toUri().toURL();
            myFile = new File(url.toURI());

            if (Desktop.isDesktopSupported()) {
                Desktop.getDesktop().open(myFile);
            }

        } catch (MalformedURLException | URISyntaxException ex) {
            Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(MainWindow.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    public void executeAnalyse(String pathFile) {

    }

    public Locale getCurrentLocale() {
        return currentLocale;
    }

    public void setCurrentLocale(Locale currentLocale) {
        this.currentLocale = currentLocale;
    }
}
