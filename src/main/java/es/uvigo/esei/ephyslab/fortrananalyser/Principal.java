package es.uvigo.esei.ephyslab.fortrananalyser;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

/**
 * Main class that is called initially when the code is executed
 *
 * @author Michael García Rodríguez
 * @version 1.9.2
 */
public class Principal {

    /**
     *
     * @param args arguments used when the programme is launched:
     *
     * In case that there are no arguments, the user interface is launched; In
     * case that there are arguments, the user interface is not launched.
     * @throws java.lang.Exception
     */
    public static void main(String[] args) throws Exception {
        org.apache.log4j.BasicConfigurator.configure();

        if (args.length == 0) {
            SwingUtilities.invokeLater(() -> {
                Window w;
                try {
                    w = new Window();
                    w.setVisible(true);
                } catch (IOException ex) {
                    Logger.getLogger(Principal.class.getName()).log(Level.SEVERE, null, ex);
                }
            });
        } else {

            if (args.length == 3) {

                String language = args[0];
                String path = args[1];
                String fileName = args[2];
                Window w;
                w = new Window(language, path, fileName);

            } else {
                throw new EditableException(111);

            }

        }

    }
}
