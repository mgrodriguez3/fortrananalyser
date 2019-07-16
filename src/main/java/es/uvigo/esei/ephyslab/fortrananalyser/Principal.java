package es.uvigo.esei.ephyslab.fortrananalyser;

import javax.swing.SwingUtilities;

/**
 * Main class that is called initially when the code is executed
 *
 * @author Michael García Rodríguez
 * @version 1.9.8
 */
public class Principal {

    /**
     * @param args arguments used when the programme is launched: In case that
     * there are no arguments, the user interface is launched; In case that
     * there are arguments, the user interface is not launched.
     * @throws es.uvigo.esei.ephyslab.fortrananalyser.EditableException
     * exception throwed in case the number of arguments are wrong
     */
    public static void main(String[] args) throws EditableException {
        org.apache.log4j.BasicConfigurator.configure();

        if (args.length == 0) {
            SwingUtilities.invokeLater(() -> {
                MainWindow mw;
                mw = new MainWindow();
                mw.setVisible(true);
                
            });
        } else {

            if (args.length == 3) {

                String language = args[0];
                String path = args[1];
                String outputFileName = args[2];

                new MainWindow(language, path, outputFileName);

            } else {
                throw new EditableException(111);
            }
        }
    }
}
