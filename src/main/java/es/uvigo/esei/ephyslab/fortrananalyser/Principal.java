package es.uvigo.esei.ephyslab.fortrananalyser;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
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
     * @param args the command line arguments
     * @throws java.io.IOException in case something wrong with intput/output
     * file
     * @throws java.lang.InterruptedException in case something interrupt the execution process
     * @throws java.lang.reflect.InvocationTargetException in case something is wrong with the GUI
     */
    public static void main(String[] args) throws IOException, InterruptedException, InvocationTargetException {
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
           
                WindowNOGUI w;
                w = new WindowNOGUI(args[0], args[1]);
         
        }

    }
}
