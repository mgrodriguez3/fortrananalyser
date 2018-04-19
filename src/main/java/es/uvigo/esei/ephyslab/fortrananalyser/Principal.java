/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

/**
 * Main class that is called initially when the code is executed
 *
 * @author Michael García Rodríguez
 * @version 1.9
 */
public class Principal {

    /**
     * @param args the command line arguments
     * @throws java.io.IOException
     * @throws java.lang.InterruptedException
     * @throws java.lang.reflect.InvocationTargetException
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
            SwingUtilities.invokeLater(() -> {
                Window w;
                w = new Window(args[0], args[1]);
            });
        }

    }
}
