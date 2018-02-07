/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
 * @version 1.9
 */
public class Principal {

    /**
     * @param args the command line arguments
     * @throws java.io.IOException when file can not open
     * @throws java.lang.InterruptedException when the process is interrupted
     * @throws java.lang.reflect.InvocationTargetException when the invocation
     * is interrupted
     */
    public static void main(String[] args) throws IOException, InterruptedException, InvocationTargetException {
        org.apache.log4j.BasicConfigurator.configure();

        SwingUtilities.invokeLater(() -> {
            Window w;
            try {
                w = new Window();
                w.setVisible(true);
            } catch (IOException ex) {
                Logger.getLogger(Principal.class.getName()).log(Level.SEVERE, null, ex);
            }
        });

    }
}
