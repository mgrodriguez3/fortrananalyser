/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.epyslab.fortrananalyser;
import java.util.List;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

/**
 *
 * @author Michael García Rodríguez
 */
public class Window extends JFrame implements ActionListener {

    private final JFileChooser fc = new JFileChooser();
    private JLabel texto;           // etiqueta o texto no editable
    private JTextField caja;        // caja de texto, para insertar datos
    private JButton boton;          // boton con una determinada accion
    private JButton boton2;
    private JButton buttonFileExplorer; //file explorer button

    public Window() {
        super();                    // usamos el contructor de la clase padre JFrame
        configurarVentana();        // configuramos la ventana
        inicializarComponentes();   // inicializamos los atributos o componentes
    }

    private void configurarVentana() {
        this.setTitle("FortranAnalyser Tool");                   // colocamos titulo a la ventana
        this.setSize(400, 200);                                 // colocamos tamanio a la ventana (ancho, alto)
        this.setLocationRelativeTo(null);                       // centramos la ventana en la pantalla
        this.setResizable(false); //evita poner redimensionar la ventana
        this.setLayout(null);                                   // no usamos ningun layout, solo asi podremos dar posiciones a los componentes
        this.setResizable(false);                               // hacemos que la ventana no sea redimiensionable
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);    // hacemos que cuando se cierre la ventana termina todo proceso
    }

    private void inicializarComponentes() {
        // creamos los componentes
        this.texto = new JLabel();
        this.caja = new JTextField();
        this.boton = new JButton();
        this.boton2 = new JButton();
        this.buttonFileExplorer = new JButton();

        // configuramos los componentes
        this.texto.setText("Seleccione un directorio");    // colocamos un texto a la etiqueta
        this.texto.setBounds(25, 25, 300, 25);   // colocamos posicion y tamanio al texto (x, y, ancho, alto)

        this.caja.setBounds(25, 50, 250, 25);   // colocamos posicion y tamanio a la caja (x, y, ancho, alto)
        this.caja.setEditable(false);

        this.boton.setText("Analizar");   // colocamos un texto al boton
        this.boton.setBounds(50, 100, 200, 30);  // colocamos posicion y tamanio al boton (x, y, ancho, alto)
        this.boton.addActionListener(this);      // hacemos que el boton tenga una accion y esa accion estara en esta clase

        this.boton2.setText("Salir");
        this.boton2.setBounds(50, 150, 200, 30);
        this.boton2.addActionListener(this);

        this.buttonFileExplorer.setText("...");
        this.buttonFileExplorer.setBounds(300, 50, 50, 25);
        this.buttonFileExplorer.addActionListener(this);

        this.fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        // adicionamos los componentes a la ventana
        this.add(texto);
        this.add(caja);
        this.add(boton);
        this.add(boton2);
        this.add(buttonFileExplorer);
    }

    @Override
    public void actionPerformed(ActionEvent e) {

        if (e.getSource().equals(boton)) {
            if (!this.caja.getText().isEmpty()) {
                analyce(this.caja.getText());
            }
            /*String nombre = caja.getText();                                 // obtenemos el contenido de la caja de texto
            JOptionPane.showMessageDialog(this, "Hola " + nombre + ".");    // mostramos un mensaje (frame, mensaje)*/
        }

        if (e.getSource().equals(boton2)) {
            System.exit(0);
        }

        if (e.getSource().equals(buttonFileExplorer)) {
            int returnVal = this.fc.showOpenDialog(Window.this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                File file = this.fc.getSelectedFile();
                this.caja.setText(file.getAbsolutePath());
            }
        }

    }

    private void analyce(String directory) {

        try {
            List<File> filesInFolder;
            String auxDir = "";
            filesInFolder = Files.walk(Paths.get(directory))
                    .filter(Files::isRegularFile)
                    .map(java.nio.file.Path::toFile)
                    .collect(Collectors.toList());

            for (File file : filesInFolder) {

                if (!auxDir.equals(getPathFromFile(file))) {
                    auxDir = getPathFromFile(file);
                    System.out.println(auxDir);
                }

                if (getFileExtension(file).equals("txt")) {
                    System.out.println(file.getName());
                }
            }
        } catch (IOException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }

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

}

