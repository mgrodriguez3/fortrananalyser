/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.epyslab.fortrananalyser;

import java.util.List;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;

/**
 *
 * @author Michael García Rodríguez
 */
public class Window extends JFrame implements ActionListener {

    private final JFileChooser fc = new JFileChooser();
    private JLabel texto;           // etiqueta o texto no editable
    private JTextField caja;        // caja de texto, para insertar datos
    private JButton buttonanalyse;          // boton con una determinada accion
    private JButton buttonExit;
    private JButton buttonFileExplorer; //file explorer button

    public Window() throws IOException {
        super();                    // usamos el contructor de la clase padre JFrame
        configureWindow();        // configuramos la ventana
        initialiceComponents();   // inicializamos los atributos o componentes
    }

    private void configureWindow() throws IOException {
        this.setTitle("FortranAnalyser Tool");                   // colocamos titulo a la ventana
        this.setSize(400, 200);                                 // colocamos tamanio a la ventana (ancho, alto)
        this.setContentPane(new JLabel(new ImageIcon(ImageIO.read(new File("./img/ephyslab.png")))));
        this.setLocationRelativeTo(null);                       // centramos la ventana en la pantalla
        this.setResizable(false); //evita poner redimensionar la ventana
        this.setLayout(null);                                   // no usamos ningun layout, solo asi podremos dar posiciones a los componentes
        this.setResizable(false);                               // hacemos que la ventana no sea redimiensionable
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);    // hacemos que cuando se cierre la ventana termina todo proceso
    }

    private void initialiceComponents() {
        // creamos los componentes
        this.texto = new JLabel();
        this.caja = new JTextField();
        this.buttonanalyse = new JButton();
        this.buttonExit = new JButton();
        this.buttonFileExplorer = new JButton();

        // configuramos los componentes
        this.texto.setText("Seleccione un directorio");    // colocamos un texto a la etiqueta
        this.texto.setBounds(25, 25, 300, 25);   // colocamos posicion y tamanio al texto (x, y, ancho, alto)

        this.caja.setBounds(25, 50, 250, 25);   // colocamos posicion y tamanio a la caja (x, y, ancho, alto)
        this.caja.setEditable(false);

        this.buttonanalyse.setText("Analizar");   // colocamos un texto al boton
        this.buttonanalyse.setBounds(50, 100, 200, 30);  // colocamos posicion y tamanio al boton (x, y, ancho, alto)
        this.buttonanalyse.addActionListener(this);      // hacemos que el boton tenga una accion y esa accion estara en esta clase

        this.buttonExit.setText("Salir");
        this.buttonExit.setBounds(50, 150, 200, 30);
        this.buttonExit.addActionListener(this);

        this.buttonFileExplorer.setText("...");
        this.buttonFileExplorer.setBounds(300, 50, 50, 25);
        this.buttonFileExplorer.addActionListener(this);

        this.fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

        // adicionamos los componentes a la ventana
        this.add(texto);
        this.add(caja);
        this.add(buttonanalyse);
        this.add(buttonExit);
        this.add(buttonFileExplorer);

    }

    @Override
    public void actionPerformed(ActionEvent e) {

        if (e.getSource().equals(buttonanalyse)) {
            if (!this.caja.getText().isEmpty()) {
                analizeDirectories(this.caja.getText());
            } else {
                JOptionPane.showMessageDialog(this, "Seleccione un directorio ");// mostramos un mensaje (frame, mensaje)
            }

        }

        if (e.getSource().equals(buttonExit)) {
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

    private void analizeDirectories(String directory) {

        PDF pdf;
        String DEST = "/home/michael/temp/QualityInform.pdf";

        try {

            pdf = new PDF();
            pdf.createPdf(DEST);

            List<File> filesInFolder;
            String auxDir = "";
            
            //start the duration of the analysis
            long timeStart = System.currentTimeMillis();
            
            filesInFolder = Files.walk(Paths.get(directory))
                    .filter(Files::isRegularFile)
                    .map(java.nio.file.Path::toFile)
                    .collect(Collectors.toList());
            
            for (File file : filesInFolder) {

                if (!auxDir.equals(getPathFromFile(file)) && getFileExtension(file).equals("txt")) {
                    auxDir = getPathFromFile(file);
                    pdf.addSection(auxDir);
                }

                if (getFileExtension(file).equals("txt")) {
                    pdf.addSubSection(file.getName());
                    pdf.addResult(analyseFile(file.getAbsolutePath()));
                }
            }
            pdf.closePDF();
            long timeStop = System.currentTimeMillis();
            timeStop = timeStop - timeStart;
            JOptionPane.showMessageDialog(this, "Análisis realizado.\n Time: " + getDurationAnalyse(timeStop));
        } catch (IOException ex) {
            Logger.getLogger(Window.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    public String analyseFile(String pathFile) throws IOException {
        String result = "";

        //1.- count the number of lines in the file
        result += "Número de líneas: " + analyseNumberOfLines(pathFile);
        result += "\n";

        //2.- count the number of calls
        return result;

    }

    private String analyseNumberOfLines(String f) throws IOException {
        int count = 0;
        String cadena = "";
        File file = new File(f);

        FileReader fr = new FileReader(file);

        try (BufferedReader b = new BufferedReader(fr)) {
            while ((cadena = b.readLine()) != null) {
                count++;
            }
        }

        return Integer.toString(count);
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

    private static String getDurationAnalyse(long millis) {
        if (millis < 0) {
            throw new IllegalArgumentException("Duration must be greater than zero!");
        }

        long days = TimeUnit.MILLISECONDS.toDays(millis);
        millis -= TimeUnit.DAYS.toMillis(days);
        long hours = TimeUnit.MILLISECONDS.toHours(millis);
        millis -= TimeUnit.HOURS.toMillis(hours);
        long minutes = TimeUnit.MILLISECONDS.toMinutes(millis);
        millis -= TimeUnit.MINUTES.toMillis(minutes);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(millis);

        StringBuilder sb = new StringBuilder(64);
        sb.append(days);
        sb.append(" Days ");
        sb.append(hours);
        sb.append(" Hours ");
        sb.append(minutes);
        sb.append(" Minutes ");
        sb.append(seconds);
        sb.append(" Seconds");

        return sb.toString();
    }

}
