package es.uvigo.esei.ephyslab.fortrananalyser.metric.comment;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class ControlStructureComment implements Runnable {

    private static final Logger LOG = Logger.getLogger(ControlStructureComment.class.getName());
    private static final String EXCLAMATION = "!";
    private static final String SELECT_CASE = "SELECT CASE";
    private static final String END_SELECT = "END SELECT";
    private static final String IF = "IF (";
    private static final String END_IF = "ENDIF";
    private final String filePath;
    private boolean controlStructureComment;


    public ControlStructureComment(String filePath) {
        this.filePath = filePath;

    }

    @Override
    public void run() {
        try {
            String chain = "";
            String previousChain = "";
            File file = new File(filePath);
            int numControlStructures = 0;
            int totalControlStructures = 0;
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if ((!chain.contains(EXCLAMATION)
                            && !chain.contains(END_IF)
                            && chain.contains(IF))
                            || (!chain.contains(EXCLAMATION)
                            && !chain.contains(END_SELECT)
                            && chain.contains(SELECT_CASE))) {
                        totalControlStructures++;
                        if (previousChain.contains(EXCLAMATION)) {
                            numControlStructures++;
                        }
                    }
                    previousChain = chain;
                }
            }
            controlStructureComment = (totalControlStructures == numControlStructures);
            LOG.info(String.format("{%s} - Control structure comments analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isControlStructureComment() {
        return controlStructureComment;
    }
}
