package es.uvigo.esei.ephyslab.fortrananalyser.metric.comment;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class VariablesComment implements Runnable {

    private static final Logger LOG = Logger.getLogger(VariablesComment.class.getName());
    private static final String EXCLAMATION = "!";
    private static final String CONSTRUCTOR = "::";

    private final String filePath;
    private int commentedElements;
    private boolean variableComment;

    public VariablesComment(String filePath) {
        this.filePath = filePath;
        this.commentedElements = 0;
    }

    @Override
    public void run() {
        try {
            String rowLine = "";
            String previousRowLine = "";
            File file = new File(filePath);
            int variablesCommented = 0;
            int totalVariables = 0;
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((rowLine = b.readLine()) != null) {
                    if (rowLine.contains(CONSTRUCTOR)) {
                        totalVariables++;
                        if (previousRowLine.contains(EXCLAMATION)) {
                            variablesCommented++;
                            commentedElements++;
                        }
                    }
                    previousRowLine = rowLine;
                }
            }
            variableComment = (totalVariables == variablesCommented);
            LOG.info(String.format("{%s} - Variable comments analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public int getCommentedElements() {
        return commentedElements;
    }

    public boolean isVariableComment() {
        return variableComment;
    }
}
