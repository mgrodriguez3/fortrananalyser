package es.uvigo.esei.ephyslab.fortrananalyser.metric.comment;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class FunctionComment implements Runnable{

    private static final Logger LOG = Logger.getLogger(FunctionComment.class.getName());
    private static final String EXCLAMATION = "!";
    private static final String END_FUNCTION = "END FUNCTION";
    private static final String FUNCTION = "FUNCTION";
    private final String filePath;
    private boolean commentedFunctions;
    private int commentedElements;


    public FunctionComment(String filePath) {
        this.filePath = filePath;
        this.commentedElements = 0;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            String previousChain = "";
            File file = new File(filePath);
            int numFunction = 0;
            int totalFunctions = 0;
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains(EXCLAMATION)
                            && !chain.contains(END_FUNCTION)
                            && chain.contains(FUNCTION)) {
                        totalFunctions++;
                        if (previousChain.contains(EXCLAMATION)) {
                            numFunction++;
                            commentedElements++;
                        }
                    }
                    previousChain = chain;
                }
            }
            commentedFunctions = (totalFunctions == numFunction);
            LOG.info(String.format("{%s} - Function comments analisis: finished", filePath));
        } catch (IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isCommentedFunctions() {
        return commentedFunctions;
    }

    public int getCommentedElements() {
        return commentedElements;
    }
}
