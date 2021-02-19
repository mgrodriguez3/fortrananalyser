package es.uvigo.esei.ephyslab.fortrananalyser.metric.comment;

import org.apache.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class SubroutineComment implements Runnable {

    private static final Logger LOG = Logger.getLogger(SubroutineComment.class.getName());
    private static final String EXCLAMATION = "!";
    private static final String SUBROUTINE = "SUBROUTINE";
    private static final String END_SUBROUTINE = "END SUBROUTINE";
    private final String filePath;
    private boolean commentedSubroutine;
    private int commentedElements;

    public SubroutineComment(String filePath) {
        this.filePath = filePath;
        this.commentedElements = 0;
    }

    @Override
    public void run() {
        try {
            String chain = "";
            String previousChain = "";
            File file = new File(filePath);
            int numSubroutines = 0;
            int totalSubroutines = 0;
            FileReader fr = new FileReader(file);
            try (BufferedReader b = new BufferedReader(fr)) {
                while ((chain = b.readLine()) != null) {
                    chain = chain.toUpperCase();
                    if (!chain.contains(EXCLAMATION)
                            && !chain.contains(END_SUBROUTINE)
                            && chain.contains(SUBROUTINE)) {
                        totalSubroutines++;
                        if (previousChain.contains(EXCLAMATION)) {
                            numSubroutines++;
                            commentedElements++;
                        }
                    }
                    previousChain = chain;
                }
            }
            commentedSubroutine = (totalSubroutines == numSubroutines);
            LOG.info(String.format("{%s} - Subroutine comments analisis: finished", filePath));
        } catch (
                IOException e) {
            LOG.error(String.format("Buffer reader error on reading file [%s]", filePath), e);
        }
    }

    public boolean isCommentedSubroutine() {
        return commentedSubroutine;
    }

    public int getCommentedElements() {
        return commentedElements;
    }
}
