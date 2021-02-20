package es.uvigo.esei.ephyslab.fortrananalyser.metric;

import es.uvigo.esei.ephyslab.fortrananalyser.metric.comment.ControlStructureComment;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class GoodComment {

    private String filePath;
    private boolean goodCommentFunctions;
    private boolean goodCommentInitDoc;
    private boolean goodCommentVariables;
    private boolean goodCommentSubroutines;
    private boolean goodCommentControlStructures;

    public GoodComment(String filePath) {
        this.filePath = filePath;
        this.goodCommentControlStructures = false;
        this.goodCommentFunctions = false;
        this.goodCommentInitDoc = false;
        this.goodCommentSubroutines = false;
        this.goodCommentVariables = false;
    }

    public void analyseGoodComment() {
        //TODO aquí habría que optimizar esto para que se ejecuten todos los análisis de comentarios con dos o tres hilos
        //TODO aplicar esta misma técnica, porteriormente a las demás métricas.
        ExecutorService executor = Executors.newFixedThreadPool(3);
        executor.execute(new ControlStructureComment(filePath));
        executor.isTerminated();
    }

    public boolean isGoodCommentFunctions() {
        return goodCommentFunctions;
    }

    public boolean isGoodCommentInitDoc() {
        return goodCommentInitDoc;
    }

    public boolean isGoodCommentVariables() {
        return goodCommentVariables;
    }

    public boolean isGoodCommentSubroutines() {
        return goodCommentSubroutines;
    }

    public boolean isGoodCommentControlStructures() {
        return goodCommentControlStructures;
    }
}
