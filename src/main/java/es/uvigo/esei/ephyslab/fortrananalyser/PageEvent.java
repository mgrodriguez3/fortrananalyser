/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package es.uvigo.esei.ephyslab.fortrananalyser;

import com.itextpdf.kernel.events.Event;
import com.itextpdf.kernel.events.IEventHandler;
import com.itextpdf.kernel.events.PdfDocumentEvent;
import com.itextpdf.kernel.geom.Rectangle;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfPage;
import com.itextpdf.kernel.pdf.canvas.PdfCanvas;
import com.itextpdf.layout.Canvas;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.property.TextAlignment;

/**
 * This class manage the event of each page in a document. 
 * 
 * @author Michael García Rodríguez
 * @version 1.9
 */
public class PageEvent implements IEventHandler {

    private final Document documento;

    public PageEvent(Document doc) {
        documento = doc;
    }
    
    /**
     * This method create a rectangle in header.
     * 
     * @param docEvent Document event
     * @return Area where header is
     */
    private Rectangle createHeaderRectangle(PdfDocumentEvent docEvent) {
        
        PdfDocument pdfDoc = docEvent.getDocument();
        PdfPage page = docEvent.getPage();        
        
        float xHeader = pdfDoc.getDefaultPageSize().getX() + documento.getLeftMargin();
        float yHeader = pdfDoc.getDefaultPageSize().getTop() - documento.getTopMargin();
        float widthHeader = page.getPageSize().getWidth() - 72;
        float heightHeader = 50F;

        Rectangle rectanguloEncabezado = new Rectangle(xHeader, yHeader, widthHeader, heightHeader);
        
        return rectanguloEncabezado;        
    }
    
    /**
     * This method create a rectangle in footer.
     * 
     * @param docEvent Document event 
     * @return Area where footer is.
     */
    private Rectangle createFooterRectangle(PdfDocumentEvent docEvent) {
        
        PdfDocument pdfDoc = docEvent.getDocument();
        PdfPage page = docEvent.getPage();
        
        float xFooter = pdfDoc.getDefaultPageSize().getX() + documento.getLeftMargin();
        float yFooter = pdfDoc.getDefaultPageSize().getBottom();
        float widthFooter = page.getPageSize().getWidth() - 72;
        float heightFooter = 50F;

        Rectangle rectanguloPie = new Rectangle(xFooter, yFooter, widthFooter, heightFooter);
        
        return rectanguloPie;
    }
    
    /**
     * This method create a table in header. 
     * 
     * @param mensaje 
     * @return Table with header
     */
    private Table createHeaderTable(String mensaje) {
        
        float[] anchos = {1F};
        Table tableHeader = new Table(anchos);
        tableHeader.setWidth(527F);

        tableHeader.addCell(mensaje);
        
        return tableHeader;
    }
    
    /**
     * This method create a table in footer.
     * 
     * @param docEvent The document event
     * @return Footer with page number
     */
    private Paragraph CreateFooterTable(PdfDocumentEvent docEvent) {
        
        PdfPage page = docEvent.getPage();
        Integer pageNum = docEvent.getDocument().getPageNumber(page) - 1;
        Paragraph p;
        
        if(pageNum > 0)
            p = new Paragraph("- "+pageNum.toString()+" -");
        else
            p = new Paragraph("");
        
        
        return p;
    }
    

    /**
     * This method manage the page change event: it add a header and a footer
     * on each page.
     * 
     * @param event Evento de pagina
     */
    @Override
    public void handleEvent(Event event) {
        
        
        PdfDocumentEvent docEvent = (PdfDocumentEvent) event;
        PdfDocument pdfDoc = docEvent.getDocument();
        PdfPage page = docEvent.getPage();
        float yPosition = pdfDoc.getDefaultPageSize().getBottom() + 10;
        float xPosition = pdfDoc.getDefaultPageSize().getX() + documento.getLeftMargin() + 260;
        PdfCanvas canvas = new PdfCanvas(page.newContentStreamBefore(), page.getResources(), pdfDoc);        
        
        //Table headerTable = this.createHeaderTable("Departamento de Recursos Humanos");
        //Rectangle headerRectangle = this.createHeaderRectangle(docEvent);        
        //Canvas headerCanvas = new Canvas(canvas, pdfDoc, headerRectangle);        
        //headerCanvas.add(headerTable);      

        //Table numsTable = this.CreateFooterTable(docEvent);
        Rectangle footerRectangle = this.createFooterRectangle(docEvent);
        Canvas footerCanvas = new Canvas(canvas, pdfDoc, footerRectangle);
        

        footerCanvas.showTextAligned(this.CreateFooterTable(docEvent), xPosition,
                yPosition, TextAlignment.CENTER);
        
    }
}
