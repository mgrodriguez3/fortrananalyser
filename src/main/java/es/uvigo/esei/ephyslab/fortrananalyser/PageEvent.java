/*
 * Copyright (C) 2019 Michael Garvcía Rodríguez
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
import com.itextpdf.layout.property.TextAlignment;

public class PageEvent implements IEventHandler {

    private final Document report;

    public PageEvent(Document doc) {
        report = doc;
    }

    public Rectangle createFooterRectangle(PdfDocumentEvent docEvent) {
        PdfDocument pdfDoc = docEvent.getDocument();
        PdfPage page = docEvent.getPage();
        float xFooter = pdfDoc.getDefaultPageSize().getX() + report.getLeftMargin();
        float yFooter = pdfDoc.getDefaultPageSize().getBottom();
        float widthFooter = page.getPageSize().getWidth() - 72;
        float heightFooter = 50F;

        return new Rectangle(xFooter, yFooter, widthFooter, heightFooter);
    }

    private Paragraph createFooterTable(PdfDocumentEvent docEvent) {
        PdfPage page = docEvent.getPage();
        int pageNum = docEvent.getDocument().getPageNumber(page) - 1;
        Paragraph p;
        if (pageNum > 0) {
            p = new Paragraph("- " + Integer.toString(pageNum) + " -");
        } else {
            p = new Paragraph("");
        }
        return p;
    }

    @Override
    public void handleEvent(Event event) {
        PdfDocumentEvent docEvent = (PdfDocumentEvent) event;
        PdfDocument pdfDoc = docEvent.getDocument();
        PdfPage page = docEvent.getPage();
        float yPosition = pdfDoc.getDefaultPageSize().getBottom() + 10;
        float xPosition = pdfDoc.getDefaultPageSize().getX() + report.getLeftMargin() + 260;
        PdfCanvas canvas = new PdfCanvas(page.newContentStreamBefore(), page.getResources(), pdfDoc);
        Rectangle footerRectangle = createFooterRectangle(docEvent);
        Canvas footerCanvas = new Canvas(canvas, pdfDoc, footerRectangle);
        footerCanvas.showTextAligned(createFooterTable(docEvent), xPosition,
                yPosition, TextAlignment.CENTER);
    }
}
