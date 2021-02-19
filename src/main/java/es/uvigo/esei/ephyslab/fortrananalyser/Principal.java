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

import es.uvigo.esei.ephyslab.fortrananalyser.GuiComponent.MainWindow;
import es.uvigo.esei.ephyslab.fortrananalyser.exception.EditableException;

import javax.swing.SwingUtilities;

/**
 * @author Michael García Rodríguez
 * @version 2.1
 */
public class Principal {

    public static void main(String[] args) throws EditableException {
        org.apache.log4j.BasicConfigurator.configure();
        if (args.length == 0) {
            SwingUtilities.invokeLater(() -> {
                MainWindow mw;
                mw = new MainWindow();
                mw.setVisible(true);
            });
        } else {
            if (args.length == 3) {
                String language = args[0];
                String path = args[1];
                String outputFileName = args[2];
                new MainWindow(language, path, outputFileName);
            } else {
                throw new EditableException(111);
            }
        }
    }
}
