/*
 * Copyright (C) 2020 Miki
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
package es.uvigo.esei.ephyslab.fortrananalyser.elementsGui;

/**
 *
 * @author Michael García Rodríguez
 */
public class CustomCheckBoxDM extends javax.swing.JCheckBox
{
    // <editor-fold defaultstate="collapsed" desc="Variable declaration">
    private boolean colored = false;
    private boolean rised = false;
    private Colors selectedColor = Colors.JUG_GREEN;
    protected static final String COLORED_PROPERTY = "colored";
    protected static final String COLOR_PROPERTY = "color";
    protected static final String RISED_PROPERTY = "rised";
    // </editor-fold>

    // <editor-fold defaultstate="collapsed" desc="Constructor">
    public CustomCheckBoxDM()
    {
        super();
        setPreferredSize(new java.awt.Dimension(100, 26));
    }
    // </editor-fold>

    // <editor-fold defaultstate="collapsed" desc="Getter/Setter">
    public boolean isColored()
    {
        return this.colored;
    }

    public void setColored(final boolean COLORED)
    {
        final boolean OLD_STATE = this.colored;
        this.colored = COLORED;
        firePropertyChange(COLORED_PROPERTY, OLD_STATE, COLORED);
        repaint();
    }

    public boolean isRised()
    {
        return this.rised;
    }

    public void setRised(final boolean RISED)
    {
        final boolean OLD_VALUE = this.rised;
        this.rised = RISED;
        firePropertyChange(RISED_PROPERTY, OLD_VALUE, RISED);
    }

    public Colors getSelectedColor()
    {
        return this.selectedColor;
    }

    public void setSelectedColor(final Colors SELECTED_COLOR)
    {
        final Colors OLD_COLOR = this.selectedColor;
        this.selectedColor = SELECTED_COLOR;
        firePropertyChange(COLOR_PROPERTY, OLD_COLOR, SELECTED_COLOR);
        repaint();
    }

    @Override
    public void setUI(final javax.swing.plaf.ButtonUI BUI)
    {
        super.setUI(new SteelCheckBoxUI(this));
    }

    public void setUi(final javax.swing.plaf.ComponentUI UI)
    {
        this.ui = new SteelCheckBoxUI(this);
    }

    @Override
    protected void setUI(final javax.swing.plaf.ComponentUI UI)
    {
        super.setUI(new SteelCheckBoxUI(this));
    }
    // </editor-fold>

    @Override
   public String toString()
    {
        return "SteelCheckBox";
    }
    
}
