/*
 * Copyright (C) 2019 Michael García Rodríguez
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

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Michael García Rodríguez
 */
public class Graph {
    
    private List<Node> graph;
 
    public void addNode(Node node) {
        if (graph == null) {
            graph = new ArrayList<>();
        }
        graph.add(node);
    }
 
    public List<Node> getGraph() {
        return graph;
    }
 
    @Override
    public String toString() {
        return "Graph [nodes=" + graph + "]";
    }
    
}
