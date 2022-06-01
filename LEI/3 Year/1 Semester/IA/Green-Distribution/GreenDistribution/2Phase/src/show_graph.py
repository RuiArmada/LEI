#!/usr/bin/env python3
import networkx as nx
import matplotlib.pyplot as plt

ADJ = nx.DiGraph()
nodes = set()

def read_graph():
    with open('arestas.pl','r') as f:
        for line in f:
            if line.startswith('%aresta(Src,Dest,Cost).'):
                continue
            l1 = line.replace('aresta(','')
            l2 = l1.replace(').','')
            data = l2.split(',')
            nodes.add(data[0])
            nodes.add(data[1])
            ADJ.add_edge(data[0],data[1])
            ADJ.add_edge(data[1],data[0])
    for a in nodes:
        ADJ.add_node(a)
    nx.draw_networkx( ADJ )
    plt.show()
    return ADJ, nodes

def main():
    read_graph()

if __name__ == "__main__":
    main()
