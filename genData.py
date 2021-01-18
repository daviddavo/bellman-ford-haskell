#!/usr/bin/python
import sys,os
import time
import subprocess as sp
import resource
import random
import networkx as nx
import itertools as it
from networkx.generators.random_graphs import *

EXE_FILE="bellmanford-exe"
MAX_WEIGHT = 1000
MIN_WEIGHT = 1
P_NEG = 0.005
SEED = 42
START_RANGE = 50
STEP_RANGE = 50
END_RANGE = 500
CSV_SEPARATOR = ','

if __name__ == "__main__":
    densities = [0.01,0.05,0.1,0.25,0.5, 1]

    for d in densities:
        if (d >= 1):
            dstr = str(d)
        else:
            dstr = "0" + str(d)[2:]

        filename = "./data/" + dstr + ".csv"

        start = START_RANGE
        if os.path.isfile(filename):
            line = sp.check_output(['tail', '-1', filename], encoding='utf-8')
            start = int(line.split(" ")[0]) + STEP_RANGE

        with open(filename, "a+") as f:
            for gsize in range(start, END_RANGE, STEP_RANGE):
                G = nx.gnp_random_graph(gsize, d, directed=True, seed=SEED)

                random.seed(SEED)
                for (u,v,w) in G.edges(data=True):
                    w["weight"] = random.randint(MIN_WEIGHT,MAX_WEIGHT)
                    if random.random() < P_NEG:
                        w["weight"] = -w["weight"]
                    # print(u,v,d["weight"])

                print(f"D: {d:.3f}, S: {gsize:6d}, E: {len(G.edges):9d}", end='\n', flush=True)
                nx.write_weighted_edgelist(G, path="/tmp/graphin")

                r = []
                for i in range(0,3):
                    p = sp.Popen([EXE_FILE, "-s", "0", "/tmp/graphin", "/tmp/graphout"], encoding=None)
                    p.wait()
                    r.append(resource.getrusage(resource.RUSAGE_CHILDREN).ru_utime)

                ravg = sum(r)/len(r)
                rmin = min(r)
                
                print(gsize, ravg, rmin, file=f, flush=True)
