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
START_RANGE = 5
STEP_RANGE = 25
END_TIME = 2*60
CSV_SEPARATOR = ','

if __name__ == "__main__":
    densities = [(0.01,5,1000),(0.05,5,1000),(0.1,5,800),(0.25,5,500),(0.5,5,500), (1,5,400)]

    for (d,step,end) in densities:
        if (d >= 1):
            dstr = str(d)
        else:
            dstr = "0" + str(d)[2:]

        filename = "./data/" + dstr + ".csv"

        start = START_RANGE
        done = set()
        if os.path.isfile(filename):
            print("Checking file", filename)
            with open(filename, "r") as f:
                for line in f:
                    done.add(int(line.split(" ")[0]))

        if len(done) > 0: print(f"Might skip {len(done)} tests")
        with open(filename, "a+") as f:
            for gsize in range(start, end+1, step):
                if gsize in done: continue
                G = nx.gnp_random_graph(gsize, d, directed=True, seed=SEED)

                random.seed(SEED)
                for (u,v,w) in G.edges(data=True):
                    w["weight"] = random.randint(MIN_WEIGHT,MAX_WEIGHT)
                    if random.random() < P_NEG:
                        w["weight"] = -w["weight"]
                    # print(u,v,d["weight"])

                print(f"D: {d:.3f}, S: {gsize:6d}, E: {len(G.edges):9d},  ", end='', flush=True)
                nx.write_weighted_edgelist(G, path="/tmp/graphin")

                r = []
                for i in range(0,3):
                    p = sp.Popen([EXE_FILE, "-s", "0", "/tmp/graphin", "/tmp/graphout"], encoding=None)
                    p.wait()
                    line = sp.check_output(['tail', '-1', "/tmp/graphout"], encoding='utf-8')
                    # r.append(resource.getrusage(resource.RUSAGE_CHILDREN).ru_utime)
                    r.append(float(line.split(" ")[-1]))

                ravg = sum(r)/len(r)
                rmin = min(r)
                
                print(f"T: {rmin:4.6f}s")
                print(gsize, ravg, rmin, len(G.edges), file=f, flush=True)

                if rmin > END_TIME: break
