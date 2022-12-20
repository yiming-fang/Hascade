from collections import defaultdict
import sys

max_node = float("inf")
output_fname ="wiki_adj_list.txt"
if len(sys.argv) == 2:
    max_node = int(sys.argv[1])
    output_fname = "wiki_adj_list_"+str(max_node)+".txt"

adj_list = defaultdict(list)
with open ("wiki_talk.txt") as fin:
    for l in fin:
        if l[0] == "#":
            continue
        (s, t) = l.split("\t")
        t = t[:-1] # trim off newline
        if int(s) < max_node and int(t) < max_node:
            adj_list[s].append(t)

with open (output_fname, "w") as fout:
    for e in adj_list.keys():
        fout.write(e + " " + " ".join(adj_list[e]) + "\n")
