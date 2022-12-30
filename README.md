# Hascade

A massively parallel solver for the Influence Maximization problem, in the context of Independent Cascade models.

### To run: 

`stack run -- [k] data/wiki_adj_list_[size].txt +RTS -lf -N[cores] -s`

where `k` is the number of seed vertices, `size` is the total number of vertices in the graph (this has to correspond to an actual data file), and `cores` is the number of CPU cores to be ran in parallel.

### To view ThreadScope:

`threadscope hascade-exe.eventlog`

### To generate data files: 

`cd data && python3 preprocess.py [size]`
