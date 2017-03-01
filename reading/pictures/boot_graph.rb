require 'graphviz'

g = GraphViz.new(:G,:type => :digraph)

pre_boot = g.add_nodes("pre_boot")
codec_correctness_check = g.add_nodes("codec_correctness_check")
file_handle_cache = g.add_nodes("file_handle_cache")

g.add_edges(codec_correctness_check,pre_boot)
g.add_edges(file_handle_cache,pre_boot)

worker_pool = g.add_nodes("worker_pool")
external_infrastructure = g.add_nodes("external_infrastructure")

g.add_edges(worker_pool,file_handle_cache)
g.add_edges(external_infrastructure,codec_correctness_check)
g.add_edges(external_infrastructure,worker_pool)

g.output(:png => "boot_graph.png")
