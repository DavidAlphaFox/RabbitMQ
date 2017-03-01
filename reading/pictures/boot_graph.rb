# before runing
# gem install ruby-graphviz
require 'graphviz'

g = GraphViz.new(:G,:type => :digraph)

pre_boot = g.add_nodes("pre_boot")
codec_correctness_check = g.add_nodes("codec_correctness_check")
file_handle_cache = g.add_nodes("file_handle_cache")

g.add_edges(codec_correctness_check,pre_boot)
g.add_edges(file_handle_cache,pre_boot)

worker_pool = g.add_nodes("worker_pool")
external_infrastructure = g.add_nodes("external_infrastructure")
database = g.add_nodes("database")
database_sync = g.add_nodes("database_sync")

g.add_edges(worker_pool,file_handle_cache)
g.add_edges(database,file_handle_cache)
g.add_edges(database_sync,database)

g.add_edges(external_infrastructure,codec_correctness_check)
g.add_edges(external_infrastructure,worker_pool)
g.add_edges(external_infrastructure,database)
g.add_edges(external_infrastructure,database_sync)

rabbit_registry = g.add_nodes("rabbit_registry")
rabbit_event = g.add_nodes("rabbit_event")
kernel_ready = g.add_nodes("kernel_ready")

g.add_edges(rabbit_registry,external_infrastructure)
g.add_edges(rabbit_event,external_infrastructure)
g.add_edges(kernel_ready,rabbit_event)
g.add_edges(kernel_ready,rabbit_registry)
g.add_edges(kernel_ready,external_infrastructure)

rabbit_alarm = g.add_nodes("rabbit_alarm")
rabbit_memory_monitor = g.add_nodes("rabbit_memory_monitor")
guid_generator = g.add_nodes("guid_generator")
delegate_sup = g.add_nodes("delegate_sup")
rabbit_node_monitor = g.add_nodes("rabbit_node_monitor")
core_initialized = g.add_nodes("core_initialized")

g.add_edges(rabbit_alarm,kernel_ready)
g.add_edges(rabbit_memory_monitor,rabbit_alarm)
g.add_edges(guid_generator,kernel_ready)
g.add_edges(delegate_sup,kernel_ready)
g.add_edges(rabbit_node_monitor, rabbit_alarm)
g.add_edges(rabbit_node_monitor,guid_generator)

g.add_edges(core_initialized,rabbit_alarm)
g.add_edges(core_initialized,guid_generator)
g.add_edges(core_initialized,delegate_sup)
g.add_edges(core_initialized,rabbit_memory_monitor)
g.add_edges(core_initialized,rabbit_node_monitor)

empty_db_check = g.add_nodes("empty_db_check")
recovery = g.add_nodes("recovery")
mirrored_queues = g.add_nodes("mirrored_queues")
routing_ready = g.add_nodes("routing_ready")

g.add_edges(empty_db_check,core_initialized)
g.add_edges(recovery,core_initialized)
g.add_edges(mirrored_queues,recovery)

g.add_edges(routing_ready,core_initialized)
g.add_edges(routing_ready,mirrored_queues)
g.add_edges(routing_ready,recovery)
g.add_edges(routing_ready,empty_db_check)

log_relay = g.add_nodes("log_relay")
direct_client = g.add_nodes("direct_client")
networking = g.add_nodes("networking")
notify_cluster = g.add_nodes("notify_cluster")
background_gc = g.add_nodes("background_gc")

g.add_edges(log_relay,routing_ready)
g.add_edges(direct_client,log_relay)
g.add_edges(networking,routing_ready)
g.add_edges(networking,log_relay)
g.add_edges(networking,background_gc)
g.add_edges(notify_cluster,networking)



g.output(:png => "boot_graph.png")
