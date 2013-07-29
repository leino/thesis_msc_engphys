import sys
import subprocess

min_vertices, max_vertices = 3, 8
min_edge_size, max_edge_size = 2, 3

tournament_size = 100
iteration_stepsize = 1
min_iterations, max_iterations = 2, 30


hypergraph_classes = []

for num_vertices in range(min_vertices, max_vertices+1):
    for num_edges in range(2, 3+1):
        print("appending hypergraphs", num_vertices, num_edges)
        hypergraph_classes.append({'num_vertices': num_vertices, 'num_edges': num_edges, 'min_edge_size': min_edge_size, 'max_edge_size': max_edge_size})

if len(sys.argv) >= 2:
    database_filename = sys.argv[1]

    for hypergraph_class in hypergraph_classes:
        num_vertices = hypergraph_class['num_vertices']
        num_edges = hypergraph_class['num_edges']
        min_edge_size = hypergraph_class['min_edge_size']
        max_edge_size = hypergraph_class['max_edge_size']

        print('adding some hypergraphs into {0}...'.format(database_filename))

        # sketch out the hypergraph class
        args_hypergraphs = ["time", "runhaskell", "addhypergraphs.hs", \
                                database_filename, str(num_vertices), str(num_edges), \
                                '-d1:%(min_edge_size)d' % {'min_edge_size': min_edge_size}, \
                                '-D%(num_edges)d:%(max_edge_size)d' % {'num_edges': num_edges, 'max_edge_size': max_edge_size}]
        subprocess.check_call(args_hypergraphs)

    print("adding some experiments")
        
    # add experiments for perfect play
    subprocess.check_call(["time", "runhaskell", "addexperiments.hs", database_filename, "Perfect", "Perfect"])

    # add experiments for mcts vs perfect, for various numbers of iterations
    for i in range(min_iterations, max_iterations, iteration_stepsize):
        first_player_desc = 'UCT%(num_iterations)d' % {'num_iterations': i}
        subprocess.check_call(["time", "runhaskell", "addexperiments.hs", database_filename, first_player_desc, "Perfect", str(tournament_size)])

    print("plan and play")

    # plan and play (takes a long time)
    subprocess.check_call(["time", "runhaskell", "plan.hs", database_filename])
    subprocess.check_call(["time", "runhaskell", "-i../poga", "play.hs", database_filename])

else:
    print("you need to specify a database filename")
