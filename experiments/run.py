import sys
import subprocess

num_vertices, num_edges = 4, 3
min_edge_size, max_edge_size = 2, 3

tournament_size = 100

iteration_stepsize = 1
min_iterations, max_iterations = 2, 20

if len(sys.argv) >= 2:
    database_filename = sys.argv[1]

    print('adding hypergraphs into {0}...'.format(database_filename))

    # sketch out the hypergraph class
    
    subprocess.check_call(["time", "runhaskell", "addhypergraphs.hs", database_filename, str(num_vertices), str(num_edges), '-d1:%(min_edge_size)d' % {'min_edge_size': min_edge_size}, '-D%(num_edges)d:%(max_edge_size)d' % {'num_edges': num_edges, 'max_edge_size': max_edge_size}])

    # add experiments for perfect play
    subprocess.check_call(["time", "runhaskell", "addexperiments.hs", database_filename, "Perfect", "Perfect"])

    # add experiments for mcts vs perfect, for various numbers of iterations
    for i in range(min_iterations, max_iterations, iteration_stepsize):
        first_player_desc = 'UCT%(num_iterations)d' % {'num_iterations': i}
        subprocess.check_call(["time", "runhaskell", "addexperiments.hs", database_filename, first_player_desc, "Perfect", str(tournament_size)])

    # plan and play (takes a long time)
    subprocess.check_call(["time", "runhaskell", "plan.hs", database_filename])
    subprocess.check_call(["time", "runhaskell", "-i../poga", "play.hs", database_filename])

else:
    print("you need to specify a database filename")
