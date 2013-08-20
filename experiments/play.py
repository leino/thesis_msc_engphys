import sys
import subprocess

# number of cores:
# can be figured out by counting the number of lines in '$ grep processor /proc/cpuinfo'
num_cores = 4

if len(sys.argv) >= 2:
    database_filename = sys.argv[1]

    # comile play command
    print("compiling the play command")
    subprocess.check_call(["ghc", "-O2", "-threaded", "-rtsopts", "-i../poga", "--make", "play.hs"])

    # play with the given number of cores
    print('playing (using %(num_cores)d cores)' % {'num_cores': num_cores})
    subprocess.check_call(["time", "./play", database_filename, "+RTS", '-N%(num_cores)d'%{'num_cores': num_cores}])

else:
    print("you need to specify a database filename")
