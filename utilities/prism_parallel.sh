#!/bin/sh
#  
# This is a simple shell script for running multiple 
# processes in parallel. 
# 
# The script takes three arguments: 
# 1: The path to the PRISM binary to be used 
# 2: Number of parallel processes
# 3: The name of a file where each line contains a sequence of goals to 
#    be executed by PRISM. 
# 
# This script terminates only when prism have been invoked with each set
# of goals in the task list and all such processes have terminated.

PRISM_BINARY=$1; shift
NUM_PROCESSORS=$1; shift
TASK_LIST_FILE=$1; shift

pp_echo() {
        echo "PP: $1"
}

pp_echo "Running generic parallel prism script: "
pp_echo "Number of processors: $NUM_PROCESSORS"
pp_echo "Task list file: $TASK_LIST_FILE"
pp_echo "------- INITIALIZING TASK LIST  -------"
i=0
while read line
do
        i=$(($i+1))
        pp_echo "task[$i] : $line"
        TASKS[$i]="$line"
done < $TASK_LIST_FILE 

NUMBER_OF_TASKS=$i

# Create and array of "free" processors of specified size 
init_processors() {
    i=1
    while [ $i -le $NUM_PROCESSORS ]
    do
        PROCESSOR[$i]="free"
        i=$(($i+1))
    done
}


# Wait that works for non-child processes
mywait() {
    pp_echo "Waiting for process $1 to terminate"
    while [ 1 ]; do
        running=`ps -eo pid |grep -c "$1"`
        [ $running -eq 0 ] && break
        sleep 1
    done 
    pp_echo "Process terminated."
}

# Check if process with given PID is running
is_running() {
    ps -eo pid |grep -c "$1"
}

# Display information about running processes
show_running_processes() {
    pp_echo "-- Running processes: " >&2
    for i in `seq $NUM_PROCESSORS`
    do
	echo -en "\tProcessor[$i]: ${PROCESSOR[$i]}\n" >&2
    done
    pp_echo "----------------------" >&2
}

# Finds a processor that is not doing anything
# block untill such is found
find_free_processor() {
    next_free_processor="nil"
    while [ 1 ]
    do
        if [ $next_free_processor != "nil" ]; then
            echo $next_free_processor
            return
        fi
        sleep 1 # wait for a bit

	i=1
        while [ $i -le $NUM_PROCESSORS ]
        do
            pid=${PROCESSOR[$i]}
	    running=`is_running $pid`

            if [ $pid = "free" ] || [ $running -eq 0 ]; then
		PROCESSOR[$i]="free"
		next_free_processor=$i
	    fi
            i=$(($i+1))
        done
	show_running_processes
    done
}

# main: 

pp_echo "Init processors..."
init_processors
pp_echo "done."

for taskid in `seq 1 $NUMBER_OF_TASKS`
do
    # Get next available processer
    show_running_processes
    pp_echo "Trying to allocate processor for training file: $f"
    processor_id=`find_free_processor` # blocks!
    pp_echo "found free processor: $processor_id"
    pp_echo "running prism with goals: ${TASKS[$taskid]}"
    exec $PRISM_BINARY -g "${TASKS[$taskid]}" & pid=$!
    PROCESSOR[$processor_id]=$pid
    pp_echo "Started task $taskid process with pid=$pid on processor $processor_id"
done

pp_echo "process ids $PROCESS_PIDS"

for index in `seq 1 $NUM_PROCESSORS`
do
    pid=${PROCESSOR[$index]}
    if [ $pid != "free" ]; then
	mywait $pid
    fi
done
