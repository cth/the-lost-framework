#!/bin/sh
#
# Trains on a number of training files simultaneously
#

NUM_PROCESSORS=14
PRISM2="~/prism/bin/prism"
TRAINING_FILES=
#for i in `seq 1 10`
#do
#    TRAINING_FILES="$TRAINING_FILES data/training_data_$i.pl.100"
#done

for file in data2/split*pl
do
    TRAINING_FILES="$TRAINING_FILES $file"
done

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
    echo "Waiting for process $1 to terminate"
    while [ 1 ]
    do
        running=`ps -eo pid |grep -c "$1"`
        if [ $running -eq 0 ]; then
            break
        fi
        sleep 1
    done 
    echo "Process terminated."
}

# Check if process with given PID is running
is_running() {
    ps -eo pid |grep -c "$1"
}

show_running_processes() {
    echo "-- Running processes: " >&2
    for i in `seq $NUM_PROCESSORS`
    do
	echo -en "\tProcessor[$i]: ${PROCESSOR[$i]}\n" >&2
    done
    echo "----------------------" >&2
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

echo "Init processors..."
init_processors
echo "done."

for f in $TRAINING_FILES
do
    show_running_processes
    echo "Trying to allocate processor for training file: $f"
    processor_id=`find_free_processor` # blocks!
    echo "found free processor: $processor_id"
    goals_file=`tempfile`
    echo "prism(clustgf),['clustgf.psm'],[erf_learn],erf_learn_file_count_only('$f'),save_counts_file('$f.counts')." > $goals_file
    echo "running prism with goals: "
    cat $goals_file
    echo "-----"
    exec $PRISM2 -l < $goals_file &> output_${processor_id}.txt & pid=$!
    PROCESSOR[$processor_id]=$pid
    echo "Started process pid=$pid on processor $processor_id"
    COUNT_FILES="$COUNT_FILES '$f.counts'"
done

echo "process ids $PROCESS_PIDS"

for index in `seq 1 $NUM_PROCESSORS`
do
    pid=${PROCESSOR[$index]}
    if [ $pid != "free" ]; then
	mywait $pid
    fi
done

COUNT_FILES=`echo $COUNT_FILES|tr " " ","`

# Merge training sessions
goals_file=`tempfile`
echo "prism(clustgf),['clustgf.psm'],[erf_learn],add_pseudo_counts,merge_counts_files([$COUNT_FILES]),set_switches_from_counts,save_counts_file('all.counts'),save_sw('final_switches.prb')." > $goals_file
cat $goals_file

exec $PRISM2 -l < $goals_file
