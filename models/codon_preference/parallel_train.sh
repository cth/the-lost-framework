#!/bin/sh
#
# Trains on a number of training files simultaneously

NUM_PROCESSORS=$1
shift
OUTPUT_FILE=$1
shift
TRAINING_FILES_LIST=$1


PRISM="/opt/prism/bin/prism -s 1000000000"

echo "------- TRAINING DATA FILES ----"
for i in `cat $TRAINING_FILES_LIST`
do
        echo -en "\t$i\n"
        TRAINING_FILES="$TRAINING_FILES $i"
done

echo $TRAINING_FILES

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
    echo "Waiting for process $1 to terminate"
    while [ 1 ]
    do
        running=`ps -eo pid |grep -c "^$1\$"`
        if [ $running -eq 0 ]; then
            break
        fi
        sleep 1
    done 
    echo "Process terminated."
}

# Check if process with given PID is running
is_running() {
    ps -eo pid |grep -c "^$1\$"
}

# Display information about running processes
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
    goals="['../../lost.pl'],lost_include_api(autoAnnotations),prismAnnot(codon_pref,direct),['codon_prefEX.psm'],lost_include_api(viterbi_learn),viterbi_learn_file_count_only('$f'),write('here'),nl,save_counts_file('$f.counts')."
    echo "running prism with goals: $goals"
    echo "-----"
    exec $PRISM  -g "$goals" & pid=$!
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
goals="['../../lost.pl'],lost_include_api(autoAnnotations),prismAnnot(codon_pref,direct),['codon_prefEX.psm'],lost_include_api(viterbi_learn),!,add_pseudo_counts,merge_counts_files([$COUNT_FILES]),set_switches_from_counts,write('$OUTPUT_FILE'),save_sw('$OUTPUT_FILE')."


exec $PRISM -g "$goals"
