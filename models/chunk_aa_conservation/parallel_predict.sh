#!/bin/sh
#
# Trains on a number of training files simultaneously
#

NUM_PROCESSORS=12
#PRISM2="~/prism/bin/prism" # Should be PRISM2
PRISM=prism

OUTPUT_FILE=$1
shift
INPUT_FILES=$@

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

echo "******* input files: $INPUT_FILES"

for f in $INPUT_FILES
do
    show_running_processes
    echo "Trying to allocate processor for training file: $f"
    processor_id=`find_free_processor` # blocks!
    echo "found free processor: $processor_id"
    goals="cl(interface), cl(options), original_options(Options), lost_best_annotation(['$f'],Options,'$f.output_lost_genefinder')."
    echo "running prism with goals: $goals"
    echo "-----"
    echo "$PRISM -g $goals"
    exec $PRISM -g "$goals" & pid=$!
    echo "******** Put in the background!!!!!"
    PROCESSOR[$processor_id]=$pid
    echo "Started process pid=$pid on processor $processor_id"
    OUTPUT_FILES="$OUTPUT_FILES $f.output_lost_genefinder"
done

echo "process ids $PROCESS_PIDS"

for index in `seq 1 $NUM_PROCESSORS`
do
    pid=${PROCESSOR[$index]}
    if [ $pid != "free" ]; then
	mywait $pid
    fi
done

echo "output_files: $OUTPUT_FILES"
echo "output_file: $OUTPUT_FILE"

cat $OUTPUT_FILES > "$OUTPUT_FILE"

first_output_file=`echo $OUTPUT_FILES | cut -d' ' -f1`
dir=`dirname $first_output_file`
for f in $dir/*chunk_conservation*
do
    echo "remove file: $f"
    #rm -f "$f"
done
