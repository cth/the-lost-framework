#!/bin/sh
#
# Predicts on a number of files simultaneously

NUM_PROCESSORS=$1
shift
ANNOTATE_GOAL=$1
shift
PARAMS_FILE=$1
shift
OUTPUT_FILE=$1
shift
INPUT_FILES_LIST=$1

PRISM="/opt/prism/bin/prism -s 1000000000"

echo "------- PREDICTION DATA FILES ----"
for i in `cat $INPUT_FILES_LIST`
do
        echo -en "\t$i\n"
        INPUT_FILES="$INPUT_FILES $i"
done

echo $PREDICTION_FILES

# Create and array of "free" processors of specified size 
init_processors() {
    i=1
    while [ $i -le $NUM_PROCESSORS ]
    do
        PROCESSOR[$i]="free"
        i=$(($i+1))
    done
}

member() {
        target=$1
        shift

        for i in $@
        do
                if [ x$i = "x$target" ]; then
                        echo "1"
                        return
                fi
        done
        echo "0"
}

# Wait that works for non-child processes
mywait() {
    echo "Waiting for process $1 to terminate"
    while [ 1 ]
    do
        running_processes=`ps -eo pid`
        running=`member $1 $running_processes`
        if [ $running -eq 0 ]; then
            break
        fi
        sleep 1
    done 
    echo "Process terminated."
}

# Check if process with given PID is running
is_running() {
    running_processes=`ps -eo pid`
    member $1 $running_processes

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

for f in $INPUT_FILES
do
    show_running_processes
    echo "Trying to allocate processor for file: $f"
    processor_id=`find_free_processor` # blocks!
    echo "found free processor: $processor_id"
    goals="['interface.pl'], $ANNOTATE_GOAL(['$PARAMS_FILE','$f'],[],'$f.predictions')."
    echo "running prism with goals: $goals"
    echo "-----"
    exec $PRISM  -g "$goals" & pid=$!
    PROCESSOR[$processor_id]=$pid
    echo "Started process pid=$pid on processor $processor_id"
    PREDICTION_FILES="$PREDICTION_FILES $f.predictions"
done

echo "process ids $PROCESS_PIDS"

for index in `seq 1 $NUM_PROCESSORS`
do
    pid=${PROCESSOR[$index]}
    if [ $pid != "free" ]; then
	mywait $pid
    fi
done

echo "Merging separate prediction files into file: $OUTPUT_FILE"

touch $OUTPUT_FILE
for f in $PREDICTION_FILES
do
        cat "$f" >> $OUTPUT_FILE
done

