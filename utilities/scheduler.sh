#!/bin/sh
#  
# This is a simple shell script for running multiple processes in parallel. 
# 
# The script takes three arguments: 
# 1: The path to the PRISM binary to be used 
# 2: Number of parallel processes
# 3: The name of a file where each line contains a sequence of goals to 
#    be executed by PRISM. Note that this file may be pipe..
# 4: A file to communicate completed tasks back to PRISM
# 
# This script terminates only when prism have been invoked with each set
# of goals in the task list and all such processes have terminated.

# Known bugs:
# show processes before they are running!

pp_echo() {
        echo "progrun:\t$1"
}

first() {
        echo $1
}

rest() {
        shift
        echo $@
}

init_processors() {
    i=1
    while [ $i -le $NUM_PROCESSORS ]
    do
        PROCESSOR[$i]="free"
        i=$(($i+1))
    done
}

#report_completion() {
#	echo ${PROCESSOR_TASK[$i]} >> $COMPLETED_TASKS_FILE
#}

# Wait that works for non-child processes
mywait() {
    pp_echo "Waiting for process $1 to terminate"
    while [ 1 ]; do
		running=`member $1 $running_processes`
        if [ $running -eq 0 ]; then
	 		break
		fi
        sleep 1
    done 
    pp_echo "Process $1 terminated."
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

# Check if process with given PID is running
is_running() {
    running_processes=`ps -eo pid`
    member $1 $running_processes
}

cpu_usage() {
	ps -p $1 -o %cpu | grep -v "%CPU"
}

memory_usage() {
	ps -p $1 -o %mem | grep -v "%MEM"
}

# 1: if some (one or more) processes are still running
any_running() {
	i=0
	while [ $i -lt $NUM_PROCESSORS ]
	do
		i=$(($i+1))		
		if [ "${PROCESSOR[$i]}" = "free" ]; then
			continue
		else
			echo "1"
			return
		fi
	done
	echo "0"
}

exit_if_parent_dead() {
	if [ `is_running $PARENT_PID` = "0" ]; then
		pp_echo "Parent process has terminated. So will I. Goodbye."
		exit
	fi
}

# Display information about running processes
show_running_processes() {
    pp_echo "-- Running processes: " >&2
	i=0
	while [ $i -lt $NUM_PROCESSORS ]
	do
		i=$(($i+1))
		if [ ${PROCESSOR[$i]} != "free" ]; then 
			taskid="${PROCESSOR_TASK[$i]}"
			task=${TASKS[$taskid]}			
			model=`basename "${MODEL_DIRECTORY[$taskid]}"`
			proc_cpu=`cpu_usage ${PROCESSOR[$i]}`
			proc_mem=`memory_usage ${PROCESSOR[$i]}`
		else
			taskid="n/a"
			task="n/a"
			model="n/a"
			proc_cpu="n/a"
			proc_mem="n/a"
		fi
		pp_echo "Processor[$i]: \tpid: ${PROCESSOR[$i]}  \tcpu: $proc_cpu \tmemory: $proc_mem \tModel: $model \ttask: $task" >&2
    done
    pp_echo "----------------------" >&2
}

# Check if running tasks have completed and if so
# write the task id to the completed tasks file to notify producer
check_and_report_completed() {
	i=0
	while [ $i -lt $NUM_PROCESSORS ]
	do
			i=$(($i+1))
            pid=${PROCESSOR[$i]}
			if [ "$pid" = "free" ]; then
				continue
			else
	    		running=`is_running $pid`
				if [ $running -eq 0 ]; then
					echo ${PROCESSOR_TASK[$i]} >> $COMPLETED_TASKS_FILE
					PROCESSOR[$i]="free"
					PROCESSOR_TASK[$i]=""
					
				fi
			fi
	done
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
	
            if [ $pid = "free" ]; then
				PROCESSOR[$i]="free"
				next_free_processor=$i
	    	elif [ $running -eq 0 ]; then
				echo ${PROCESSOR_TASK[$i]} >> $COMPLETED_TASKS_FILE
				PROCESSOR[$i]="free"
				next_free_processor=$i
	    	fi
            i=$(($i+1))
        done
    done
}

update_state() {
	exit_if_parent_dead
	check_and_report_completed
	show_running_processes
	sleep 1	
}

# Trick to make a script put it self in the background

if [ "x$1" = "xbackground" ]; then
	shift
	PARENT_PID=$1; shift
	PRISM_BINARY=$1; shift
	NUM_PROCESSORS=$1; shift
	PENDING_TASKS_FILE=$1; shift
	COMPLETED_TASKS_FILE=$1; shift
else # relaunce
	PARENT_PID=`ps -p $$ -o ppid=`
	echo "Starting child process:"
	echo "$0 background $PARENT_PID $@ &"
	$0 background $PARENT_PID "$@" & 
#	mywait $! # Wait for child to terminate
	exit
fi


pp_echo "Running generic parallel prism script: "
pp_echo "Number of processors: $NUM_PROCESSORS"
pp_echo "Pending tasks file: $PENDING_TASKS_FILE"
pp_echo "Completed tasks file: $COMPLETED_TASKS_FILE"
pp_echo "PRISM BINARY: $PRISM_BINARY"
pp_echo "PARENT ID: $PARENT_ID"
pp_echo "------- INITIALIZING TASK LIST  -------"

# Reset complement task file
echo "" > $COMPLETED_TASKS_FILE

# Make sure that the pending task file exist before proceeding
while [ 1 ]
do
	if [ -f $PENDING_TASKS_FILE ]; then
		pp_echo "Pending file: $PENDING_TASKS_FILE is ready"
		break
	fi
done

init_processors

# Main loop:
# Read tasks from PENDING_TASKS_FILE and allocate them to a free processor
# If processors are currently available, the loop blocks until a running 
# task completes.
mode="taskid"
shutdown=false
ready_task="false"
no_lines_read=0
no_lines_processed=0
while [ $shutdown = false ]
do
	no_lines_read=0
	while read line
	do
		no_lines_read=$((no_lines_read+1))

		if [ $no_lines_read -le $no_lines_processed ]; then
			#echo "not (re)-processing line: $line"
			continue
		fi
		
		no_lines_processed=$(($no_lines_processed+1))
		
		#echo "lines processed: $no_lines_processed"
		
	
		if [ "x$line" = "x" ]; then
			echo "Reading empty line"
			no_lines_read=$((no_lines_read+1))
			sleep 1
			continue
		elif [ "x$line" = "xhangup" ]; then
			shutdown=true
			break
		fi
	
		pp_echo $line
		if [ "$mode" = "taskid" ]; then
			taskid="$line"
			mode="directory"
			pp_echo "Read task id: $taskid"
		elif [ "$mode" = "directory" ]; then
			MODEL_DIRECTORY[$taskid]="$line"
			mode="prism_goal"
			pp_echo "Read directory: $line"
		elif [ "$mode" = "prism_goal" ]; then
			TASKS[$taskid]="$line"
			mode="taskid"
			ready_task="true"
			pp_echo "Read task: $line"
			break
		fi
	done < "$PENDING_TASKS_FILE"
	
	if [ $shutdown = true ]; then
		break
	fi
	
	if [ $ready_task = "true" ]; then
		processor_id=`find_free_processor` # blocks untill processor is available.
		pp_echo "task[$taskid] : $line"
		cd "${MODEL_DIRECTORY[$taskid]}"
		echo "[interface], $line, halt." > "/tmp/scheduler_goals_task_$taskid.pl"
		`first $PRISM_BINARY` `rest $PRISM_BINARY` -l < "/tmp/scheduler_goals_task_$taskid.pl" &> "/tmp/log_$taskid" & pid=$!
		cd -
		PROCESSOR[$processor_id]=$pid
		PROCESSOR_TASK[$processor_id]=$taskid
		ready_task="false"
	fi
	
	update_state
done

pp_echo "Got hangup from Prolog scheduler.."

# Wait for processes to complete.
while [ `any_running` -ne 0 ]
do
	update_state
done

echo "hangup" >>  $COMPLETED_TASKS_FILE

pp_echo "shutting down."
