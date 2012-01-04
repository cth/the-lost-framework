#!/bin/sh

if [ "x$1" = "xbackground" ]; then
	sleep 1
	echo Running in background
	sleep 1
	echo Running in background
	sleep 1
	echo Running in background
else
	$0 background $@ &
fi
