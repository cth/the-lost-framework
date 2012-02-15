#!/bin/sh

PRISM=`cat lost.pl | grep "prism_command" | grep -v "^%.*" | cut -d"'" -f 2`

case $# in
	1)  $PRISM -g "[lost], use(unittest), test_and_report_single_suite('$1')";;
	2)  $PRISM -g "[lost], use(unittest), test_and_report_single_testcase('$1','$2')";;
	*) 
		echo "Wrong number of arguments $#"
		echo "Usage is:"
		echo "$0 testsuite"
		echo "    which runs all testcases in testsuite."
		echo "or "
		echo "$0 testsuite testcase"
		echo "    which runs testcase in testsuite." 
		;;
esac



