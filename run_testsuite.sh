#!/bin/sh


case $# in
	1)  prism -g "[lost], use(unittest), test_and_report_single_suite('$1')";;
	2)  prism -g "[lost], use(unittest), test_and_report_single_testcase('$1','$2')";;
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



