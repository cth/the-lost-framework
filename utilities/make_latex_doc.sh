#!/bin/sh

SWIPL=pl
LOST_BASE_DIR=..
LOST_DOC_DIR=$LOST_BASE_DIR/doc

make_pldoc_script() {
	echo ":- use_module(library(doc_latex))." > $1
	echo ":- ['$2']." >> $1
}

make_latex_for_prolog_file() {
        dirname=`dirname $1`
        base_file_name=`basename $1`
	prefix_name=`echo "$base_file_name" | cut -d. -f1`
        doc_file="$2/$prefix_name.tex"
        echo "$1 -> $doc_file"

        cd $dirname
	make_pldoc_script "tmp_latex_script.pl" "$base_file_name"
	pl -f tmp_latex_script.pl -g "doc_latex('$base_file_name','$doc_file',[stand_alone(false)]),halt" -t "halt(1)"
        rm -f "tmp_latex_script.pl"
        cd -
}


# Make documentation for lib/
#
mkdir $LOST_DOC_DIR/lib
for file in $LOST_BASE_DIR/lib/*
do 
        echo "Generating Latex documentation for file: $file"
        make_latex_for_prolog_file "$file" "$LOST_DOC_DIR/lib"
done
