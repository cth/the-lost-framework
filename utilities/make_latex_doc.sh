#!/bin/sh
#
# A script to generate latex documentation for all the Prolog files in the lib/ directory
#
# author: Christian Theil Have

SWIPL=pl
cd ..
LOST_BASE_DIR=`pwd`
cd -
LOST_DOC_DIR=$LOST_BASE_DIR/doc

#make_pldoc_script() {
#	echo ":- use_module(library(doc_latex))." > $1
#	echo "lost_include_api(X)." > $1 
#	echo ":- ['$2']." >> $1
#}

#make_latex_for_prolog_file() {
#        dirname=`dirname $1`
#        base_file_name=`basename $1`
#	prefix_name=`echo "$base_file_name" | cut -d. -f1`
#	prefix_name_safe=`echo $prefix_name|tr -d '_'`
#        doc_file="$2/$prefix_name_safe.tex"
#        echo "$1 -> $doc_file"
#	LIB_DOC_FILES="$LIB_DOC_FILES $doc_file"
#        cd $dirname
#	make_pldoc_script "tmp_latex_script.pl" "$base_file_name"
#	swipl -f tmp_latex_script.pl -g "doc_latex('$base_file_name','$doc_file',[stand_alone(false)]),halt" -t "halt(1)"
#        rm -f "tmp_latex_script.pl"
#        cd -
#}

make_pldoc_script() {
	cat $2 | grep ":- module" >>  $1 
	cat $2 | grep ":-module" >>  $1 
	echo ":- use_module(library(doc_latex))." >> $1
	cat $2 | grep -v "^:-" >> $1 
}

make_latex_for_prolog_file() {
	utils_dir=`pwd`
    dirname=`dirname $1`
	echo $dirname
    base_file_name=`basename $1`
	prefix_name=`echo "$base_file_name" | cut -d. -f1`
	prefix_name_safe=`echo $prefix_name|tr -d '_'`
    doc_file="$2/$prefix_name_safe.tex"
    echo "$1 -> $doc_file"
	echo "" > "$doc_file"
	LIB_DOC_FILES="$LIB_DOC_FILES $doc_file"
	echo "dirname: $dirname"
	cd $dirname
	make_pldoc_script "/tmp/$base_file_name" "$base_file_name"
	cd -
	cd /tmp/
	echo $base_file_name
	swipl -f $base_file_name -g "doc_latex('./$base_file_name','$doc_file',[stand_alone(false),public_only(false),section_level(section)]),halt" -t "halt(1)" | grep -v "^%.*"
	cat $doc_file | sed "s/\/tmp\///g" > $doc_file.1 # remove tmp prefix
	mv $doc_file.1 $doc_file
    rm -f "$base_file_name"
    cd -
}


# Make documentation for lib/
#
if [ "x$1" = "x" ]; then
	mkdir $LOST_DOC_DIR/lib
	LIB_DOC_FILES=
	for file in $LOST_BASE_DIR/lib/*pl
	do 
        echo "Generating Latex documentation for file: $file"
        make_latex_for_prolog_file "$file" "$LOST_DOC_DIR/lib"
	done

	# Make indirect include file for latex:
	metainclude="$LOST_DOC_DIR/libraries.tex"
	echo "" > $LOST_DOC_DIR/libraries.tex
	for texfile in $LIB_DOC_FILES
	do
		filebasename=`basename $texfile|cut -d'.' -f1`
		echo "\\input{lib/$filebasename}" >> $metainclude
	done
else
	make_latex_for_prolog_file "$1" "$LOST_DOC_DIR/lib"
fi

