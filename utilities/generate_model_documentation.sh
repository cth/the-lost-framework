#!/bin/sh

SWIPL=pl
cd ..
LOST_BASE_DIR=`pwd`
cd -
LOST_MODELS_DIR=$LOST_BASE_DIR/models
LOST_DOC_DIR=$LOST_BASE_DIR/doc

make_pldoc_script() {
	cat $2 | grep ":- module" >>  $1 
	cat $2 | grep ":-module" >>  $1 
	echo ":- use_module(library(doc_latex))." >> $1
	cat $2 | grep -v "^:-" >> $1 
}

function latex_for_model() {
	model=$1
	safe_model=`echo $model|tr -d '_'`
	doc_file="$LOST_DOC_DIR/models/$safe_model.tex"
	MODEL_DOC_FILES="$MODEL_DOC_FILES $doc_file"
	#create_task_extractor "$model"
	prism -g "[extractor], go($model)." # /tmp/target.pl is generated	
	make_pldoc_script "/tmp/$model.pl" "/tmp/target.pl"
	cd /tmp/
	swipl -f "/tmp/$model.pl" -g "doc_latex('/tmp/$model.pl','$doc_file',[stand_alone(false),public_only(false),section_level(subsection)]),halt" -t "halt(1)" | grep -v "^%.*"
	cat $doc_file | sed "s/\/tmp\///g" | sed "s/$model.pl -- $model/$model/g" > $doc_file.1 # remove tmp prefix
	mv $doc_file.1 $doc_file
    rm -f "/tmp/target.pl"
#	rm -f "/tmp/$model.pl"
    cd -
}

function extract_task_declations() {
	cat "$1" | grep "task()"
}

if [ "x$1" = x ]; then
	rm -f $LOST_DOC_DIR/models/*
	MODEL_DOC_FILES=
	for dir in $LOST_MODELS_DIR/*
	do
		echo `basename $dir`
		latex_for_model `basename $dir`
	done
	
	# generate meta-include file:
	metainclude="$LOST_DOC_DIR/models.tex"
	echo "" > $metainclude
	for texfile in $MODEL_DOC_FILES
	do
		filebasename=`basename $texfile|cut -d'.' -f1`
		echo "\\input{models/$filebasename}" >> $metainclude
	done
	
	echo "TBD"
	exit
else
	latex_for_model "$1"
fi

