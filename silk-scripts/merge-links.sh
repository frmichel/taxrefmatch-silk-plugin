#!/bin/bash
# This script merges a set of files containing alignement triples generated by SILK.
# Files to merge are produced according to the TAXREF-MATCH algorithm (function TAXREFMATCH-SHORT):
# they provide triples aligning TAXREF with other peer ontologies.
#
# Processing:
# - Files 4.1 and 4.2 are intersected. Idem for files 5.1 and 5.2.
# - Triples of the first file are all kept.
#   Then, for subsequent files, triples are kept if their subject is not already 
#   mentioned as the subject of triples previously kept.
#
# Input arguments:
# - arg1: name of the peer ontology, e.g. agrovoc ($DS)
# Output: 
#   File links-${DS}.nt
#
# Author: F. Michel, CNRS I3S

help()
{
  exe=$(basename $0)
  echo "Usage: $exe <dataset name>"
  echo "Call example:"
  echo "   $exe agrovoc"
  exit 1
}

# Read input arguments
DS=$1
if [[ -z "$DS" ]] ; then help; fi


OUTPUT=links-${DS}.nt

FILE1=links-${DS}-1-equality-fullname.nt
FILE2=links-${DS}-2-equality-fullname-nodate.nt
FILE3=links-${DS}-3-equality-name.nt
FILE41=links-${DS}-4.1-jaro-fullname.nt
FILE42=links-${DS}-4.2-levenstein-fullname.nt
FILE4=links-${DS}-4-jaro-levenstein-fullname.nt
FILE51=links-${DS}-5.1-jaro-name.nt
FILE52=links-${DS}-5.2-levenstein-name.nt
FILE5=links-${DS}-5-jaro-levenstein-name.nt

# Make the intersection of two files, store the result in an output file.
# arg1: first file to intersect
# arg2: second file to intersect
# arg3: output file wher eto write the result
function intersect() {
    local file1=$1
    local file2=$2
    local output=$3

    if [[ ! -e "$file1" ]]; then echo "Warning (fctn intersect): file $file1 does not exist. Ignoring."
    elif [[ ! -e "$file2" ]]; then echo "Warning (fctn intersect): file $file2 does not exist. Ignoring."
    else
        echo "Intersecting $file1 with $file2 into $output..."
        echo -n "" > $output
        cat $file1 | while read line; do
            # If that line is also in file2, then keep it
            if grep --silent "$line" $file2; then
              echo $line >> $output
            fi
        done
    fi
}

# Look, in file $1, for triples whose subjects are not yet mentioned in output file $2,
# and append the corresponding triple to the output file.
# arg1: existing set of triples
# arg2: file with new triples to append to $1 if their subject is not is $1 yet
function appendNewUri() {
    local output=$1
    local toMerge=$2
    local tmpOutput=$2.kept

    if [[ ! -e "$output" ]]; then echo "Warning (fctn appendNewUri): file $output does not exist. Ignoring."
    elif [[ ! -e "$toMerge" ]]; then echo "Warning (fctn appendNewUri): file $toMerge does not exist. Ignoring."
    else
        echo -n "Appending triples of $toMerge to $output: "

        echo -n "" > $tmpOutput
        cat $toMerge | while read line; do
            # Get the taxon URI = first term on the line
            taxon=$(echo "$line" | cut -f1 -d" ")

            # If this taxon is not yet in the output, keep it, otherwise ignore it
            if ! grep --silent "$taxon" $output; then
              echo $line >> $tmpOutput
            fi
        done
        echo "keeping $(wc -l $tmpOutput | cut -f1 -d" ") triples out of $(wc -l $toMerge | cut -f1 -d" ")."
        cat $tmpOutput >> $output
        rm -f $tmpOutput
    fi
}

echo "------------------------------------------------------------------"
echo "$(date)"
echo "Starting merging links for data source $DS..."

# Create files 4 and 5 by intersecting 4.1 and 4.2, and 5.1 and 5.2
if [[ -e "$FILE41" && -e "$FILE42" ]]; then
    intersect $FILE41 $FILE42 $FILE4
fi
if [[ -e "$FILE51" && -e "$FILE52" ]]; then
    intersect $FILE51 $FILE52 $FILE5
fi

echo -n "" > $OUTPUT

# Merge files
if [[ -e "$FILE1" ]]; then
    appendNewUri $OUTPUT $FILE1
fi
if [[ -e "$FILE2" ]]; then
    appendNewUri $OUTPUT $FILE2
fi
if [[ -e "$FILE3" ]]; then
    appendNewUri $OUTPUT $FILE3
fi
if [[ -e "$FILE4" ]]; then
    appendNewUri $OUTPUT $FILE4
fi
if [[ -e "$FILE5" ]]; then
    appendNewUri $OUTPUT $FILE5
fi

