#!/bin/bash
# This script runs the SILK framework on a set of SILK configurations that align
# TAXREF taxa with peer data sets and ontologies.
#
# Input argument:
# - arg1: name of the peer dataset, e.g. agrovoc ($DS)
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


SILKJAR=/home/taxref/silk/lib/silk-2.7.1.jar

echo "------------------------------------------------------------------"
echo "$(date)"
echo "Starting generation of links for data source $DS..."

CFG=silk-${DS}-1-equality-fullname.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

CFG=silk-${DS}-2-equality-fullname-nodate.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

CFG=silk-${DS}-3-equality-name.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

CFG=silk-${DS}-4.1-jaro-fullname.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

CFG=silk-${DS}-4.2-levenstein-fullname.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

CFG=silk-${DS}-5.1-jaro-name.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

CFG=silk-${DS}-5.2-levenstein-name.xml
if [[ -e "$CFG" ]]; then
    echo "Processing file $CFG"
    java -Xmx8g -DconfigFile=$CFG -jar $SILKJAR
fi

