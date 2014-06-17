#!/bin/bash

if [ $# -ne 3 ] ; then
 echo "usage: ./gencsv.sh <num_rows> <num_columns> <data_per_column>"
 exit 1
fi

gen_data() {
 str=$1
 num=$2
 v=$(printf "%-${num}s" "$str")
 echo "${v// /*}"
}

gen_columns() {
 num_columns=$1
 data_per_column=$2
 export DATA=`gen_data "*" $data_per_column`
 for j in `seq 1 $num_columns`; do
  printf "\"$DATA\""
  if [ $j -ne $num_columns ] ; then
   printf ","
  fi
 done | xargs echo
}

gen_rows() {
 num_rows=$1
 row=$2
 for i in `seq 1 $num_rows`; do
  echo $row
 done
}

main() {

 num_rows=$1
 num_columns=$2
 data_per_column=$3

 export ROW=`gen_columns $num_columns $data_per_column`

 gen_rows $num_rows "$ROW"
}

main $@
