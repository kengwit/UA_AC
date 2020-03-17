# /bin/sh

# 60 counters at 5s so do 6 iterations for a 30 minute run.
PATH=$PATH:/home/perf

# Change i for the number of 5 minute iterations you want to do
set -A MMCR "1 2 3 6 8 9 12 30 62"

#set -A MMCR "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 51 52 53 54 55 56 57 58 59 60 61"

c1=" -1 1 -2 6 -3 2 -4 3 -5 10 -6 12 -7 14 -8 4 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c2=" -1 86 -2 12 -3 10 -4 13 -5 12 -6 10 -7 3 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c3=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c4=" -1 1 -2 6 -3 2 -4 3 -5 10 -6 12 -7 14 -8 4 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c5=" -1 8 -2 8 -3 8 -4 8 -5 4 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c6=" -1 3 -2 4 -3 7 -4 7 -5 7 -6 7 -7 7 -8 7 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c7=" -1 10 -2 10 -3 13 -4 13 -5 8 -6 8 -7 8 -8 8 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c8=" -1 15 -2 15 -3 15 -4 15 -5 15 -6 15 -7 15 -8 15 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c9=" -1 8 -2 8 -3 8 -4 8 -5 4 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c10=" -1 3 -2 4 -3 7 -4 7 -5 7 -6 7 -7 7 -8 7 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c11=" -1 10 -2 10 -3 13 -4 13 -5 8 -6 8 -7 8 -8 8 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c12=" -1 15 -2 15 -3 15 -4 15 -5 15 -6 15 -7 15 -8 15 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c13=" -1 15 -2 16 -3 16 -4 16 -5 15 -6 15 -7 4 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c14=" -1 64 -2 0 -3 0 -4 0 -5 0 -6 4 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c15=" -1 65 -2 0 -3 0 -4 0 -5 15 -6 4 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c16=" -1 65 -2 0 -3 0 -4 0 -5 6 -6 4 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c17=" -1 80 -2 0 -3 0 -4 0 -5 12 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c18=" -1 81 -2 0 -3 0 -4 4 -5 0 -6 0 -7 0 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c19=" -1 86 -2 12 -3 10 -4 13 -5 12 -6 10 -7 3 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c20=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c21=" -1 82 -2 0 -3 0 -4 0 -5 4 -6 2 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c22=" -1 84 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c23=" -1 85 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c24=" -1 85 -2 0 -3 0 -4 0 -5 14 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c25=" -1 96 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c26=" -1 97 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c27=" -1 98 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c28=" -1 86 -2 12 -3 10 -4 13 -5 12 -6 10 -7 3 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c29=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c30=" -1 87 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c31=" -1 88 -2 0 -3 0 -4 0 -5 0 -6 0 -7 4 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c32=" -1 100 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c33=" -1 101 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c34=" -1 102 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c35=" -1 103 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c36=" -1 105 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c37=" -1 106 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c38=" -1 86 -2 12 -3 10 -4 13 -5 12 -6 10 -7 3 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c39=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c40=" -1 107 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 4 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c41=" -1 108 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 4 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c42=" -1 109 -2 0 -3 0 -4 0 -5 4 -6 6 -7 2 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c43=" -1 110 -2 0 -3 0 -4 0 -5 0 -6 0 -7 4 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c44=" -1 111 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c45=" -1 112 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c46=" -1 113 -2 0 -3 0 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c47=" -1 28 -2 21 -3 9 -4 4 -5 21 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c48=" -1 22 -2 22 -3 22 -4 22 -5 4 -6 10 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
C49=" -1 86 -2 12 -3 10 -4 13 -5 12 -6 10 -7 3 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c50=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c51=" -1 24 -2 24 -3 24 -4 24 -5 4 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c52=" -1 25 -2 25 -3 25 -4 25 -5 4 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c53=" -1 26 -2 26 -3 26 -4 26 -5 4 -6 2 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c54=" -1 27 -2 27 -3 27 -4 27 -5 4 -6 11 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c55=" -1 30 -2 30 -3 30 -4 4 -5 17 -6 3 -7 11 -8 11 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c56=" -1 31 -2 31 -3 31 -4 31 -5 4 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c57=" -1 85 -2 0 -3 0 -4 0 -5 3 -6 0 -7 0 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c58=" -1 86 -2 12 -3 10 -4 13 -5 12 -6 10 -7 3 -8 1 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c59=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c60=" -1 86 -2 0 -3 0 -4 0 -5 0 -6 4 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c61=" -1 23 -2 23 -3 23 -4 23 -5 4 -6 1 -7 6 -8 3 -mmcra 00000000 -imru 10000000 -imrl ffffffff"
c62=" -1 112 -2 3 -3 6 -4 0 -5 0 -6 0 -7 0 -8 0 -mmcra 00000000 -imru 10000000 -imrl ffffffff"

bindprocessor $$ 0
rm -f cpu.out
touch cpu.out
command="$*"

for j in ${MMCR[*]}
do
  echo "Counter Set $j"
  eval y='$c'$j

  rdpm -s -v -p 0 $y 1 >> cpu.out & 
  rdpmpid=$!
  $command
  kill $rdpmpid
  wait $rdpmpid
done

mkdir -p counter_data
num=`ls data/*.[0-9]* | sed 's#.*/[^/]*\.##' | sort -n | tail -1`
num=$(( $num + 1 ))
mv cpu.out counter_data/cpu.out.$num

exit 0

