set -e

if [[ $# -ne 1 && $# -ne 2 ]]
then
    echo "Usage: $0 <day number> [year]"
    exit 1
fi

MYDIR=$(dirname $0)

N=$1
NN=$(printf "%02d" $1)
DAY=day$NN

YEAR=2024
if [[ $# -eq 2 ]]
then
    YEAR=$2
fi

mkdir "${DAY}"

cp ${MYDIR}/dayNN/CMakeLists.txt "${DAY}/CMakeLists.txt"
sed -e "s/__NN__/$NN/g" ${MYDIR}/dayNN/dayNN.cpp > ${DAY}/${DAY}.cpp
sed -e "s/__NN__/$NN/g" ${MYDIR}/dayNN/dayNN.h > ${DAY}/${DAY}.h
sed -e "s/__NN__/$NN/g" -e "s/__N__/$N/g" -e "s/__YEAR__/$YEAR/g" ${MYDIR}/dayNN/dayNN_main.cpp > ${DAY}/${DAY}_main.cpp
sed -e "s/__NN__/$NN/g" ${MYDIR}/dayNN/dayNN_test.cpp > ${DAY}/${DAY}_test.cpp
