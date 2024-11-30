set -e

if [[ $# -ne 1 ]]
then
    echo "Usage: $0 <day number>"
    exit 1
fi

MYDIR=$(dirname $0)

N=$1
NN=$(printf "%02d" $1)
DAY=day$NN

mkdir "${DAY}"

cp ${MYDIR}/dayNN/CMakeLists.txt "${DAY}/CMakeLists.txt"
sed -e "s/__NN__/$NN/g" ${MYDIR}/dayNN/dayNN.cpp > ${DAY}/${DAY}.cpp
sed -e "s/__NN__/$NN/g" ${MYDIR}/dayNN/dayNN.h > ${DAY}/${DAY}.h
sed -e "s/__NN__/$NN/g" -e "s/__N__/$N/g" ${MYDIR}/dayNN/dayNN_main.cpp > ${DAY}/${DAY}_main.cpp
sed -e "s/__NN__/$NN/g" ${MYDIR}/dayNN/dayNN_test.cpp > ${DAY}/${DAY}_test.cpp
