set -e

if [[ $# -ne 1 ]]
then
    echo "Usage: $0 <day number>"
    exit 1
fi

X=0$1
N=$1
NN=${X: -2}
DAY=day$NN

mkdir "${DAY}"

cp dayNN/CMakeLists.txt "${DAY}/CMakeList.txt"
sed -e "s/__NN__/$NN/g" dayNN/dayNN.cpp > ${DAY}/${DAY}.cpp
sed -e "s/__NN__/$NN/g" dayNN/dayNN.h > ${DAY}/${DAY}.h
sed -e "s/__NN__/$NN/g" -e "s/__N__/$N/g" dayNN/dayNN_main.cpp > ${DAY}/${DAY}_main.cpp
sed -e "s/__NN__/$NN/g" dayNN/dayNN_test.cpp > ${DAY}/${DAY}_test.cpp
