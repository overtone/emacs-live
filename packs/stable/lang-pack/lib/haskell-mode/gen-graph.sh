echo "digraph {"
echo "rankdir=TB;"
for i in $(ls *.el | grep ^haskell)
do

    for x in $(egrep -o "^\\(require '([^)]+)" $i | sed "s/.require '//" | grep ^haskell)
    do
        z=$(echo $i | sed 's/.el$//')
        echo "\"$z\" -> \"$x\"; "
    done
done
echo "}"
