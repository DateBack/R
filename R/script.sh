#!/bin/bash
i=1
echo 'Hellow'
for file in R/SuperShop/БилюкинСергей/*
do
if [ -d "$file" ]
then
echo "$file is a directory"

sudo cp -r $file/ R/SuperShop/Analysis

mv R/SuperShop/Analysis/in.txt R/SuperShop/Analysis/"$i.store_in.txt"
mv R/SuperShop/Analysis/out.txt R/SuperShop/Analysis/"$i.store_out.txt"

i=$((i+1))

elif [ -f "$file" ]
then
echo "$file is a fileeee"
fi
done