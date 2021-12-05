#!/usr/bin/env bash
for i in $(ls examples/*.pluto)
do
	./result/bin/pluto assemble $i >/dev/null
	if [ $? -ne 0 ]; then
		echo "failed to assemble example: " $i
		exit 1
	fi
done
