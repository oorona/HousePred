for i in {1..10}
do
	cp ./split_$i/* .
	time /usr/bin/Rscript mymain.R
	/usr/bin/Rscript evaluation.R
done

