for i in {1..10}
do
	cp ./split_$i/* .
	/usr/bin/Rscript testlinear.R
done

