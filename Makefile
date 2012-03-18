all: comp test

comp: comp.cc o.h
	g++ -O2 -Wall -W -pipe -g comp.cc -o comp

test:comp
	#./comp data/f26fbc235167ab7630e19634d254fa26.dat
	./comp data/bcd67c198d66ce35e8ecc4a9a8543280.dat data/bcc23e0a25c6c910034f9e05021b1b1a.dat data/f26fbc235167ab7630e19634d254fa26.dat data/1558dee2ecfb7a0f9f63e27376675b6c.dat

ex:comp
	./comp data/5716a414b41586e046887486ceb0f13d.dat data/2bf3c74ae472ef9b268d6bb89e23db33.dat data/32ea14604c00b6ee878442be5e0d4915.dat data/52909913e796610ffbb8bad2b252c4cd.dat data/62b9d5cff05423698cca681220205134.dat

profile:comp
	sudo opcontrol --reset
	sudo opcontrol --event=CPU_CLK_UNHALTED:50000
	sudo opcontrol --start
	./comp data/C22-169_1600_1000_100.tab
	sudo opcontrol --stop
	sudo opcontrol -h

bench:comp 
	time ./comp data/*.tab

    # data/B28-39_2800_2800_acq_0400.tab data/*.tab
