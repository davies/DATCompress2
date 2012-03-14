all: comp test

comp: comp.cc o.h
	g++ -Wall -W -O2 -pipe -g comp.cc -o comp

test:comp
	./comp data/1558dee2ecfb7a0f9f63e27376675b6c.dat

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
