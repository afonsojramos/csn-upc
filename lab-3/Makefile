.PHONY = all clean

CPP=g++

SRCS := $(wildcard *.cc)
BINS := $(SRCS:%.cc=%)

all: ${BINS}

%: %.cc
	${CPP} $< -fopenmp -o $@

clean:
	rm -rvf ${BINS}