ifdef DEBUG
	OPT = -O0 -DDEBUG=1 --debug -g -ggdb
else
	OPT = -O3
endif

CFLAGS = -Wall -Wextra -I. $(OPT)
LDFLAGS = -L BitArray -l bitarr

all: task

task: task.c
	$(CC) $< -o $@ $(CFLAGS) $(LDFLAGS)

clean:
	rm -rf *.o task

# Comment this line out to keep .o files
.INTERMEDIATE: $(OBJS)
.PHONY: all clean
