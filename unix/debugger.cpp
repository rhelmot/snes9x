#include <readline/readline.h>
#include <readline/history.h>
#include <iostream>
#include <stdlib.h>
#include "../debug.h"
#include "../snes9x.h"
#include "display.h"

bool is_start = true;

void debug_setup() {
	char *filename = getenv("S9X_HISTFILE");
	if (!filename) {
		filename = (char*)malloc(512);
		snprintf(filename, 512, "%s/.s9xhist", getenv("HOME"));
		read_history(filename);
		free(filename);
	} else {
		read_history(filename);
	}
}

void debug_teardown() {
	char *filename = getenv("S9X_HISTFILE");
	if (!filename) {
		filename = (char*)malloc(512);
		snprintf(filename, 512, "%s/.s9xhist", getenv("HOME"));
		write_history(filename);
		free(filename);
	} else {
		write_history(filename);
	}
}

void S9xDebugInteract (void)
{
	static char	*line_persist = NULL;
	char		*line = NULL;
	int32		cycles;
	char		*p;
	bool		is_redo = true;

	if (is_start) {
		// Do some things which shouldn't be done in the signal handler
		S9xDebugPrintStatus(std::cout);
		S9xTextMode();
		is_start = false;
	}

	free(line);
	line = readline("(snes9x) ");
	if (line == NULL || line[0] == 'q') {
		free(line);
		line = readline("\nAre you sure you want to quit? [y/n] ");
		if (line == NULL || line[0] == '\0' || line[0] == 'Y' || line[0] == 'y') {
			if (line == NULL) printf("\n");
			S9xExit();
		}
		return;
	} else if (line[0] != '\0') {
		add_history(line);
		free(line_persist);
		line_persist = strdup(line);
		is_redo = false;
	}

	cycles = CPU.Cycles;
	if (line_persist) S9xDebugCommand(line_persist, std::cout, is_redo);
	CPU.Cycles = cycles;
}

void S9xStartDebug(void)
{
	Debug.Dump.Bank = 0;
	Debug.Dump.Address = 0;
	Debug.Unassemble.Bank = 0;
	Debug.Unassemble.Address = 0;
	S9xBreakpoint[5].Enabled = FALSE;

	CPU.Flags |= DEBUG_MODE_FLAG;
	CPU.Flags &= ~(FRAME_ADVANCE_FLAG | SINGLE_STEP_FLAG);
	is_start = true;
}

void S9xStopDebug(void)
{
	CPU.Flags &= ~DEBUG_MODE_FLAG;
	//S9xGraphicsMode();
}
