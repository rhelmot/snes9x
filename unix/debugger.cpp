#ifdef DEBUGGER

#include <readline/readline.h>
#include <readline/history.h>
#include <iostream>
#include <stdlib.h>
#include <dirent.h>
#include "../debug.h"
#include "../snes9x.h"
#include "../display.h"

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

	if (Settings.TraceOnStart) {
		printf("Instruction tracing to trace.log enabled\n");
		S9xTrace();
	}

	if (Settings.DebugOnStart) {
		S9xStartDebug();
		printf("\nWelcome to the Snes9x Debugger!\n"
				"Press '?' for help, or 'c' to begin execution.\n"
				"Return to this prompt at any time with ctrl-c.\n\n");
	} else {
		printf("\nWelcome to the Snes9x Debugger!\n"
				"Press ctrl-c to activate the debug prompt.\n"
				"Then, press '?' for help or 'c' to resume execution.\n\n");
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
	bool		is_redo = true;

	if (is_start) {
		// Do some things which shouldn't be done in the signal handler
		S9xDebugPrintStatus(std::cout);
		if (Debug.break_reason == 1) {
			printf("Hit breakpoint %d: $%06X\n",
					Debug.break_number,
					S9xBreakpoint[Debug.break_number].Address);
		} else if (Debug.break_reason == 2) {
			printf("Hit read watchpoint %d: $%06X (%02X)\n",
					Debug.break_number,
					S9xWatchpoint[Debug.break_number].Address,
					*S9xWatchpoint[Debug.break_number].RealAddress);
		} else if (Debug.break_reason == 3) {
			printf("Hit write watchpoint %d: $%06X (%02X -> %02X)\n",
					Debug.break_number,
					S9xWatchpoint[Debug.break_number].Address,
					Debug.break_initval,
					*S9xWatchpoint[Debug.break_number].RealAddress);
		}
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

void S9xStartDebug(int kind, int which)
{
	if (CPU.Flags & DEBUG_MODE_FLAG) return;

	Debug.Dump.Bank = 0;
	Debug.Dump.Address = 0;
	Debug.Unassemble.Bank = 0;
	Debug.Unassemble.Address = 0;
	Debug.break_reason = kind;
	Debug.break_number = which;

	if (kind == 3) Debug.break_initval = *S9xWatchpoint[which].RealAddress;

	CPU.Flags |= DEBUG_MODE_FLAG;
	CPU.Flags &= ~(FRAME_ADVANCE_FLAG | SINGLE_STEP_FLAG | STEP_OUT_FLAG);
	is_start = true;
}

void S9xStopDebug(void)
{
	CPU.Flags &= ~DEBUG_MODE_FLAG;
	//S9xGraphicsMode();
}

void S9xLoadDebugSymbols(char *base_dir) {
	DIR *dir;
	struct dirent *entry;
	char full_filename[PATH_MAX];
	if (!(dir = opendir(base_dir))) {
		return;
	}

	while (entry = readdir(dir)) {
		char *filename = entry->d_name;
		int namelen = strlen(filename);
		if (namelen >= 7 && !strcmp(&filename[namelen-7], ".labels")) {
			snprintf(full_filename, PATH_MAX, "%s/%s", base_dir, filename);
			S9xDebugLoadLabels(full_filename);
		} else if (namelen >= 6 && !strcmp(&filename[namelen-6], ".lines")) {
			snprintf(full_filename, PATH_MAX, "%s/%s", base_dir, filename);
			S9xDebugLoadLines(full_filename);
		}
	}

	closedir(dir);
}

#endif
