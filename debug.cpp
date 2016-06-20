/***********************************************************************************
  Snes9x - Portable Super Nintendo Entertainment System (TM) emulator.

  (c) Copyright 1996 - 2002  Gary Henderson (gary.henderson@ntlworld.com),
                             Jerremy Koot (jkoot@snes9x.com)

  (c) Copyright 2002 - 2004  Matthew Kendora

  (c) Copyright 2002 - 2005  Peter Bortas (peter@bortas.org)

  (c) Copyright 2004 - 2005  Joel Yliluoma (http://iki.fi/bisqwit/)

  (c) Copyright 2001 - 2006  John Weidman (jweidman@slip.net)

  (c) Copyright 2002 - 2006  funkyass (funkyass@spam.shaw.ca),
                             Kris Bleakley (codeviolation@hotmail.com)

  (c) Copyright 2002 - 2010  Brad Jorsch (anomie@users.sourceforge.net),
                             Nach (n-a-c-h@users.sourceforge.net),

  (c) Copyright 2002 - 2011  zones (kasumitokoduck@yahoo.com)

  (c) Copyright 2006 - 2007  nitsuja

  (c) Copyright 2009 - 2011  BearOso,
                             OV2


  BS-X C emulator code
  (c) Copyright 2005 - 2006  Dreamer Nom,
                             zones

  C4 x86 assembler and some C emulation code
  (c) Copyright 2000 - 2003  _Demo_ (_demo_@zsnes.com),
                             Nach,
                             zsKnight (zsknight@zsnes.com)

  C4 C++ code
  (c) Copyright 2003 - 2006  Brad Jorsch,
                             Nach

  DSP-1 emulator code
  (c) Copyright 1998 - 2006  _Demo_,
                             Andreas Naive (andreasnaive@gmail.com),
                             Gary Henderson,
                             Ivar (ivar@snes9x.com),
                             John Weidman,
                             Kris Bleakley,
                             Matthew Kendora,
                             Nach,
                             neviksti (neviksti@hotmail.com)

  DSP-2 emulator code
  (c) Copyright 2003         John Weidman,
                             Kris Bleakley,
                             Lord Nightmare (lord_nightmare@users.sourceforge.net),
                             Matthew Kendora,
                             neviksti

  DSP-3 emulator code
  (c) Copyright 2003 - 2006  John Weidman,
                             Kris Bleakley,
                             Lancer,
                             z80 gaiden

  DSP-4 emulator code
  (c) Copyright 2004 - 2006  Dreamer Nom,
                             John Weidman,
                             Kris Bleakley,
                             Nach,
                             z80 gaiden

  OBC1 emulator code
  (c) Copyright 2001 - 2004  zsKnight,
                             pagefault (pagefault@zsnes.com),
                             Kris Bleakley
                             Ported from x86 assembler to C by sanmaiwashi

  SPC7110 and RTC C++ emulator code used in 1.39-1.51
  (c) Copyright 2002         Matthew Kendora with research by
                             zsKnight,
                             John Weidman,
                             Dark Force

  SPC7110 and RTC C++ emulator code used in 1.52+
  (c) Copyright 2009         byuu,
                             neviksti

  S-DD1 C emulator code
  (c) Copyright 2003         Brad Jorsch with research by
                             Andreas Naive,
                             John Weidman

  S-RTC C emulator code
  (c) Copyright 2001 - 2006  byuu,
                             John Weidman

  ST010 C++ emulator code
  (c) Copyright 2003         Feather,
                             John Weidman,
                             Kris Bleakley,
                             Matthew Kendora

  Super FX x86 assembler emulator code
  (c) Copyright 1998 - 2003  _Demo_,
                             pagefault,
                             zsKnight

  Super FX C emulator code
  (c) Copyright 1997 - 1999  Ivar,
                             Gary Henderson,
                             John Weidman

  Sound emulator code used in 1.5-1.51
  (c) Copyright 1998 - 2003  Brad Martin
  (c) Copyright 1998 - 2006  Charles Bilyue'

  Sound emulator code used in 1.52+
  (c) Copyright 2004 - 2007  Shay Green (gblargg@gmail.com)

  SH assembler code partly based on x86 assembler code
  (c) Copyright 2002 - 2004  Marcus Comstedt (marcus@mc.pp.se)

  2xSaI filter
  (c) Copyright 1999 - 2001  Derek Liauw Kie Fa

  HQ2x, HQ3x, HQ4x filters
  (c) Copyright 2003         Maxim Stepin (maxim@hiend3d.com)

  NTSC filter
  (c) Copyright 2006 - 2007  Shay Green

  GTK+ GUI code
  (c) Copyright 2004 - 2011  BearOso

  Win32 GUI code
  (c) Copyright 2003 - 2006  blip,
                             funkyass,
                             Matthew Kendora,
                             Nach,
                             nitsuja
  (c) Copyright 2009 - 2011  OV2

  Mac OS GUI code
  (c) Copyright 1998 - 2001  John Stiles
  (c) Copyright 2001 - 2011  zones


  Specific ports contains the works of other authors. See headers in
  individual files.


  Snes9x homepage: http://www.snes9x.com/

  Permission to use, copy, modify and/or distribute Snes9x in both binary
  and source form, for non-commercial purposes, is hereby granted without
  fee, providing that this license information and copyright notice appear
  with all copies and any derived work.

  This software is provided 'as-is', without any express or implied
  warranty. In no event shall the authors be held liable for any damages
  arising from the use of this software or it's derivatives.

  Snes9x is freeware for PERSONAL USE only. Commercial users should
  seek permission of the copyright holders first. Commercial use includes,
  but is not limited to, charging money for Snes9x or software derived from
  Snes9x, including Snes9x or derivatives in commercial game bundles, and/or
  using Snes9x as a promotion for your commercial product.

  The copyright holders request that bug fixes and improvements to the code
  should be forwarded to them so everyone can benefit from the modifications
  in future versions.

  Super NES and Super Nintendo Entertainment System are trademarks of
  Nintendo Co., Limited and its subsidiary companies.
 ***********************************************************************************/


#ifdef DEBUGGER

#include <stdarg.h>
#include <iomanip>
#include <iostream>
#include "snes9x.h"
#include "memmap.h"
#include "cpuops.h"
#include "dma.h"
#include "apu/apu.h"
#include "display.h"
#include "debug.h"
#include "missing.h"

using std::endl;

extern SDMA	DMA[8];
extern FILE	*apu_trace;
FILE		*trace = NULL, *trace2 = NULL;

struct SBreakPoint	S9xBreakpoint[6];
struct SWatchPoint	S9xWatchpoint[6];
struct SDebug	Debug = { { 0, 0 },{ 0, 0 } };
int step_depth;

static const char	*HelpMessage[] =
{
	"Command Help:",
	"?, help                   - Shows this command help",
	"status                    - Shows the current status of the registers and current instruction",
	"info [topic]              - Print out information about the given topic, run with no argument for topics",
	"x[/format] $[addr]        - Display memory contents. GDB syntax.",
	"dump [addr] [count]       - Dump memory to file",
	"r, reset                  - Reset the emulated system",
	"trace [what]              - Toggle tracing of some subsystem, run with no arguments for options",
	"b, break $[addr]          - Add a breakpoint at [addr]",
	"u, unbreak [n]            - Remove breakpoing #n",
	"w, watch $[addr] [type]   - Add a watchpoint, breaking on {read, write, both} to/from [addr]",
	"unwatch [n]               - Remove watchpoint #n",
	"s, step                   - Step to the next emulated instruction",
	"n, next                   - Step to the next emulated instruction, stepping over subroutine calls",
	"c, continue               - Continue execution",
	"o, out                    - Step until the current function returns"
	"f, frame [n]              - Step n frames forward. Will break as soon as rendering finishes for the nth frame."

	"z                         - ???? Dumps some sort of VRAM data, I don't know what this does",
	"",
	NULL
};

static const char	*S9xMnemonics[256] =
{
	"BRK", "ORA", "COP", "ORA", "TSB", "ORA", "ASL", "ORA",
	"PHP", "ORA", "ASL", "PHD", "TSB", "ORA", "ASL", "ORA",
	"BPL", "ORA", "ORA", "ORA", "TRB", "ORA", "ASL", "ORA",
	"CLC", "ORA", "INC", "TCS", "TRB", "ORA", "ASL", "ORA",
	"JSR", "AND", "JSL", "AND", "BIT", "AND", "ROL", "AND",
	"PLP", "AND", "ROL", "PLD", "BIT", "AND", "ROL", "AND",
	"BMI", "AND", "AND", "AND", "BIT", "AND", "ROL", "AND",
	"SEC", "AND", "DEC", "TSC", "BIT", "AND", "ROL", "AND",
	"RTI", "EOR", "WDM", "EOR", "MVP", "EOR", "LSR", "EOR",
	"PHA", "EOR", "LSR", "PHK", "JMP", "EOR", "LSR", "EOR",
	"BVC", "EOR", "EOR", "EOR", "MVN", "EOR", "LSR", "EOR",
	"CLI", "EOR", "PHY", "TCD", "JMP", "EOR", "LSR", "EOR",
	"RTS", "ADC", "PER", "ADC", "STZ", "ADC", "ROR", "ADC",
	"PLA", "ADC", "ROR", "RTL", "JMP", "ADC", "ROR", "ADC",
	"BVS", "ADC", "ADC", "ADC", "STZ", "ADC", "ROR", "ADC",
	"SEI", "ADC", "PLY", "TDC", "JMP", "ADC", "ROR", "ADC",
	"BRA", "STA", "BRL", "STA", "STY", "STA", "STX", "STA",
	"DEY", "BIT", "TXA", "PHB", "STY", "STA", "STX", "STA",
	"BCC", "STA", "STA", "STA", "STY", "STA", "STX", "STA",
	"TYA", "STA", "TXS", "TXY", "STZ", "STA", "STZ", "STA",
	"LDY", "LDA", "LDX", "LDA", "LDY", "LDA", "LDX", "LDA",
	"TAY", "LDA", "TAX", "PLB", "LDY", "LDA", "LDX", "LDA",
	"BCS", "LDA", "LDA", "LDA", "LDY", "LDA", "LDX", "LDA",
	"CLV", "LDA", "TSX", "TYX", "LDY", "LDA", "LDX", "LDA",
	"CPY", "CMP", "REP", "CMP", "CPY", "CMP", "DEC", "CMP",
	"INY", "CMP", "DEX", "WAI", "CPY", "CMP", "DEC", "CMP",
	"BNE", "CMP", "CMP", "CMP", "PEI", "CMP", "DEC", "CMP",
	"CLD", "CMP", "PHX", "STP", "JML", "CMP", "DEC", "CMP",
	"CPX", "SBC", "SEP", "SBC", "CPX", "SBC", "INC", "SBC",
	"INX", "SBC", "NOP", "XBA", "CPX", "SBC", "INC", "SBC",
	"BEQ", "SBC", "SBC", "SBC", "PEA", "SBC", "INC", "SBC",
	"SED", "SBC", "PLX", "XCE", "JSR", "SBC", "INC", "SBC"
};

static int	AddrModes[256] =
{
  // 0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
	 3, 10,  3, 19,  6,  6,  6, 12,  0,  1, 24,  0, 14, 14, 14, 17, // 0
	 4, 11,  9, 20,  6,  7,  7, 13,  0, 16, 24,  0, 14, 15, 15, 18, // 1
	14, 10, 17, 19,  6,  6,  6, 12,  0,  1, 24,  0, 14, 14, 14, 17, // 2
	 4, 11,  9, 20,  7,  7,  7, 13,  0, 16, 24,  0, 15, 15, 15, 18, // 3
	 0, 10,  3, 19, 25,  6,  6, 12,  0,  1, 24,  0, 14, 14, 14, 17, // 4
	 4, 11,  9, 20, 25,  7,  7, 13,  0, 16,  0,  0, 17, 15, 15, 18, // 5
	 0, 10,  5, 19,  6,  6,  6, 12,  0,  1, 24,  0, 21, 14, 14, 17, // 6
	 4, 11,  9, 20,  7,  7,  7, 13,  0, 16,  0,  0, 23, 15, 15, 18, // 7
	 4, 10,  5, 19,  6,  6,  6, 12,  0,  1,  0,  0, 14, 14, 14, 17, // 8
	 4, 11,  9, 20,  7,  7,  8, 13,  0, 16,  0,  0, 14, 15, 15, 18, // 9
	 2, 10,  2, 19,  6,  6,  6, 12,  0,  1,  0,  0, 14, 14, 14, 17, // A
	 4, 11,  9, 20,  7,  7,  8, 13,  0, 16,  0,  0, 15, 15, 16, 18, // B
	 2, 10,  3, 19,  6,  6,  6, 12,  0,  1,  0,  0, 14, 14, 14, 17, // C
	 4, 11,  9,  9, 27,  7,  7, 13,  0, 16,  0,  0, 22, 15, 15, 18, // D
	 2, 10,  3, 19,  6,  6,  6, 12,  0,  1,  0,  0, 14, 14, 14, 17, // E
	 4, 11,  9, 20, 26,  7,  7, 13,  0, 16,  0,  0, 23, 15, 15, 18  // F
};

static uint8 S9xDebugGetByte (uint32);
static uint16 S9xDebugGetWord (uint32);
static uint8 S9xDebugSA1GetByte (uint32);
static uint16 S9xDebugSA1GetWord (uint32);
static uint8 debug_cpu_op_print (char *, uint8, uint16);
static uint8 debug_sa1_op_print (char *, uint8, uint16);
static void debug_print_window (std:: ostream &, uint8 *);
static const char * debug_clip_fn (int);


uint8 S9xDebugGetByte (uint32 Address)
{
	int		block = (Address & 0xffffff) >> MEMMAP_SHIFT;
	uint8	*GetAddress = Memory.Map[block];
	uint8	byte = 0;

	if (GetAddress >= (uint8 *) CMemory::MAP_LAST)
	{
		byte = *(GetAddress + (Address & 0xffff));
		return (byte);
	}

	switch ((pint) GetAddress)
	{
		case CMemory::MAP_LOROM_SRAM:
		case CMemory::MAP_SA1RAM:
			byte = *(Memory.SRAM + ((((Address & 0xff0000) >> 1) | (Address & 0x7fff)) & Memory.SRAMMask));
			return (byte);

		case CMemory::MAP_LOROM_SRAM_B:
			byte = *(Multi.sramB + ((((Address & 0xff0000) >> 1) | (Address & 0x7fff)) & Multi.sramMaskB));
			return (byte);

		case CMemory::MAP_HIROM_SRAM:
		case CMemory::MAP_RONLY_SRAM:
			byte = *(Memory.SRAM + (((Address & 0x7fff) - 0x6000 + ((Address & 0xf0000) >> 3)) & Memory.SRAMMask));
			return (byte);

		case CMemory::MAP_BWRAM:
			byte = *(Memory.BWRAM + ((Address & 0x7fff) - 0x6000));
			return (byte);

		default:
			return (byte);
	}
}

uint16 S9xDebugGetWord (uint32 Address)
{
	uint16	word;

	word  = S9xDebugGetByte(Address);
	word |= S9xDebugGetByte(Address + 1) << 8;

	return (word);
}

uint8 S9xDebugSA1GetByte (uint32 Address)
{
	int		block = (Address & 0xffffff) >> MEMMAP_SHIFT;
	uint8	*GetAddress = SA1.Map[block];
	uint8	byte = 0;

	if (GetAddress >= (uint8 *) CMemory::MAP_LAST)
	{
		byte = *(GetAddress + (Address & 0xffff));
		return (byte);
	}

	switch ((pint) GetAddress)
	{
		case CMemory::MAP_LOROM_SRAM:
		case CMemory::MAP_SA1RAM:
			byte = *(Memory.SRAM + (Address & 0xffff));
			return (byte);

		case CMemory::MAP_BWRAM:
			byte = *(SA1.BWRAM + ((Address & 0x7fff) - 0x6000));
			return (byte);

		case CMemory::MAP_BWRAM_BITMAP:
			Address -= 0x600000;
			if (SA1.VirtualBitmapFormat == 2)
				byte = (Memory.SRAM[(Address >> 2) & 0xffff] >> ((Address & 3) << 1)) &  3;
			else
				byte = (Memory.SRAM[(Address >> 1) & 0xffff] >> ((Address & 1) << 2)) & 15;
			return (byte);

		case CMemory::MAP_BWRAM_BITMAP2:
			Address = (Address & 0xffff) - 0x6000;
			if (SA1.VirtualBitmapFormat == 2)
				byte = (SA1.BWRAM[(Address >> 2) & 0xffff] >> ((Address & 3) << 1)) &  3;
			else
				byte = (SA1.BWRAM[(Address >> 1) & 0xffff] >> ((Address & 1) << 2)) & 15;
			return (byte);

		default:
			return (byte);
	}
}

uint16 S9xDebugSA1GetWord (uint32 Address)
{
	uint16	word;

	word  = S9xDebugSA1GetByte(Address);
	word |= S9xDebugSA1GetByte(Address + 1) << 8;

	return (word);
}

static void debug_cpu_status_print (char *Line) {
	debug_cpu_op_print(Line, Registers.PB, Registers.PCw);
	sprintf(Line, "%-44s A:%04X X:%04X Y:%04X D:%04X DB:%02X S:%04X P:%c%c%c%c%c%c%c%c%c HC:%04ld VC:%03ld FC:%02d %03x",
	        Line, Registers.A.W, Registers.X.W, Registers.Y.W,
	        Registers.D.W, Registers.DB, Registers.S.W,
	        CheckEmulation() ? 'E' : 'e',
	        CheckNegative() ? 'N' : 'n',
	        CheckOverflow() ? 'V' : 'v',
	        CheckMemory() ? 'M' : 'm',
	        CheckIndex() ? 'X' : 'x',
	        CheckDecimal() ? 'D' : 'd',
	        CheckIRQ() ? 'I' : 'i',
	        CheckZero() ? 'Z' : 'z',
	        CheckCarry() ? 'C' : 'c',
	        (long) CPU.Cycles,
	        (long) CPU.V_Counter,
	        IPPU.FrameCount,
	        (CPU.IRQExternal ? 0x100 : 0) | (PPU.HTimerEnabled ? 0x10 : 0) | (PPU.VTimerEnabled ? 0x01 : 0));
}

static uint8 debug_cpu_op_print (char *Line, uint8 Bank, uint16 Address)
{
	uint8	S9xOpcode;
	uint8	Operant[3];
	uint16	Word;
	uint8	Byte;
	int16	SWord;
	int8	SByte;
	uint8	Size = 0;

	S9xOpcode = S9xDebugGetByte((Bank << 16) + Address);
	sprintf(Line, "$%02X%04X %02X ", Bank, Address, S9xOpcode);

	Operant[0] = S9xDebugGetByte((Bank << 16) + Address + 1);
	Operant[1] = S9xDebugGetByte((Bank << 16) + Address + 2);
	Operant[2] = S9xDebugGetByte((Bank << 16) + Address + 3);

	switch (AddrModes[S9xOpcode])
	{
		case 0:
			// Implied
			sprintf(Line, "%s         %s",
					Line,
					S9xMnemonics[S9xOpcode]);
			Size = 1;
			break;

		case 1:
			// Immediate[MemoryFlag]
			if (!CheckFlag(MemoryFlag))
			{
				// Accumulator 16 - Bit
				sprintf(Line, "%s%02X %02X    %s #$%02X%02X",
						Line,
						Operant[0],
						Operant[1],
						S9xMnemonics[S9xOpcode],
						Operant[1],
						Operant[0]);
				Size = 3;
			}
			else
			{
				// Accumulator 8 - Bit
				sprintf(Line, "%s%02X       %s #$%02X",
						Line,
						Operant[0],
						S9xMnemonics[S9xOpcode],
						Operant[0]);
				Size = 2;
			}

			break;

		case 2:
			// Immediate[IndexFlag]
			if (!CheckFlag(IndexFlag))
			{
				// X / Y 16 - Bit
				sprintf(Line, "%s%02X %02X    %s #$%02X%02X",
				        Line,
				        Operant[0],
				        Operant[1],
				        S9xMnemonics[S9xOpcode],
				        Operant[1],
				        Operant[0]);
				Size = 3;
			}
			else
			{
				// X / Y 8 - Bit
				sprintf(Line, "%s%02X       %s #$%02X",
				        Line,
				        Operant[0],
				        S9xMnemonics[S9xOpcode],
				        Operant[0]);
				Size = 2;
			}

			break;

		case 3:
			// Immediate[Always 8 - Bit]
			sprintf(Line, "%s%02X       %s #$%02X",
					Line,
					Operant[0],
					S9xMnemonics[S9xOpcode],
					Operant[0]);
			Size = 2;
			break;

		case 4:
			// Relative
			sprintf(Line, "%s%02X       %s $%02X",
					Line,
					Operant[0],
					S9xMnemonics[S9xOpcode],
					Operant[0]);
			SByte = Operant[0];
			Word = Address;
			Word += SByte;
			Word += 2;
			sprintf(Line, "%-32s[$%04X]", Line, Word);
			Size = 2;
			break;

		case 5:
			// Relative Long
			sprintf(Line, "%s%02X %02X    %s $%02X%02X",
					Line,
					Operant[0],
					Operant[1],
					S9xMnemonics[S9xOpcode],
					Operant[1],
					Operant[0]);
			SWord = (Operant[1] << 8) | Operant[0];
			Word = Address;
			Word += SWord;
			Word += 3;
			sprintf(Line, "%-32s[$%04X]", Line, Word);
			Size = 3;
			break;

		case 6:
			// Direct
			sprintf(Line, "%s%02X       %s $%02X",
					Line,
					Operant[0],
					S9xMnemonics[S9xOpcode],
					Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 7:
			// Direct Indexed (with X)
			sprintf(Line, "%s%02X       %s $%02X,x",
					Line,
					Operant[0],
					S9xMnemonics[S9xOpcode],
					Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Word += Registers.X.W;
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 8:
			// Direct Indexed (with Y)
			sprintf(Line, "%s%02X       %s $%02X,y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Word += Registers.Y.W;
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 9:
			// Direct Indirect
			sprintf(Line, "%s%02X       %s ($%02X)",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Word = S9xDebugGetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 2;
			break;

		case 10:
			// Direct Indexed Indirect
			sprintf(Line, "%s%02X       %s ($%02X,x)",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Word += Registers.X.W;
			Word = S9xDebugGetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 2;
			break;

		case 11:
			// Direct Indirect Indexed
			sprintf(Line, "%s%02X       %s ($%02X),y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Word = S9xDebugGetWord(Word);
			Word += Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 2;
			break;

		case 12:
			// Direct Indirect Long
			sprintf(Line, "%s%02X       %s [$%02X]",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Byte = S9xDebugGetByte(Word + 2);
			Word = S9xDebugGetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Byte, Word);
			Size = 2;
			break;

		case 13:
			// Direct Indirect Indexed Long
			sprintf(Line, "%s%02X       %s [$%02X],y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Byte = S9xDebugGetByte(Word + 2);
			Word = S9xDebugGetWord(Word);
			Word += Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Byte, Word);
			Size = 2;
			break;

		case 14:
			// Absolute
			sprintf(Line, "%s%02X %02X    %s $%02X%02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 3;
			break;

		case 15:
			// Absolute Indexed (with X)
			sprintf(Line, "%s%02X %02X    %s $%02X%02X,x",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += Registers.X.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 3;
			break;

		case 16:
			// Absolute Indexed (with Y)
			sprintf(Line, "%s%02X %02X    %s $%02X%02X,y",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 3;
			break;

		case 17:
			// Absolute Long
			sprintf(Line, "%s%02X %02X %02X %s $%02X%02X%02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        Operant[2],
			        S9xMnemonics[S9xOpcode],
			        Operant[2],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			sprintf(Line, "%-32s[$%02X%04X]", Line, Operant[2], Word);
			Size = 4;
			break;

		case 18:
			// Absolute Indexed Long
			sprintf(Line, "%s%02X %02X %02X %s $%02X%02X%02X,x",
			        Line,
			        Operant[0],
			        Operant[1],
			        Operant[2],
			        S9xMnemonics[S9xOpcode],
			        Operant[2],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += Registers.X.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Operant[2], Word);
			Size = 4;
			break;

		case 19:
			// Stack Relative
			sprintf(Line, "%s%02X       %s $%02X,s",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Registers.S.W;
			Word += Operant[0];
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 20:
			// Stack Relative Indirect Indexed
			sprintf(Line, "%s%02X       %s ($%02X,s),y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Registers.S.W;
			Word += Operant[0];
			Word = S9xDebugGetWord(Word);
			Word += Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.DB, Word);
			Size = 2;
			break;

		case 21:
			// Absolute Indirect
			sprintf(Line, "%s%02X %02X    %s ($%02X%02X)",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word = S9xDebugGetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.PB, Word);
			Size = 3;
			break;

		case 22:
			// Absolute Indirect Long
			sprintf(Line, "%s%02X %02X    %s [$%02X%02X]",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Byte = S9xDebugGetByte(Word + 2);
			Word = S9xDebugGetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Byte, Word);
			Size = 3;
			break;

		case 23:
			// Absolute Indexed Indirect
			sprintf(Line, "%s%02X %02X    %s ($%02X%02X,x)",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += Registers.X.W;
			Word = S9xDebugGetWord(ICPU.ShiftedPB + Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Registers.PB, Word);
			Size = 3;
			break;

		case 24:
			// Implied Accumulator
			sprintf(Line, "%s         %s A",
			        Line,
			        S9xMnemonics[S9xOpcode]);
			Size = 1;
			break;

		case 25:
			// MVN/MVP SRC DST
			sprintf(Line, "%s%02X %02X    %s %02X %02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Size = 3;
			break;

		case 26:
			// PEA
			sprintf(Line, "%s%02X %02X    %s $%02X%02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Size = 3;
			break;

		case 27:
			// PEI Direct Indirect
			sprintf(Line, "%s%02X       %s ($%02X)",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += Registers.D.W;
			Word = S9xDebugGetWord(Word);
			sprintf(Line, "%-32s[$%04X]", Line, Word);
			Size = 2;
			break;
	}

	return (Size);
}

static uint8 debug_sa1_op_print (char *Line, uint8 Bank, uint16 Address)
{
	uint8	S9xOpcode;
	uint8	Operant[3];
	uint16	Word;
	uint8	Byte;
	int16	SWord;
	int8	SByte;
	uint8	Size = 0;

	S9xOpcode = S9xDebugSA1GetByte((Bank << 16) + Address);
	sprintf(Line, "$%02X%04X %02X ", Bank, Address, S9xOpcode);

	Operant[0] = S9xDebugSA1GetByte((Bank << 16) + Address + 1);
	Operant[1] = S9xDebugSA1GetByte((Bank << 16) + Address + 2);
	Operant[2] = S9xDebugSA1GetByte((Bank << 16) + Address + 3);

	switch (AddrModes[S9xOpcode])
	{
		case 0:
			// Implied
			sprintf(Line, "%s         %s",
			        Line,
			        S9xMnemonics[S9xOpcode]);
			Size = 1;
			break;

		case 1:
			// Immediate[MemoryFlag]
			if (!SA1CheckFlag(MemoryFlag))
			{
				// Accumulator 16 - Bit
				sprintf(Line, "%s%02X %02X    %s #$%02X%02X",
				        Line,
				        Operant[0],
				        Operant[1],
				        S9xMnemonics[S9xOpcode],
				        Operant[1],
				        Operant[0]);
				Size = 3;
			}
			else
			{
				// Accumulator 8 - Bit
				sprintf(Line, "%s%02X       %s #$%02X",
				        Line,
				        Operant[0],
				        S9xMnemonics[S9xOpcode],
				        Operant[0]);
				Size = 2;
			}

			break;

		case 2:
			// Immediate[IndexFlag]
			if (!SA1CheckFlag(IndexFlag))
			{
				// X / Y 16 - Bit
				sprintf(Line, "%s%02X %02X    %s #$%02X%02X",
				        Line,
				        Operant[0],
				        Operant[1],
				        S9xMnemonics[S9xOpcode],
				        Operant[1],
				        Operant[0]);
				Size = 3;
			}
			else
			{
				// X / Y 8 - Bit
				sprintf(Line, "%s%02X       %s #$%02X",
				        Line,
				        Operant[0],
				        S9xMnemonics[S9xOpcode],
				        Operant[0]);
				Size = 2;
			}

			break;

		case 3:
			// Immediate[Always 8 - Bit]
			sprintf(Line, "%s%02X       %s #$%02X",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Size = 2;
			break;

		case 4:
			// Relative
			sprintf(Line, "%s%02X       %s $%02X",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			SByte = Operant[0];
			Word = Address;
			Word += SByte;
			Word += 2;
			sprintf(Line, "%-32s[$%04X]", Line, Word);
			Size = 2;
			break;

		case 5:
			// Relative Long
			sprintf(Line, "%s%02X %02X    %s $%02X%02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			SWord = (Operant[1] << 8) | Operant[0];
			Word = Address;
			Word += SWord;
			Word += 3;
			sprintf(Line, "%-32s[$%04X]", Line, Word);
			Size = 3;
			break;

		case 6:
			// Direct
			sprintf(Line, "%s%02X       %s $%02X",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 7:
			// Direct Indexed (with X)
			sprintf(Line, "%s%02X       %s $%02X,x",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Word += SA1Registers.X.W;
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 8:
			// Direct Indexed (with Y)
			sprintf(Line, "%s%02X       %s $%02X,y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Word += SA1Registers.Y.W;
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 9:
			// Direct Indirect
			sprintf(Line, "%s%02X       %s ($%02X)",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Word = S9xDebugSA1GetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 2;
			break;

		case 10:
			// Direct Indexed Indirect
			sprintf(Line, "%s%02X       %s ($%02X,x)",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Word += SA1Registers.X.W;
			Word = S9xDebugSA1GetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 2;
			break;

		case 11:
			// Direct Indirect Indexed
			sprintf(Line, "%s%02X       %s ($%02X),y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Word = S9xDebugSA1GetWord(Word);
			Word += SA1Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 2;
			break;

		case 12:
			// Direct Indirect Long
			sprintf(Line, "%s%02X       %s [$%02X]",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Byte = S9xDebugSA1GetByte(Word + 2);
			Word = S9xDebugSA1GetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Byte, Word);
			Size = 2;
			break;

		case 13:
			// Direct Indirect Indexed Long
			sprintf(Line, "%s%02X       %s [$%02X],y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = Operant[0];
			Word += SA1Registers.D.W;
			Byte = S9xDebugSA1GetByte(Word + 2);
			Word = S9xDebugSA1GetWord(Word);
			Word += SA1Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Byte, Word);
			Size = 2;
			break;

		case 14:
			// Absolute
			sprintf(Line, "%s%02X %02X    %s $%02X%02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 3;
			break;

		case 15:
			// Absolute Indexed (with X)
			sprintf(Line, "%s%02X %02X    %s $%02X%02X,x",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += SA1Registers.X.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 3;
			break;

		case 16:
			// Absolute Indexed (with Y)
			sprintf(Line, "%s%02X %02X    %s $%02X%02X,y",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += SA1Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 3;
			break;

		case 17:
			// Absolute Long
			sprintf(Line, "%s%02X %02X %02X %s $%02X%02X%02X",
			        Line,
			        Operant[0],
			        Operant[1],
			        Operant[2],
			        S9xMnemonics[S9xOpcode],
			        Operant[2],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			sprintf(Line, "%-32s[$%02X%04X]", Line, Operant[2], Word);
			Size = 4;
			break;

		case 18:
			// Absolute Indexed Long
			sprintf(Line, "%s%02X %02X %02X %s $%02X%02X%02X,x",
			        Line,
			        Operant[0],
			        Operant[1],
			        Operant[2],
			        S9xMnemonics[S9xOpcode],
			        Operant[2],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += SA1Registers.X.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, Operant[2], Word);
			Size = 4;
			break;

		case 19:
			// Stack Relative
			sprintf(Line, "%s%02X       %s $%02X,s",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = SA1Registers.S.W;
			Word += Operant[0];
			sprintf(Line, "%-32s[$00%04X]", Line, Word);
			Size = 2;
			break;

		case 20:
			// Stack Relative Indirect Indexed
			sprintf(Line, "%s%02X       %s ($%02X,s),y",
			        Line,
			        Operant[0],
			        S9xMnemonics[S9xOpcode],
			        Operant[0]);
			Word = SA1Registers.S.W;
			Word += Operant[0];
			Word = S9xDebugSA1GetWord(Word);
			Word += SA1Registers.Y.W;
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.DB, Word);
			Size = 2;
			break;

		case 21:
			// Absolute Indirect
			sprintf(Line, "%s%02X %02X    %s ($%02X%02X)",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word = S9xDebugSA1GetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.PB, Word);
			Size = 3;
			break;

		case 22:
			// Absolute Indirect Long
			sprintf(Line, "%s%02X %02X    %s [$%02X%02X]",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Byte = S9xDebugSA1GetByte(Word + 2);
			Word = S9xDebugSA1GetWord(Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, Byte, Word);
			Size = 3;
			break;

		case 23:
			// Absolute Indexed Indirect
			sprintf(Line, "%s%02X %02X    %s ($%02X%02X,x)",
			        Line,
			        Operant[0],
			        Operant[1],
			        S9xMnemonics[S9xOpcode],
			        Operant[1],
			        Operant[0]);
			Word = (Operant[1] << 8) | Operant[0];
			Word += SA1Registers.X.W;
			Word = S9xDebugSA1GetWord(SA1.ShiftedPB + Word);
			sprintf(Line, "%-32s[$%02X%04X]", Line, SA1Registers.PB, Word);
			Size = 3;
			break;

		case 24:
			// Implied Accumulator
			sprintf(Line, "%s         %s A",
			        Line,
			        S9xMnemonics[S9xOpcode]);
			Size = 1;
			break;

		case 25:
			// MVN/MVP SRC DST
			sprintf(Line, "%s         %s %02X %02X",
			        Line,
			        S9xMnemonics[S9xOpcode],
			        Operant[0],
			        Operant[1]);
			Size = 3;
			break;
	}

	sprintf(Line, "%-44s A:%04X X:%04X Y:%04X D:%04X DB:%02X S:%04X P:%c%c%c%c%c%c%c%c%c HC:%04ld VC:%03ld FC:%02d",
	        Line, SA1Registers.A.W, SA1Registers.X.W, SA1Registers.Y.W,
	        SA1Registers.D.W, SA1Registers.DB, SA1Registers.S.W,
	        SA1CheckEmulation() ? 'E' : 'e',
	        SA1CheckNegative() ? 'N' : 'n',
	        SA1CheckOverflow() ? 'V' : 'v',
	        SA1CheckMemory() ? 'M' : 'm',
	        SA1CheckIndex() ? 'X' : 'x',
	        SA1CheckDecimal() ? 'D' : 'd',
	        SA1CheckIRQ() ? 'I' : 'i',
	        SA1CheckZero() ? 'Z' : 'z',
	        SA1CheckCarry() ? 'C' : 'c',
	        (long) CPU.Cycles,
	        (long) CPU.V_Counter,
	        IPPU.FrameCount);

	return (Size);
}

static void debug_print_window(std::ostream &out, uint8 *window)
{
	bool first = true;
	for (int i = 0; i < 6; i++)
	{
		if (window[i])
		{
			if (first)
				first = false;
			else
				out << ", ";

			switch (i) {
			case 0:
			case 1:
			case 2:
			case 3:
				out << "Background " << i;
				break;

			case 4:
				out << "Objects";
				break;

			case 5:
				out << "Color window";
				break;
			}
		}
	}
	out << '\n';
}

static const char * debug_clip_fn(int logic)
{
	switch (logic)
	{
	case CLIP_OR:
		return ("OR");

	case CLIP_AND:
		return ("AND");

	case CLIP_XOR:
		return ("XOR");

	case CLIP_XNOR:
		return ("XNOR");

	default:
		return ("???");
	}
}

void S9xDebugPrintWhatsUsed(std::ostream &out) {
	char string[2048];

	sprintf(string, "V-line: %ld, H-Pos: %ld, \n", (long)CPU.V_Counter, (long)CPU.Cycles);
	out << string;

	sprintf(string, "Screen mode: %d%s\n", PPU.BGMode, (PPU.BGMode <= 1 && (Memory.FillRAM[0x2105] & 8) ? " (BG#2 Priority)" : ""));
	out << string;

	sprintf(string, "Brightness: %d%s\n", PPU.Brightness, (Memory.FillRAM[0x2100] & 0x80 ? " (screen blanked)" : ""));
	out << string;

	if (Memory.FillRAM[0x2133] & 1)
		out << "Interlace\n";

	if (Memory.FillRAM[0x2133] & 4)
		out << "240 line visible\n";

	if (Memory.FillRAM[0x2133] & 8)
		out << "Pseudo 512 pixels horizontal resolution\n";

	if (Memory.FillRAM[0x2133] & 0x40)
		out << "Mode 7 priority per pixel\n";

	if (PPU.BGMode == 7) {
		out << "Mode 7 enabled:\n";
		out << "\tScreen repeat: " << ((Memory.FillRAM[0x211a] & 0xc0) >> 6) << '\n';

		if (Memory.FillRAM[0x211a] & 3)
			out << "\tFlipping enabled\n";

		// Sign extend 13 bit values to 16 bit values...
		if (PPU.CentreX & (1 << 12))
			PPU.CentreX |= 0xe000;

		if (PPU.CentreY & (1 << 12))
			PPU.CentreY |= 0xe000;

		sprintf(string, "\tMatrix A: %.3f, B: %.3f, C: %.3f, D: %.3f, Centre X: %d Y:%d\n",
			(double)PPU.MatrixA / 256, (double)PPU.MatrixB / 256,
			(double)PPU.MatrixC / 256, (double)PPU.MatrixD / 256,
			PPU.CentreX, PPU.CentreY);
		out << string;
	}

	if (Memory.FillRAM[0x2130] & 1)
		out << "32K colour mode\n";

	if ((Memory.FillRAM[0x2106] & 0xf0) && (Memory.FillRAM[0x2106] & 0x0f)) {
		out << "Mosaic effect(" << PPU.Mosaic << ") on ";

		bool first = true;
		for (int i = 0; i < 4; i++)
			if (Memory.FillRAM[0x2106] & (1 << i)) {
				if (first) first = false;
				else out << ", ";
				out << "BG" << i;
			}
		out << '\n';
	}

	if (PPU.HVBeamCounterLatched)
		out << "V and H beam pos latched\n";

	if (Memory.FillRAM[0x4200] & 0x20)
		out << "V-IRQ enabled at " << PPU.IRQVBeamPos << '\n';

	if (Memory.FillRAM[0x4200] & 0x10)
		out << "H-IRQ enabled at " << PPU.IRQHBeamPos << '\n';

	if (Memory.FillRAM[0x4200] & 0x80)
		out << "V-blank NMI enabled\n";

	for (int i = 0; i < 8; i++)
	{
		if (missing.hdma_this_frame & (1 << i))
		{
			sprintf(string, "H-DMA %d [%d] 0x%02X%04X->0x21%02X %s %s 0x%02X%04X %s addressing\n",
				i, DMA[i].TransferMode, DMA[i].ABank, DMA[i].AAddress, DMA[i].BAddress,
				DMA[i].AAddressDecrement ? "dec" : "inc",
				DMA[i].Repeat ? "repeat" : "continue",
				DMA[i].IndirectBank, DMA[i].IndirectAddress,
				DMA[i].HDMAIndirectAddressing ? "indirect" : "absolute");
			out << string;
		}
	}

	for (int i = 0; i < 8; i++)
	{
		if (missing.dma_this_frame & (1 << i))
		{
			sprintf(string, "DMA %d [%d] 0x%02X%04X->0x21%02X Num: %d %s\n",
				i, DMA[i].TransferMode, DMA[i].ABank, DMA[i].AAddress, DMA[i].BAddress, DMA[i].TransferBytes,
				DMA[i].AAddressFixed ? "fixed" : (DMA[i].AAddressDecrement ? "dec" : "inc"));
			out << string;
		}
	}

	sprintf(string, "VRAM write address: 0x%04x(%s), Full Graphic: %d, Address inc: %d\n",
		PPU.VMA.Address,
		PPU.VMA.High ? "Byte" : "Word",
		PPU.VMA.FullGraphicCount, PPU.VMA.Increment);
	out << string;

	for (int i = 0; i < 4; i++)
	{
		sprintf(string, "BG%d: VOffset:%d, HOffset:%d, W:%d, H:%d, TS:%d, BA:0x%04x, TA:0x%04X\n",
			i, PPU.BG[i].VOffset, PPU.BG[i].HOffset,
			(PPU.BG[i].SCSize & 1) * 32 + 32,
			(PPU.BG[i].SCSize & 2) * 16 + 32,
			PPU.BG[i].BGSize * 8 + 8,
			PPU.BG[i].SCBase,
			PPU.BG[i].NameBase);
		out << string;
	}

	const char	*s = "";

	switch ((Memory.FillRAM[0x2130] & 0xc0) >> 6)
	{
	case 0:
		s = "always on";
		break;

	case 1:
		s = "inside";
		break;

	case 2:
		s = "outside";
		break;

	case 3:
		s = "always off";
		break;
	}

	out << "Main screen (" << s << "):";
	for (int i = 0; i < 5; i++)
	{
		if (Memory.FillRAM[0x212c] & (1 << i))
		{
			switch (i)
			{
			case 0:
			case 1:
			case 2:
			case 3:
				out << " BG" << i;
				break;

			case 4:
				out << " OBJ";
				break;
			}
		}
	}
	out << '\n';

	switch ((Memory.FillRAM[0x2130] & 0x30) >> 4)
	{
	case 0:
		s = "always on";
		break;

	case 1:
		s = "inside";
		break;

	case 2:
		s = "outside";
		break;

	case 3:
		s = "always off";
		break;
	}

	out << "Subscreen (" << s << "):";
	for (int i = 0; i < 5; i++)
	{
		if (Memory.FillRAM[0x212d] & (1 << i))
		{
			switch (i)
			{
			case 0:
			case 1:
			case 2:
			case 3:
				out << " BG" << i;
				break;

			case 4:
				out << " OBJ";
				break;
			}
		}
	}
	out << '\n';

	if ((Memory.FillRAM[0x2131] & 0x3f))
	{
		if (Memory.FillRAM[0x2131] & 0x80)
		{
			if (Memory.FillRAM[0x2130] & 0x02)
				out << "Subscreen subtract";
			else
				out << "Fixed colour subtract";
		}
		else
		{
			if (Memory.FillRAM[0x2130] & 0x02)
				out << "Subscreen addition";
			else
				out << "Fixed colour addition";
		}

		if (Memory.FillRAM[0x2131] & 0x40)
			out << " (half):";
		else
			out << ":";

		for (int i = 0; i < 6; i++)
		{
			if (Memory.FillRAM[0x2131] & (1 << i))
			{
				switch (i)
				{
				case 0:
				case 1:
				case 2:
				case 3:
					out << " BG" << i;
					break;

				case 4:
					out << " OBJ";
					break;

				case 5:
					out << " BACK";
					break;
				}
			}
		}
		out << '\n';
	}

	sprintf(string, "Window 1 (%d, %d, %02x, %02x): ", PPU.Window1Left, PPU.Window1Right, Memory.FillRAM[0x212e], Memory.FillRAM[0x212f]);
	out << string;

	for (int i = 0; i < 6; i++)
	{
		if (PPU.ClipWindow1Enable[i])
		{
			switch (i)
			{
			case 0:
				sprintf(string, "BG0(%s-%s), ", PPU.ClipWindow1Inside[0] ? "I" : "O", debug_clip_fn(PPU.ClipWindowOverlapLogic[0]));
				break;

			case 1:
				sprintf(string, "BG1(%s-%s), ", PPU.ClipWindow1Inside[1] ? "I" : "O", debug_clip_fn(PPU.ClipWindowOverlapLogic[1]));
				break;

			case 2:
				sprintf(string, "BG2(%s-%s), ", PPU.ClipWindow1Inside[2] ? "I" : "O", debug_clip_fn(PPU.ClipWindowOverlapLogic[2]));
				break;

			case 3:
				sprintf(string, "BG3(%s-%s), ", PPU.ClipWindow1Inside[3] ? "I" : "O", debug_clip_fn(PPU.ClipWindowOverlapLogic[3]));
				break;

			case 4:
				sprintf(string, "OBJ(%s-%s), ", PPU.ClipWindow1Inside[4] ? "I" : "O", debug_clip_fn(PPU.ClipWindowOverlapLogic[4]));
				break;

			case 5:
				sprintf(string, "COL(%s-%s), ", PPU.ClipWindow1Inside[5] ? "I" : "O", debug_clip_fn(PPU.ClipWindowOverlapLogic[5]));
				break;
			}
			out << string;
		}
	}

	sprintf(string, "\n");
	out << string;

	sprintf(string, "Window 2 (%d, %d): ", PPU.Window2Left, PPU.Window2Right);
	out << string;

	for (int i = 0; i < 6; i++)
	{
		if (PPU.ClipWindow2Enable[i])
		{
			switch (i)
			{
			case 0:
				sprintf(string, "BG0(%s), ", PPU.ClipWindow2Inside[0] ? "I" : "O");
				break;

			case 1:
				sprintf(string, "BG1(%s), ", PPU.ClipWindow2Inside[1] ? "I" : "O");
				break;

			case 2:
				sprintf(string, "BG2(%s), ", PPU.ClipWindow2Inside[2] ? "I" : "O");
				break;

			case 3:
				sprintf(string, "BG3(%s), ", PPU.ClipWindow2Inside[3] ? "I" : "O");
				break;

			case 4:
				sprintf(string, "OBJ(%s), ", PPU.ClipWindow2Inside[4] ? "I" : "O");
				break;

			case 5:
				sprintf(string, "COL(%s), ", PPU.ClipWindow2Inside[5] ? "I" : "O");
				break;
			}
			out << string;
		}
	}

	sprintf(string, "\n");
	out << string;

	sprintf(string, "Fixed colour: %02x%02x%02x, \n", PPU.FixedColourRed, PPU.FixedColourGreen, PPU.FixedColourBlue);
	out << string;
}

void S9xDebugPrintWhatsMissing(std::ostream &out)
{
	char string[2048];
	sprintf(string, "Processor: ");
	out << string;

	if (missing.emulate6502) {
		sprintf(string, "emulation mode, ");
		out << string;
	}

	if (missing.decimal_mode) {
		sprintf(string, "decimal mode, ");
		out << string;
	}

	if (missing.mv_8bit_index) {
		sprintf(string, "MVP/MVN with 8bit index registers and XH or YH > 0, ");
		out << string;
	}

	if (missing.mv_8bit_acc) {
		sprintf(string, "MVP/MVN with 8bit accumulator > 255, ");
		out << string;
	}

	sprintf(string, "\n");
	out << string;

	sprintf(string, "Screen modes used: ");
	out << string;

	for (int i = 0; i < 8; i++)
		if (missing.modes[i]) {
			sprintf(string, "%d, ", i);
			out << string;
		}

	sprintf(string, "\n");
	out << string;

	if (missing.interlace) {
		sprintf(string, "Interlace, ");
		out << string;
	}

	if (missing.pseudo_512) {
		sprintf(string, "Pseudo 512 pixels horizontal resolution, ");
		out << string;
	}

	if (missing.lines_239) {
		sprintf(string, "240 lines visible, ");
		out << string;
	}
	if (missing.sprite_double_height) {
		sprintf(string, "double-hight sprites, ");
		out << string;
	}

	sprintf(string, "\n");
	out << string;

	if (missing.mode7_fx) {
		sprintf(string, "Mode 7 rotation/scaling, ");
		out << string;
	}

	if (missing.matrix_read) {
		sprintf(string, "Mode 7 read matrix registers, ");
		out << string;
	}

	if (missing.mode7_flip) {
		sprintf(string, "Mode 7 flipping, ");
		out << string;
	}

	if (missing.mode7_bgmode) {
		sprintf(string, "Mode 7 priority per pixel, ");
		out << string;
	}

	if (missing.direct) {
		sprintf(string, "Direct 32000 colour mode, ");
		out << string;
	}

	sprintf(string, "\n");
	out << string;

	if (missing.mosaic) {
		sprintf(string, "Mosaic effect, ");
		out << string;
	}
	if (missing.subscreen) {
		sprintf(string, "Subscreen enabled, ");
		out << string;
	}

	if (missing.subscreen_add) {
		sprintf(string, "Subscreen colour add, ");
		out << string;
	}

	if (missing.subscreen_sub) {
		sprintf(string, "Subscreen colour subtract, ");
		out << string;
	}

	if (missing.fixed_colour_add) {
		sprintf(string, "Fixed colour add, ");
		out << string;
	}

	if (missing.fixed_colour_sub) {
		sprintf(string, "Fixed colour subtract, ");
		out << string;
	}

	sprintf(string, "\n");
	out << string;

	sprintf(string, "Window 1 enabled on: ");
	out << string;
	debug_print_window(out, missing.window1);

	sprintf(string, "\n");
	out << string;

	sprintf(string, "Window 2 enabled on: ");
	out << string;
	debug_print_window(out, missing.window2);

	sprintf(string, "\n");
	out << string;

	if (missing.bg_offset_read) {
		sprintf(string, "BG offset read, ");
		out << string;
	}

	if (missing.oam_address_read) {
		sprintf(string, "OAM address read, ");
		out << string;
	}
	if (missing.sprite_priority_rotation) {
		sprintf(string, "Sprite priority rotation, ");
		out << string;
	}
	if (missing.fast_rom) {
		sprintf(string, "Fast 3.58MHz ROM access enabled, ");
		out << string;
	}
	if (missing.matrix_multiply) {
		sprintf(string, "Matrix multiply 16bit by 8bit used, ");
		out << string;
	}
	sprintf(string, "\n");
	out << string;

	if (missing.virq) {
		sprintf(string, "V-IRQ used at line %d, ", missing.virq_pos);
		out << string;
	}
	if (missing.hirq) {
		sprintf(string, "H-IRQ used at position %d, ", missing.hirq_pos);
		out << string;
	}
	sprintf(string, "\n");
	out << string;

	if (missing.h_v_latch) {
		sprintf(string, "H and V-Pos latched, ");
		out << string;
	}
	if (missing.h_counter_read) {
		sprintf(string, "H-Pos read, ");
		out << string;
	}
	if (missing.v_counter_read) {
		sprintf(string, "V-Pos read, ");
		out << string;
	}
	sprintf(string, "\n");
	out << string;

	if (missing.oam_read) {
		sprintf(string, "OAM read, ");
		out << string;
	}
	if (missing.vram_read) {
		sprintf(string, "VRAM read, ");
		out << string;
	}
	if (missing.cgram_read) {
		sprintf(string, "CG-RAM read, ");
		out << string;
	}
	if (missing.wram_read) {
		sprintf(string, "WRAM read, ");
		out << string;
	}
	if (missing.dma_read) {
		sprintf(string, "DMA read, ");
		out << string;
	}
	if (missing.vram_inc) {
		sprintf(string, "VRAM inc: %d, ", missing.vram_inc);
		out << string;
	}
	if (missing.vram_full_graphic_inc) {
		sprintf(string, "VRAM full graphic inc: %d, ", missing.vram_full_graphic_inc);
		out << string;
	}
	sprintf(string, "\n");
	out << string;

	for (int i = 0; i < 8; i++)
	{
		if (missing.hdma[i].used)
		{
			sprintf(string, "HDMA %d 0x%02X%04X->0x21%02X %s, ",
				i, missing.hdma[i].abus_bank, missing.hdma[i].abus_address, missing.hdma[i].bbus_address,
				missing.hdma[i].indirect_address ? "indirect" : "absolute");
			out << string;

			if (missing.hdma[i].force_table_address_write) {
				sprintf(string, "Forced address write, ");
				out << string;
			}

			if (missing.hdma[i].force_table_address_read) {
				sprintf(string, "Current address read, ");
				out << string;
			}

			if (missing.hdma[i].line_count_write) {
				sprintf(string, "Line count write, ");
				out << string;
			}

			if (missing.hdma[i].line_count_read) {
				sprintf(string, "Line count read, ");
				out << string;
			}

			sprintf(string, "\n");
			out << string;
		}
	}

	for (int i = 0; i < 8; i++)
	{
		if (missing.dma_channels & (1 << i))
		{
			sprintf(string, "DMA %d [%d] 0x%02X%04X->0x21%02X Num: %d %s, \n",
				i, DMA[i].TransferMode, DMA[i].ABank, DMA[i].AAddress, DMA[i].BAddress, DMA[i].TransferBytes,
				DMA[i].AAddressFixed ? "fixed" : (DMA[i].AAddressDecrement ? "dec" : "inc"));
			out << string;
		}
	}

	if (missing.unknownppu_read) {
		sprintf(string, "Read from unknown PPU register: $%04X, \n", missing.unknownppu_read);
		out << string;
	}

	if (missing.unknownppu_write) {
		sprintf(string, "Write to unknown PPU register: $%04X, \n", missing.unknownppu_write);
		out << string;
	}

	if (missing.unknowncpu_read) {
		sprintf(string, "Read from unknown CPU register: $%04X, \n", missing.unknowncpu_read);
		out << string;
	}

	if (missing.unknowncpu_write) {
		sprintf(string, "Write to unknown CPU register: $%04X, \n", missing.unknowncpu_write);
		out << string;
	}

	if (missing.unknowndsp_read) {
		sprintf(string, "Read from unknown DSP register: $%04X, \n", missing.unknowndsp_read);
		out << string;
	}

	if (missing.unknowndsp_write) {
		sprintf(string, "Write to unknown DSP register: $%04X, \n", missing.unknowndsp_write);
		out << string;
	}
}

void S9xDebugPrintStatus(std::ostream &out) {
	char	string[512];
	debug_cpu_status_print(string);
	out << string << '\n';
}

void S9xDebugPrintVectors (std::ostream &out)
{
	struct {
		const char *name;
		uint16 addr8;
		uint16 addr16;
	} vectors[6] = {
		{ "ABT", 0xFFF8, 0xFFE8 },
		{ "BRK", 0xFFFE, 0xFFE6 },
		{ "COP", 0xFFF4, 0xFFE4 },
		{ "IRQ", 0xFFFE, 0xFFEE },
		{ "NMI", 0xFFFA, 0xFFEA },
		{ "RES", 0xFFFC, 0xFFFC }
	};
	char string[512];
	out << "Vectors:\n";
	out << "      8 Bit   16 Bit \n";
	for (int i = 0; i < 6; i++) {
		sprintf(string, "%s $00%04X|$00%04X\n", vectors[i].name, S9xDebugGetWord(vectors[i].addr8), S9xDebugGetWord(vectors[i].addr16));
		out << string;
	}
}

void S9xDebugPrintColors (std::ostream &out, bool rgb_mode) {
	char string[512];
	if (rgb_mode) {
		out << "Colors (RGB):\n    ";

		for (int i = 0; i < 16; i++) {

			out << std::setw(2) << i << "      ";
			if (i == 7) {
				out << "\n    ";
			}
		}

		for (int i = 0; i < 16; i++) {
			sprintf(string, "\n%2d  ", i);
			out << string;
			for (int j = 0; j < 16; j++) {
				sprintf(string, "%06x  ", ((PPU.CGDATA[i * 16 + j] & 0x1f) << 16) | (((PPU.CGDATA[i * 16 + j] >> 5) & 0x1f) << 8) | ((PPU.CGDATA[i * 16 + j] >> 10) & 0x1f));
				out << string;
				if (j == 7) {
					out << "\n    ";
				}
			}
		}

		out << '\n';
	} else {
		out << "Colors (SNES):\n    ";

		for (int i = 0; i < 16; i++) {

			sprintf(string, "%2d    ", i);
			out << string;
			if (i == 7) {
				out << "\n    ";
			}
		}

		for (int i = 0; i < 16; i++) {
			sprintf(string, "\n%2d  ", i);
			out << string;
			for (int j = 0; j < 16; j++) {
				sprintf(string, "%04x  ", PPU.CGDATA[i*16 + j]);
				out << string;
				if (j == 7) {
					out << "\n    ";
				}
			}
		}

		out << '\n';
	}
}

void S9xDebugPrintSprites (std::ostream &out)
{
	char string[2048];
	int	SmallWidth, LargeWidth, SmallHeight, LargeHeight;

	switch ((Memory.FillRAM[0x2101] >> 5) & 7)
	{
		case 0:
			SmallWidth = SmallHeight = 8;
			LargeWidth = LargeHeight = 16;
			break;

		case 1:
			SmallWidth = SmallHeight = 8;
			LargeWidth = LargeHeight = 32;
			break;

		case 2:
			SmallWidth = SmallHeight = 8;
			LargeWidth = LargeHeight = 64;
			break;

		case 3:
			SmallWidth = SmallHeight = 16;
			LargeWidth = LargeHeight = 32;
			break;

		case 4:
			SmallWidth = SmallHeight = 16;
			LargeWidth = LargeHeight = 64;
			break;

		default:
		case 5:
			SmallWidth = SmallHeight = 32;
			LargeWidth = LargeHeight = 64;
			break;

		case 6:
			SmallWidth = 16;
			SmallHeight = 32;
			LargeWidth = 32;
			LargeHeight = 64;
			break;

		case 7:
			SmallWidth = 16;
			SmallHeight = 32;
			LargeWidth = LargeHeight = 32;
			break;
	}

	sprintf(string, "\
Sprites: Small: %dx%d\n\
         Large: %dx%d\n\
         OAMAddr: 0x%04x\n\
         OBJNameBase: 0x%04x\n\
         OBJNameSelect: 0x%04x\n\
         First: %d\n",
		   SmallWidth,
		   SmallHeight,
		   LargeWidth,
		   LargeHeight,
		   PPU.OAMAddr,
		   PPU.OBJNameBase,
		   PPU.OBJNameSelect,
		   PPU.FirstSprite);
	out << string;

	out << "\nOAM:";
	for (int i = 0; i < 128; i++)
	{
		if (i % 4 == 0) {
			sprintf(string, "\n%2x  ", i);
			out << string;
		} else {
			out << "  |  ";
		}
		sprintf(string, "X:%3d Y:%3d %c%c%d%c",
			   PPU.OBJ[i].HPos,
			   PPU.OBJ[i].VPos,
			   PPU.OBJ[i].VFlip ? 'V' : 'v',
			   PPU.OBJ[i].HFlip ? 'H' : 'h',
			   PPU.OBJ[i].Priority,
			   PPU.OBJ[i].Size ? 'S' : 's');
		out << string;
	}
	out << '\n';
}

void S9xDebugPrintBreakpoints (std::ostream &out) {
	char string[512];
	out << "Active breakpoints:\n";
	for (int i = 0; i < 5; i++) {
		if (S9xBreakpoint[i].Enabled) {
			snprintf(string, 512, "  %d: $%06X\n", i, S9xBreakpoint[i].Address);
			out << string;
		}
	}
}

void S9xDebugPrintWatchpoints (std::ostream &out) {
	char string[512];
	out << "Active watchpoints:\n";
	for (int i = 0; i < 6; i++) {
		if (S9xWatchpoint[i].Mode != WATCH_MODE_NONE) {
			snprintf(string, 512, "  %d: $%06X (", i, S9xWatchpoint[i].Address);
			out << string;

			out << (S9xWatchpoint[i].Mode == WATCH_MODE_READ ? "read" :
				(S9xWatchpoint[i].Mode == WATCH_MODE_WRITE ? "write" : "both"));
			out << ")\n";
		}
	}
}

int S9xSetBreakpoint(uint32 addr) {
	int empty = -1;
	for (int i = 0; i < 5; i++) {
		if (S9xBreakpoint[i].Address == addr && S9xBreakpoint[i].Enabled) {
			return i;
		}
		else if (empty == -1 && !S9xBreakpoint[i].Enabled) {
			empty = i;
		}
	}
	if (empty != -1) {
		S9xBreakpoint[empty].Address = addr;
		S9xBreakpoint[empty].Enabled = TRUE;
		CPU.Flags |= BREAK_FLAG;
	}
	return empty;
}

int S9xSetWatchpoint(uint32 addr, uint8 type) {
	int empty = -1;
	for (int i = 0; i < 6; i++) {
		if (S9xWatchpoint[i].Address == addr && S9xWatchpoint[i].Mode != WATCH_MODE_NONE) {
			return i;
		}
		else if (empty == -1 && S9xWatchpoint[i].Mode == WATCH_MODE_NONE) {
			empty = i;
		}
	}
	if (empty != -1) {
		S9xWatchpoint[empty].Address = addr;
		S9xWatchpoint[empty].RealAddress = Memory.Map[(addr & 0xffffff) >> MEMMAP_SHIFT] + (addr & 0xffff);
		S9xWatchpoint[empty].Mode = type;
		CPU.Flags |= WATCH_FLAG;
	}
	return empty;
}

bool S9xRemoveBreakpoint(int i) {
	if (i <= 4 && i >= 0) {
		S9xBreakpoint[i].Enabled = FALSE;
		return true;
	}
	return false;
}

bool S9xRemoveWatchpoint(int i) {
	if (i <= 5 && i >= 0) {
		S9xWatchpoint[i].Mode = WATCH_MODE_NONE;
		return true;
	}
	return false;
}

void S9xDebugStepOver() {
	CPU.Flags |= STEP_OUT_FLAG;
	step_depth = 0;
	S9xStopDebug();
}

void S9xDebugStepOut() {
	CPU.Flags |= STEP_OUT_FLAG;
	step_depth = 1;
	S9xStopDebug();
}

void S9xDebugStepInto() {
	CPU.Flags |= SINGLE_STEP_FLAG;
	S9xStopDebug();
}

void S9xDebugContinue() {
	CPU.Flags |= CONTINUE_FLAG;
	S9xStopDebug();
}

void S9xDebugFrameAdvance(unsigned int frames) {
	CPU.Flags |= FRAME_ADVANCE_FLAG;
	S9xStopDebug();

	IPPU.RenderThisFrame = TRUE;
	IPPU.FrameSkip = 0;
	ICPU.FrameAdvanceCount = frames;
}

void S9xDebugCommand (const char *Line, std::ostream &out, bool is_redo)
{
	char	string[512];

	if (strncasecmp(Line, "status", 6) == 0) {
		S9xDebugPrintStatus(out);
	}

	else if (Line[0] == 'i') {
		const char * cmd = strchr(Line, ' ');
		if (!cmd) {
			out << "Usage: info [vectors | colors-snes | colors-rgb | sprites | missing | used | breakpoints | watchpoints]\n";
			return;
		}
		cmd++;

		if (strncasecmp(cmd, "vectors", 7) == 0)
			S9xDebugPrintVectors(out);
		else if (strncasecmp(cmd, "colors-rgb", 10) == 0 ||
				strncasecmp(cmd, "colours-rgb", 11) == 0)
			S9xDebugPrintColors(out, true);
		else if (strncasecmp(cmd, "colors", 6) == 0 ||
				strncasecmp(cmd, "colours", 6) == 0 ||
				strncasecmp(cmd, "colors-snes", 11) == 0 ||
				strncasecmp(cmd, "colours-snes", 12) == 0)
			S9xDebugPrintColors(out, false);
		else if (strncasecmp(cmd, "sprites", 7) == 0 ||
				strncasecmp(cmd, "oam", 3) == 0)
			S9xDebugPrintSprites(out);
		else if (strncasecmp(cmd, "missing", 7) == 0)
			S9xDebugPrintWhatsMissing(out);
		else if (strncasecmp(cmd, "used", 4) == 0)
			S9xDebugPrintWhatsUsed(out);
		else if (strncasecmp(cmd, "breakpoints", 11) == 0 ||
		    	strncasecmp(cmd, "break", 5) == 0)
			S9xDebugPrintBreakpoints(out);
		else if (strncasecmp(cmd, "watchpoints", 11) == 0 ||
				strncasecmp(cmd, "watch", 5) == 0)
			S9xDebugPrintWatchpoints(out);
		else
			out << "Usage: info [vectors | colors-snes | colors-rgb | sprites | missing | used | breakpoints | watchpoints]\n";
	}

	else if (Line[0] == 'x') { // eXamine memory
		const char *p;
		static uint32 count = 0;
		static uint8 wordsize = 1;
		static uint8 format = 1;	// {Dec, Hex, Insn}
		static uint32 address = 0;

		if (!is_redo) {
			if (Line[1] == '/') {
				count = 0;
				// Parse count
				for (p = &Line[2]; *p >= '0' && *p <= '9'; p++) {
					count *= 10;
					count += *p - '0';
				}

				// Parse wordsize
				if (*p == 'b') {
					wordsize = 1;
					p++;
				} else if (*p == 'c') {
					wordsize = 1;
					p++;
				} else if (*p == 'w') {
					wordsize = 2;
					p++;
				} else if (*p == 'h') {
					wordsize = 2;
					p++;
				} else if (*p == 'p') {
					wordsize = 3;
					p++;
				}

				// Parse format
				if (*p == 'd') {
					format = 0;
					p++;
				} else if (*p == 'x') {
					format = 1;
					p++;
				} else if (*p == 'i') {
					format = 2;
					p++;
				}
			} else {
				p = &Line[1];
			}

			if (count == 0)
				count = 1;

			if (p[0] != '\0') {
				// Parse address
				if (p[0] != ' ' || p[1] != '$' || !((p[2] >= '0' && p[2] <= '9') || (p[2] >= 'A' && p[2] <= 'Z') || (p[2] >= 'a' && p[2] <= 'z'))) {
					out << "Bad examine command.\nFormat: x/{count}{wordsize}{format} ${addr}\nWordsize can be one of 'bwp' (byte, word, pointer), and format can be one of 'dxi' (decimal, hex, instruction)\n";
					return;
				}
				p += 2;

				address = 0;
				for (; *p != '\0'; p++) {
					if (*p >= '0' && *p <= '9') {
						address *= 16;
						address += *p - '0';
					} else if (*p >= 'A' && *p <= 'Z') {
						address *= 16;
						address += *p - 'A' + 10;
					} else if (*p >= 'a' && *p <= 'z') {
						address *= 16;
						address += *p - 'a' + 10;
					} else if (*p == ':' || *p == ' ' || *p == 'x') {
					  continue;
					} else {
					out << "Bad examine command.\nFormat: x/{count}{wordsize}{format} ${addr}\nWordsize can be one of 'bwp' (byte, word, pointer), and format can be one of 'dxi' (decimal, hex, instruction)\n";
						return;
					}
				}
			}
		}

		// Do it!
		int perline = (wordsize == 1 ? 16 : (wordsize == 2 ? 8 : 4));
		for (unsigned int i = 0; i < count; i++) {
			if (format == 2) {
				address += debug_cpu_op_print(string, address >> 16, address & 0xFFFF);
				out << string << '\n';
			} else {
				if (i % perline == 0) {
					if (i != 0) out << '\n';
					sprintf(string, "$%06X", address);
					out << string;
				}
				uint32 value = 0;
				for (int j = 0; j < wordsize; j++) {
					value |= S9xDebugGetByte(address) << (j * 8);
					address += 1;
				}

				sprintf(string, wordsize == 1 ? 
					(format == 0 ? " %3d" : " %02X")
						:
					(wordsize == 2 ? 
					(format == 0 ? " %5d" : " %04X")
					    :
					(format == 0 ? " %7d" : " %06X")), value);
				out << string;
			}
		}
		if (format != 2) out << '\n';
	}

	else if (strncasecmp(Line, "dump", 4) == 0)
	{
		int count;
		FILE *fs;
		unsigned int addr;
		if (sscanf(&Line[4], "$%x %d", &addr, &count) == 2)
		{
			sprintf(string, "%06x%05d.sd2", addr, count);
			fs = fopen(string, "wb");
			if (fs) {
				for (int i = 0; i < count; i++)
					putc(S9xDebugGetByte(addr + i), fs);
				fclose(fs);
				out << "Dumped " << count << " bytes to " << string << '\n';
			} else {
				out << "Can't open " << string << " for writing\n";
			}
		}
		else
			out << "Usage: dump $addr count\n";
	}

	else if (*Line == 'R' || *Line == 'r')	// reset
	{
		//sprintf(string, "Are you sure you want to reset the emulator? [y/n] ");
		//const char * p = fgets(string, sizeof(string) - 1, stdin);
		//if (p[0] == 'y' || p[0] == 'Y') {
			S9xReset();
			out << "SNES reset.\n";
		//}
	}

	else if (strncasecmp(Line, "trace", 5) == 0)
	{
		if (strncasecmp(Line + 6, "sa1", 3) == 0) {
			SA1.Flags ^= TRACE_FLAG;

			if (SA1.Flags & TRACE_FLAG)
			{
				out << "Tracing SA1 instructions to trace_sa1.log\n";
				ENSURE_TRACE_OPEN(trace2, "trace_sa1.log", "wb")
			}
			else
			{
				out << "SA1 CPU instruction tracing disabled\n";
				fclose(trace2);
				trace2 = NULL;
			}
		}
		else if (strncasecmp(Line + 6, "cpu", 3) == 0) {
			CPU.Flags ^= TRACE_FLAG;

			if (CPU.Flags & TRACE_FLAG)
			{
				out << "Tracing SNES CPU instructions to trace.log\n";
				ENSURE_TRACE_OPEN(trace, "trace.log", "wb")
			}
			else
			{
				out << "CPU instruction tracing disabled\n";
				fclose(trace);
				trace = NULL;
			}
		}
		else if (strncasecmp(Line + 6, "hcevent", 7) == 0) {
			Settings.TraceHCEvent = !Settings.TraceHCEvent;
			out << (Settings.TraceHCEvent ? "HC event tracing enabled\n" : "HC event tracing disabled\n");
		}
		else if (strncasecmp(Line + 6, "dma", 3) == 0) {
			Settings.TraceDMA = !Settings.TraceDMA;
			out << (Settings.TraceDMA ? "DMA tracing enabled\n" : "DMA tracing disabled\n");
		}
		else if (strncasecmp(Line + 6, "vram", 4) == 0) {
			Settings.TraceVRAM = !Settings.TraceVRAM;
			out << (Settings.TraceVRAM ? "Non-DMA VRAM write tracing enabled\n" : "Non-DMA VRAM write tracing disabled\n");
		}
		else if (strncasecmp(Line + 6, "hdma", 4) == 0) {
			Settings.TraceHDMA = !Settings.TraceHDMA;
			out << (Settings.TraceHDMA ? "HDMA tracing enabled" : "HDMA tracing disabled\n");
		}
		else if (strncasecmp(Line + 6, "unknowns", 8) == 0) {
			Settings.TraceUnknownRegisters = !Settings.TraceUnknownRegisters;
			out << (Settings.TraceUnknownRegisters ? "Unknown registers read/write tracing enabled\n" : "Unknown registers read/write tracing disabled\n");
		}
		else {
			out << "Usage: trace [cpu | sa1 | hcevent | dma | vram | hdma | unknowns]\n";
		}
	}

	else if (*Line == 'z') { // ???
		uint16	*p = (uint16 *) &Memory.VRAM[PPU.BG[2].SCBase << 1];

		for (int l = 0; l < 32; l++)
		{
			for (int c = 0; c < 32; c++, p++) {
				sprintf(string, "%04X ", *p++);
				out << string;
			}
			out << '\n';
		}
	}

	else if (*Line == 'n' || *Line == 'N') {	// Next
		S9xDebugStepOver();
	}

	else if (*Line == 'o' || *Line == 'O') {	// Step out
		S9xDebugStepOut();
	}

	else if (*Line == 'b' || *Line == 'B') { // Break
		uint32 addr;
		if (sscanf(Line, "%*s $%x", &addr) != 1) {
			out << "Usage: break ${addr}\n";
			return;
		}
		if (addr > 0xFFFFFF) {
			out << "Invalid address\n";
			return;
		}
		int i = S9xSetBreakpoint(addr);
		if (i == -1) {
			out << "Too many breakpoints!\n";
		} else {
			sprintf(string, "Set breakpoint %d\n", i);
			out << string;
		}
	}

	else if (*Line == 'w' || *Line == 'W') { //Watch
		uint32 addr;
		uint8 type;
		if (sscanf(Line, "%*s $%x %s", &addr, string) != 2) {
			out << "Usage: watch ${addr} {read,write,both}\n";
			return;
		}
		if (addr > 0xFFFFFF) {
			out << "Invalid address\n";
			return;
		}
		if (string[0] == 'r' || string[0] == 'R')
			type = WATCH_MODE_READ;
		else if (string[0] == 'w' || string[0] == 'W')
			type = WATCH_MODE_BOTH;
		else if (string[0] == 'b' || string[0] == 'B')
			type = WATCH_MODE_BOTH;
		else {
			out << "Usage: watch ${addr} {read,write,both}\n";
			return;
		}

		int i = S9xSetWatchpoint(addr, type);
		if (i == -1) {
			out << "Too many watchpoints!\n";
		} else {
			sprintf(string, "Set watchpoint %d\n", i);
			out << string;
		}
	}

	else if (strncasecmp(Line, "unwatch", 7) == 0) {
		int i;
		if (sscanf(Line, "%*s %d", &i) != 1) {
			out << "Usage: unwatch {watchpoint num}\n";
			return;
		}
		if (i < 0 || i > 5) {
			out << "Bad watchpoint number\n";
			return;
		}

		S9xRemoveWatchpoint(i);
		sprintf(string, "Removed watchpoint %d\n", i);
		out << string;
	}

	else if (*Line == 'u' || *Line == 'U') { // unbreak
		int i;
		if (sscanf(Line, "%*s %d", &i) != 1) {
			out << "Usage: unbreak {breakpoint num}\n";
			return;
		}
		if (i < 0 || i > 4) {
			out << "Bad breakpoint number\n";
			return;
		}
		S9xRemoveBreakpoint(i);
		sprintf(string, "Removed breakpoint %d\n", i);
		out << string;
	}

	else if (*Line == '?' || strncasecmp(Line, "help", 4) == 0)
	{
		for (int i = 0; HelpMessage[i] != NULL; i++)
			out << HelpMessage[i] << '\n';
	}

	else if (*Line == 's' || *Line == 'S') {
		S9xDebugStepInto();
	}

	else if (*Line == 'f' || *Line == 'F')
	{
		unsigned int frames = 0;
		sscanf(&Line[1], "%u", &frames);
		if (frames) frames--;

		S9xDebugFrameAdvance(frames);
	}

	else if (*Line == 'c' || *Line == 'C') {
		S9xDebugContinue();
	}

	else {
		out << "Invalid command.\n";
	}

	return;
}

void S9xTrace (void)
{
	char	msg[512];

	ENSURE_TRACE_OPEN(trace, "trace.log", "a")

	debug_cpu_status_print(msg);
	fprintf(trace, "%s\n", msg);
}

void S9xSA1Trace (void)
{
	char	msg[512];

	ENSURE_TRACE_OPEN(trace2, "trace_sa1.log", "a")

	debug_sa1_op_print(msg, SA1Registers.PB, SA1Registers.PCw);
	fprintf(trace2, "%s\n", msg);
}

void S9xTraceMessage (const char *s)
{
	if (s)
	{
		if (trace)
			fprintf(trace, "%s\n", s);
		else
		if (trace2)
			fprintf(trace2, "%s\n", s);
	}
}

void S9xTraceFormattedMessage (const char *s, ...)
{
	char	msg[512];

	if (s)
	{
		va_list	argptr;

		va_start(argptr, s);
		vsprintf(msg, s, argptr);
		va_end(argptr);

		S9xTraceMessage(msg);
	}
}

void S9xPrintHVPosition (char *s)
{
	sprintf(s, "HC:%04ld VC:%03ld FC:%02d", (long) CPU.Cycles, (long) CPU.V_Counter, IPPU.FrameCount);
}

#endif
