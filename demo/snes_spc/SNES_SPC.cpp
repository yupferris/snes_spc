// Core SPC emulation: CPU, timers, SMP registers, memory

// snes_spc 0.9.0. http://www.slack.net/~ant/

#include "SNES_SPC.h"

#include <string.h>

/* Copyright (C) 2004-2007 Shay Green. This module is free software; you
can redistribute it and/or modify it under the terms of the GNU Lesser
General Public License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version. This
module is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details. You should have received a copy of the GNU Lesser General Public
License along with this module; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA */

#include "blargg_source.h"

#define RAM         (ram)
#define REGS        (smp_regs [0])
#define REGS_IN     (smp_regs [1])

// (n ? n : 256)
#define IF_0_THEN_256( n ) ((uint8_t) ((n) - 1) + 1)

#ifdef BLARGG_ENABLE_OPTIMIZER
	#include BLARGG_ENABLE_OPTIMIZER
#endif


//// Timers

#define TIMER_DIV( t, n ) ((n) >> t->prescaler)
#define TIMER_MUL( t, n ) ((n) << t->prescaler)

SNES_SPC::Timer* SNES_SPC::run_timer_( Timer* t, rel_time_t time )
{
	int elapsed = TIMER_DIV( t, time - t->next_time ) + 1;
	t->next_time += TIMER_MUL( t, elapsed );
	
	if ( t->enabled )
	{
		int remain = IF_0_THEN_256( t->period - t->divider );
		int divider = t->divider + elapsed;
		int over = elapsed - remain;
		if ( over >= 0 )
		{
			int n = over / t->period;
			t->counter = (t->counter + 1 + n) & 0x0F;
			divider = over - n * t->period;
		}
		t->divider = (uint8_t) divider;
	}
	return t;
}

inline SNES_SPC::Timer* SNES_SPC::run_timer( Timer* t, rel_time_t time )
{
	if ( time >= t->next_time )
		t = run_timer_( t, time );
	return t;
}


//// ROM

void SNES_SPC::enable_rom( int enable )
{
	if ( rom_enabled != enable )
	{
		rom_enabled = enable;
		if ( enable )
			memcpy( hi_ram, &RAM [rom_addr], sizeof hi_ram );
		memcpy( &RAM [rom_addr], (enable ? rom : hi_ram), rom_size );
		// TODO: ROM can still get overwritten when DSP writes to echo buffer
	}
}


//// DSP

#define RUN_DSP( time, offset ) \
	{\
		int count = (time) - dsp_time;\
		assert( count > 0 );\
		dsp_time = (time);\
		dsp->run( count );\
	}

int SNES_SPC::dsp_read( rel_time_t time )
{
	RUN_DSP( time, reg_times [REGS [r_dspaddr] & 0x7F] );
	
	int result = dsp->read( REGS [r_dspaddr] & 0x7F );
	
	return result;
}

inline void SNES_SPC::dsp_write( int data, rel_time_t time )
{
	RUN_DSP( time, reg_times [REGS [r_dspaddr]] )
	
	if ( REGS [r_dspaddr] <= 0x7F )
		dsp->write( REGS [r_dspaddr], data );
	else
		dprintf( "SPC wrote to DSP register > $7F\n" );
}


//// Memory access extras

// divided into multiple functions to keep rarely-used functionality separate
// so often-used functionality can be optimized better by compiler

int const no_read_before_write = 0x2000;

void SNES_SPC::cpu_write_smp_reg_( int data, int addr )
{
	rel_time_t time = rel_time;

	switch ( addr )
	{
	case r_t0target:
	case r_t1target:
	case r_t2target: {
		Timer* t = &timers [addr - r_t0target];
		int period = IF_0_THEN_256( data );
		if ( t->period != period )
		{
			t = run_timer( t, time );
			t->period = period;
		}
		break;
	}
	
	case r_t0out:
	case r_t1out:
	case r_t2out:
		dprintf( "SPC wrote to counter %d\n", (int) addr - r_t0out );
		
		if ( data < no_read_before_write  / 2 )
			run_timer( &timers [addr - r_t0out], time - 1 )->counter = 0;
		break;
	
	// Registers that act like RAM
	case 0x8:
	case 0x9:
		REGS_IN [addr] = (uint8_t) data;
		break;
	
	case r_test:
		if ( (uint8_t) data != 0x0A )
			dprintf( "SPC wrote to test register\n" );
		break;
	
	case r_control:
		// port clears
		if ( data & 0x10 )
		{
			REGS_IN [r_cpuio0] = 0;
			REGS_IN [r_cpuio1] = 0;
		}
		if ( data & 0x20 )
		{
			REGS_IN [r_cpuio2] = 0;
			REGS_IN [r_cpuio3] = 0;
		}
		
		// timers
		{
			for ( int i = 0; i < timer_count; i++ )
			{
				Timer* t = &timers [i];
				int enabled = data >> i & 1;
				if ( t->enabled != enabled )
				{
					t = run_timer( t, time );
					t->enabled = enabled;
					if ( enabled )
					{
						t->divider = 0;
						t->counter = 0;
					}
				}
			}
		}
		enable_rom( data & 0x80 );
		break;
	}
}

void SNES_SPC::cpu_write_smp_reg( int data, int addr )
{
	rel_time_t time = rel_time;

	if ( addr == r_dspdata ) // 99%
		dsp_write( data, time );
	else
		cpu_write( data, time );
}

int const bits_in_int = CHAR_BIT * sizeof (int);

void SNES_SPC::cpu_write( uint8_t data, uint16_t addr )
{
	rel_time_t time = rel_time;

	// RAM
	RAM [addr] = (uint8_t) data;
	int reg = addr - 0xF0;
	if ( reg >= 0 ) // 64%
	{
		// $F0-$FF
		if ( reg < reg_count ) // 87%
		{
			REGS [reg] = (uint8_t) data;
			
			// Registers other than $F2 and $F4-$F7
			//if ( reg != 2 && reg != 4 && reg != 5 && reg != 6 && reg != 7 )
			// TODO: this is a bit on the fragile side
			if ( ((~0x2F00 << (bits_in_int - 16)) << reg) < 0 ) // 36%
				cpu_write_smp_reg( data, reg );
		}
		// High mem/address wrap-around
		else
		{
			reg -= rom_addr - 0xF0;
			if ( reg >= 0 ) // 1% in IPL ROM area or address wrapped around
			{
				if ( reg < rom_size )
				{
					hi_ram [reg] = (uint8_t) data;
					if ( rom_enabled )
						RAM [reg + rom_addr] = rom [reg]; // restore overwritten ROM
				}
				else
				{
					assert( RAM [reg + rom_addr] == (uint8_t) data );
					RAM [reg + rom_addr] = cpu_pad_fill; // restore overwritten padding
					cpu_write( data, reg + rom_addr - 0x10000 );
				}
			}
		}
	}
}


//// CPU read

int SNES_SPC::cpu_read( int addr )
{
	rel_time_t time = rel_time;
	// RAM
	int result = RAM [addr];
	int reg = addr - 0xF0;
	if ( reg >= 0 ) // 40%
	{
		reg -= 0x10;
		if ( (unsigned) reg >= 0xFF00 ) // 21%
		{
			reg += 0x10 - r_t0out;
			
			// Timers
			if ( (unsigned) reg < timer_count ) // 90%
			{
				Timer* t = &timers [reg];
				if ( time >= t->next_time )
					t = run_timer_( t, time );
				result = t->counter;
				t->counter = 0;
			}
			// Other registers
			else if ( reg < 0 ) // 10%
			{
				reg += r_t0out;
				result = REGS_IN [reg];
				reg -= r_dspaddr;
				// DSP addr and data
				if ( (unsigned) reg <= 1 ) // 4% 0xF2 and 0xF3
				{
					result = REGS [r_dspaddr];
					if ( (unsigned) reg == 1 )
						result = dsp_read( time ); // 0xF3
				}
			}
			else // 1%
			{
				assert( reg + (r_t0out + 0xF0 - 0x10000) < 0x100 );
				result = cpu_read( reg + (r_t0out + 0xF0 - 0x10000) );
			}
		}
	}
	
	return result;
}

int SNES_SPC::read_port( time_t t, int port )
{
	assert( (unsigned) port < port_count );
	return run_until_( t ) [port];
}

void SNES_SPC::write_port( time_t t, int port, int data )
{
	assert( (unsigned) port < port_count );
	run_until_( t ) [0x10 + port] = data;
}


//// Run

int const cpu_lag_max = 12 - 1; // DIV YA,X takes 12 clocks

void SNES_SPC::end_frame( time_t end_time )
{
	// Catch CPU up to as close to end as possible. If final instruction
	// would exceed end, does NOT execute it and leaves m.spc_time < end.
	if ( end_time > spc_time )
		run_until_( end_time );
	
	spc_time     -= end_time;
	extra_clocks += end_time;
	
	// Greatest number of clocks early that emulation can stop early due to
	// not being able to execute current instruction without going over
	// allowed time.
	assert( -cpu_lag_max <= spc_time && spc_time <= 0 );
	
	// Catch timers up to CPU
	for ( int i = 0; i < timer_count; i++ )
		run_timer( &timers [i], 0 );
	
	// Catch DSP up to CPU
	if ( dsp_time < 0 )
	{
		RUN_DSP( 0, max_reg_time );
	}
	
	// Save any extra samples beyond what should be generated
	if ( buf_begin )
		save_extra();
}

BOOST::uint8_t* SNES_SPC::run_until_( time_t end_time )
{
	rel_time = spc_time - end_time;
	assert( rel_time <= 0 );
	spc_time = end_time;
	dsp_time += rel_time;
	timers [0].next_time += rel_time;
	timers [1].next_time += rel_time;
	timers [2].next_time += rel_time;

	smp->run_until( rel_time );

	spc_time += rel_time;
	dsp_time -= rel_time;
	timers [0].next_time -= rel_time;
	timers [1].next_time -= rel_time;
	timers [2].next_time -= rel_time;
	assert( spc_time <= end_time );
	return &REGS [r_cpuio0];
}
