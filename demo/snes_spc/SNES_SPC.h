// SNES SPC-700 APU emulator

// snes_spc 0.9.0
#ifndef SNES_SPC_H
#define SNES_SPC_H

#include "SPC_DSP.h"
#include "SnesSmp.h"
#include "blargg_endian.h"
#include "CommonTypes.h"

class SNES_SPC {
public:
	SNES_SPC();
	~SNES_SPC();
	
	// Sample pairs generated per second
	enum { sample_rate = 32000 };
	
// Emulator use
	
	// Sets IPL ROM data. Library does not include ROM data. Most SPC music files
	// don't need ROM, but a full emulator must provide this.
	enum { rom_size = 0x40 };
	void init_rom( uint8_t const rom [rom_size] );

	// Sets destination for output samples
	typedef short sample_t;
	void set_output( sample_t* out, int out_size );

	// Number of samples written to output since last set
	int sample_count() const;

	// Resets SPC to power-on state. This resets your output buffer, so you must
	// call set_output() after this.
	void reset();

	// Emulates pressing reset switch on SNES. This resets your output buffer, so
	// you must call set_output() after this.
	void soft_reset();

	// 1024000 SPC clocks per second, sample pair every 32 clocks
	typedef int time_t;
	enum { clock_rate = 1024000 };
	enum { clocks_per_sample = 32 };
	
	// Emulated port read/write at specified time
	enum { port_count = 4 };
	int  read_port ( time_t, int port );
	void write_port( time_t, int port, int data );

	// Runs SPC to end_time and starts a new time frame at 0
	void end_frame( time_t end_time );

// SPC music files

	// Loads SPC data into emulator
	enum { spc_min_file_size = 0x10180 };
	enum { spc_file_size     = 0x10200 };
	blargg_err_t load_spc( void const* in, long size );
	
	// Clears echo region. Useful after loading an SPC as many have garbage in echo.
	void clear_echo();

	// Plays for count samples and write samples to out. Discards samples if out
	// is NULL. Count must be a multiple of 2 since output is stereo.
	blargg_err_t play( int count, sample_t* out );
	
	// Skips count samples. Several times faster than play() when using fast DSP.
	blargg_err_t skip( int count );

public:
	BLARGG_DISABLE_NOTHROW
	
	struct Timer
	{
		rel_time_t next_time; // time of next event
		int prescaler;
		int period;
		int divider;
		int enabled;
		int counter;
	};
	enum { reg_count = 0x10 };
	enum { timer_count = 3 };
	enum { extra_size = SPC_DSP::extra_size };
	
	enum { signature_size = 35 };
	enum { rom_addr = 0xFFC0 };
	
private:
	SnesSmp *smp;
	SPC_DSP *dsp;

	Timer timers [timer_count];
		
	uint8_t smp_regs [2] [reg_count];
	
	rel_time_t  dsp_time;
	time_t      spc_time;
	bool        echo_accessed;
		
	int         skipped_kon;
	int         skipped_koff;
	const char* cpu_error;
		
	int         extra_clocks;
	sample_t*   buf_begin;
	sample_t const* buf_end;
	sample_t*   extra_pos;
	sample_t    extra_buf [extra_size];
		
	int         rom_enabled;
	uint8_t     rom    [rom_size];
	uint8_t     hi_ram [rom_size];
		
	unsigned char cycle_table [256];
	
	uint8_t ram      [0x10000];
	
	enum { skipping_time = 127 };
	
	// Value that padding should be filled with
	enum { cpu_pad_fill = 0xFF };
	
	enum {
        r_test     = 0x0, r_control  = 0x1,
        r_dspaddr  = 0x2, r_dspdata  = 0x3,
        r_cpuio0   = 0x4, r_cpuio1   = 0x5,
        r_cpuio2   = 0x6, r_cpuio3   = 0x7,
        r_f8       = 0x8, r_f9       = 0x9,
        r_t0target = 0xA, r_t1target = 0xB, r_t2target = 0xC,
        r_t0out    = 0xD, r_t1out    = 0xE, r_t2out    = 0xF
	};
	
	void timers_loaded();
	void enable_rom( int enable );
	void reset_buf();
	void save_extra();
	void load_regs( uint8_t const in [reg_count] );
	void ram_loaded();
	void reset_time_regs();
	void reset_common( int timer_counter_init );
	
	Timer* run_timer_      ( Timer* t, rel_time_t );
	Timer* run_timer       ( Timer* t, rel_time_t );
	int dsp_read           ( rel_time_t );
	void dsp_write         ( int data, rel_time_t );
	void cpu_write_smp_reg_( int data, rel_time_t, int addr );
	void cpu_write_smp_reg ( int data, rel_time_t, int addr );
	void cpu_write         ( int data, int addr, rel_time_t );
	int cpu_read           ( int addr, rel_time_t );
	
	bool check_echo_access ( int addr );

	uint8_t* run_until_( time_t end_time );

	void run_until( time_t end_time, time_t& rel_time );
	unsigned CPU_mem_bit   ( uint16_t pc, rel_time_t );

	static char const signature [signature_size + 1];
};

struct spc_file_t
{
	char    signature [SNES_SPC::signature_size];
	uint8_t has_id666;
	uint8_t version;
	uint8_t pcl, pch;
	uint8_t a;
	uint8_t x;
	uint8_t y;
	uint8_t psw;
	uint8_t sp;
	char    text [212];
	uint8_t ram [0x10000];
	uint8_t dsp [128];
	uint8_t unused [0x40];
	uint8_t ipl_rom [0x40];
};

#include <assert.h>

inline int SNES_SPC::sample_count() const { return (extra_clocks >> 5) * 2; }

#endif
