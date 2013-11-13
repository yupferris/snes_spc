#ifndef __SNES_SMP_H__
#define __SNES_SMP_H__

#include "blargg_common.h"
#include "CommonTypes.h"

class SNES_SPC;
struct spc_file_t;

class SnesSmp
{
public:
	struct
	{
		uint16_t pc;
		uint8_t a;
		uint8_t x;
		uint8_t y;
		uint8_t psw;
		uint8_t sp;
	} Regs;

	SnesSmp(SNES_SPC *apu);

	void Reset();
	void LoadSpc(const spc_file_t *spc);

	void run_until( time_t end_time, rel_time_t& rel_time );
	unsigned CPU_mem_bit   ( uint16_t pc, rel_time_t );

private:
	unsigned char cycle_table [256];

	SNES_SPC *apu;
};

#endif
