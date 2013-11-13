#ifndef __SNES_SMP_H__
#define __SNES_SMP_H__

#include "blargg_common.h"
#include "CommonTypes.h"

class SNES_SPC;
struct spc_file_t;

class SnesSmp
{
public:
	SnesSmp(SNES_SPC *apu);

	void Reset();
	void LoadSpc(const spc_file_t *spc);

	struct
	{
		uint16_t pc;
		uint8_t a;
		uint8_t x;
		uint8_t y;
		uint8_t psw;
		uint8_t sp;
	} Regs;

private:
	SNES_SPC *apu;
};

#endif
