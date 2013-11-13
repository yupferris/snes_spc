#ifndef __SNES_SMP_H__
#define __SNES_SMP_H__

#include "blargg_common.h"

class SNES_SPC;
struct spc_file_t;

class SnesSmp
{
public:
	SnesSmp(SNES_SPC *apu);

	void Reset();
	void LoadSpc(const spc_file_t *spc);

private:
	SNES_SPC *apu;
};

#endif
