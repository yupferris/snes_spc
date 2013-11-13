#ifndef __SNES_SMP_H__
#define __SNES_SMP_H__

#include "blargg_common.h"

class SNES_SPC;

class SnesSmp
{
public:
	SnesSmp(SNES_SPC *apu);

private:
	SNES_SPC *apu;
};

#endif
