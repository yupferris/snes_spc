#include "SnesSmp.h"
#include "SNES_SPC.h"
#include <string.h>

SnesSmp::SnesSmp(SNES_SPC *apu)
{
	this->apu = apu;
}

void SnesSmp::Reset()
{
}

void SnesSmp::LoadSpc(const spc_file_t *spc)
{
}
