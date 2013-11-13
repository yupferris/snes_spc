#include "SnesSmp.h"
#include "SNES_SPC.h"
#include <string.h>

SnesSmp::SnesSmp(SNES_SPC *apu)
{
	this->apu = apu;
}

void SnesSmp::Reset()
{
	// Run IPL ROM
	memset( &Regs, 0, sizeof Regs );
	Regs.pc = SNES_SPC::rom_addr;
}

void SnesSmp::LoadSpc(const spc_file_t *spc)
{
	Regs.pc  = spc->pch * 0x100 + spc->pcl;
	Regs.a   = spc->a;
	Regs.x   = spc->x;
	Regs.y   = spc->y;
	Regs.psw = spc->psw;
	Regs.sp  = spc->sp;
}
