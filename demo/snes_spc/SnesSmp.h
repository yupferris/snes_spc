#ifndef __SNES_SMP_H__
#define __SNES_SMP_H__

#include <SnesApu.h>

#include "blargg_common.h"
#include "CommonTypes.h"

class SNES_SPC;
struct spc_file_t;

class SnesSmp : public SnesApu::SmpBase
{
public:
	SnesSmp(SNES_SPC *apu);

	virtual void Reset();

	virtual int Run(int targetCycles);

	virtual void SetRegPc(unsigned short value);
	virtual void SetRegA(unsigned char value);
	virtual void SetRegX(unsigned char value);
	virtual void SetRegY(unsigned char value);
	virtual void SetRegSp(unsigned char value);
	virtual void SetRegYa(unsigned short value);

	virtual unsigned short GetRegPc() const;
	virtual unsigned char GetRegA() const;
	virtual unsigned char GetRegX() const;
	virtual unsigned char GetRegY() const;
	virtual unsigned char GetRegSp() const;
	virtual unsigned short GetRegYa() const;

	virtual void SetPsw(unsigned char value);

	virtual unsigned char GetPsw() const;
	virtual bool GetPswC() const;
	virtual bool GetPswZ() const;
	virtual bool GetPswH() const;
	virtual bool GetPswP() const;
	virtual bool GetPswV() const;
	virtual bool GetPswN() const;

private:
	unsigned CPU_mem_bit   ( uint16_t pc );

	unsigned char cycle_table [256];

	struct
	{
		uint16_t pc;
		uint8_t a;
		uint8_t x;
		uint8_t y;
		uint8_t psw;
		uint8_t sp;
	} regs;
};

#endif
