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

	int Run(int targetCycles);

	void SetRegPc(unsigned short value);
	void SetRegA(unsigned char value);
	void SetRegX(unsigned char value);
	void SetRegY(unsigned char value);
	void SetRegSp(unsigned char value);
	void SetRegYa(unsigned short value);

	unsigned short GetRegPc() const;
	unsigned char GetRegA() const;
	unsigned char GetRegX() const;
	unsigned char GetRegY() const;
	unsigned char GetRegSp() const;
	unsigned short GetRegYa() const;

	void SetPsw(unsigned char value);

	unsigned char GetPsw() const;
	bool GetPswC() const;
	bool GetPswZ() const;
	bool GetPswH() const;
	bool GetPswP() const;
	bool GetPswV() const;
	bool GetPswN() const;

private:
	SNES_SPC *apu;

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
