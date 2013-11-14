#include "SnesSmp.h"
#include "SNES_SPC.h"
#include "blargg_source.h"
#include <string.h>

SnesSmp::SnesSmp(SNES_SPC *apu)
	: SmpBase(apu)
{
}

void SnesSmp::Reset()
{
	// Run IPL ROM
	memset( &regs, 0, sizeof regs );
	regs.pc = SNES_SPC::rom_addr;

	static unsigned char const cycle_table [128] =
	{//   01   23   45   67   89   AB   CD   EF
	    0x28,0x47,0x34,0x36,0x26,0x54,0x54,0x68, // 0
	    0x48,0x47,0x45,0x56,0x55,0x65,0x22,0x46, // 1
	    0x28,0x47,0x34,0x36,0x26,0x54,0x54,0x74, // 2
	    0x48,0x47,0x45,0x56,0x55,0x65,0x22,0x38, // 3
	    0x28,0x47,0x34,0x36,0x26,0x44,0x54,0x66, // 4
	    0x48,0x47,0x45,0x56,0x55,0x45,0x22,0x43, // 5
	    0x28,0x47,0x34,0x36,0x26,0x44,0x54,0x75, // 6
	    0x48,0x47,0x45,0x56,0x55,0x55,0x22,0x36, // 7
	    0x28,0x47,0x34,0x36,0x26,0x54,0x52,0x45, // 8
	    0x48,0x47,0x45,0x56,0x55,0x55,0x22,0xC5, // 9
	    0x38,0x47,0x34,0x36,0x26,0x44,0x52,0x44, // A
	    0x48,0x47,0x45,0x56,0x55,0x55,0x22,0x34, // B
	    0x38,0x47,0x45,0x47,0x25,0x64,0x52,0x49, // C
	    0x48,0x47,0x56,0x67,0x45,0x55,0x22,0x83, // D
	    0x28,0x47,0x34,0x36,0x24,0x53,0x43,0x40, // E
	    0x48,0x47,0x45,0x56,0x34,0x54,0x22,0x60, // F
	};
	
	// unpack cycle table
	for ( int i = 0; i < 128; i++ )
	{
		int n = cycle_table [i];
		this->cycle_table [i * 2 + 0] = n >> 4;
		this->cycle_table [i * 2 + 1] = n & 0x0F;
	}
}

void SnesSmp::SetRegPc(unsigned short value)
{
	regs.pc = value;
}

void SnesSmp::SetRegA(unsigned char value)
{
	regs.a = value;
}

void SnesSmp::SetRegX(unsigned char value)
{
	regs.x = value;
}

void SnesSmp::SetRegY(unsigned char value)
{
	regs.y = value;
}

void SnesSmp::SetRegSp(unsigned char value)
{
	regs.sp = value;
}

void SnesSmp::SetRegYa(unsigned short value)
{
	regs.a = value & 0xff;
	regs.y = (value >> 8) & 0xff;
}

unsigned short SnesSmp::GetRegPc() const
{
	return regs.pc;
}

unsigned char SnesSmp::GetRegA() const
{
	return regs.a;
}

unsigned char SnesSmp::GetRegX() const
{
	return regs.x;
}

unsigned char SnesSmp::GetRegY() const
{
	return regs.y;
}

unsigned char SnesSmp::GetRegSp() const
{
	return regs.sp;
}

unsigned short SnesSmp::GetRegYa() const
{
	return (regs.y << 8) | regs.a;
}

void SnesSmp::SetPsw(unsigned char value)
{
	regs.psw = value;
}

// Hex value in name to clarify code and bit shifting.
// Flag stored in indicated variable during emulation
int const n80 = 0x80; // nz
int const v40 = 0x40; // psw
int const p20 = 0x20; // dp
int const b10 = 0x10; // psw
int const h08 = 0x08; // psw
int const i04 = 0x04; // psw
int const z02 = 0x02; // nz
int const c01 = 0x01; // c

int const nz_neg_mask = 0x880; // either bit set indicates N flag set

unsigned char SnesSmp::GetPsw() const
{
	return regs.psw;
}

bool SnesSmp::GetPswC() const
{
	return (regs.psw & c01) != 0;
}

bool SnesSmp::GetPswZ() const
{
	return (regs.psw & z02) != 0;
}

bool SnesSmp::GetPswH() const
{
	return (regs.psw & h08) != 0;
}

bool SnesSmp::GetPswP() const
{
	return (regs.psw & p20) != 0;
}

bool SnesSmp::GetPswV() const
{
	return (regs.psw & v40) != 0;
}

bool SnesSmp::GetPswN() const
{
	return (regs.psw & n80) != 0;
}

//// Memory access

int const no_read_before_write = 0x2000;

#define SUSPICIOUS_OPCODE( name ) dprintf( "SPC: suspicious opcode: " name "\n" )

#define CPU_READ( time, offset, addr )\
	emulator->ReadByte( addr )

#define CPU_WRITE( time, offset, addr, data )\
	emulator->WriteByte( addr, data )

#define READ(  time, addr )                 CPU_READ ( rel_time, time, (addr) )
#define WRITE( time, addr, data )           CPU_WRITE( rel_time, time, (addr), (data) )

#define DP_ADDR( addr )                     (dp + (addr))

#define READ_DP_TIMER(  time, addr, out )   { out = CPU_READ( rel_time, time, DP_ADDR( addr ) ); }
#define READ_DP(  time, addr )              READ ( time, DP_ADDR( addr ) )
#define WRITE_DP( time, addr, data )        WRITE( time, DP_ADDR( addr ), data )

#define READ_PROG16( addr, out )\
{\
	uint8_t low = READ( 0, addr );\
	uint8_t high = READ( 0, addr + 1 );\
	out = (high << 8) | low;\
}

#define READ_PC()           READ( 0, pc )
#define READ_PC16( out )    READ_PROG16( pc, out );

#define PUSH( data )\
{\
	WRITE(0, sp-- + 0x100, data); \
}

#define POP( out )\
{\
	out = READ(0, ++sp + 0x100); \
}

#define PUSHADDR16( data )\
{\
	PUSH( data >> 8 ); \
	PUSH( data ); \
}

#define POPADDR16( out ) \
{\
	uint8_t low, high; \
	POP( low ); \
	POP( high ); \
	out = (high << 8) | low; \
}

#define MEM_BIT( rel ) CPU_mem_bit( pc )

unsigned SnesSmp::CPU_mem_bit( uint16_t pc )
{
	unsigned addr;
	READ_PC16( addr );
	unsigned t = READ( 0, addr & 0x1FFF ) >> (addr >> 13);
	return t << 8 & 0x100;
}

//// Status flag handling

#define GET_PSW( out )\
{\
	out = psw & ~(n80 | p20 | z02 | c01);\
	out |= c  >> 8 & c01;\
	out |= dp >> 3 & p20;\
	out |= ((nz >> 4) | nz) & n80;\
	if ( !(uint8_t) nz ) out |= z02;\
}

#define SET_PSW( in )\
{\
	psw = in;\
	c   = in << 8;\
	dp  = in << 3 & 0x100;\
	nz  = (in << 4 & 0x800) | (~in & z02);\
}

//// LAWL

#define CYCLES( count )\
{\
	emulator->CpuCyclesCallback( count );\
	targetCycles -= (count);\
}

int SnesSmp::Run(int targetCycles)
{
{
	uint8_t a = regs.a;
	uint8_t x = regs.x;
	uint8_t y = regs.y;
	uint16_t pc;
	uint8_t sp;
	int psw;
	int c;
	int nz;
	int dp;
	
	pc = regs.pc;
	sp = regs.sp;
	SET_PSW( regs.psw );
	
	goto loop;
	
	
	// Main loop
	unsigned opcode;
	
cbranch_taken_loop:
	pc += (BOOST::int8_t)READ_PC();
inc_pc_loop:
	pc++;
loop:
{
	// Uncache registers
	regs.pc = (uint16_t) pc;
	regs.sp = ( uint8_t) sp;
	regs.a  = ( uint8_t) a;
	regs.x  = ( uint8_t) x;
	regs.y  = ( uint8_t) y;
	{
		int temp;
		GET_PSW( temp );
		regs.psw = (uint8_t) temp;
	}

	unsigned data;
	
	opcode = READ_PC();
	CYCLES( cycle_table [opcode] );
	if ( targetCycles < 0 )
		goto out_of_time;
	
	pc++;
	data = READ_PC();
	switch ( opcode )
	{
	
// Common instructions

#define BRANCH( cond )\
{\
	pc++;\
	pc += (BOOST::int8_t) data;\
	if ( cond )\
		goto loop;\
	pc -= (BOOST::int8_t) data;\
	CYCLES( -2 );\
	goto loop;\
}

	case 0xF0: // BEQ
		BRANCH( !(uint8_t) nz ) // 89% taken
	
	case 0xD0: // BNE
		BRANCH( (uint8_t) nz )
	
	case 0x3F:{// CALL
		int old_addr = pc + 2;
		READ_PC16( pc );
		PUSHADDR16( old_addr );
		goto loop;
	}
	
	case 0x6F:// RET
		{
			int addr;
			POPADDR16( addr );
			pc = addr;
		}
		goto loop;
	
	case 0xE4: // MOV a,dp
		++pc;
		// 80% from timer
		READ_DP_TIMER( 0, data, a = nz );
		goto loop;
	
	case 0xFA:{// MOV dp,dp
		int temp;
		READ_DP_TIMER( -2, data, temp );
		data = temp + no_read_before_write ;
	}
	// fall through
	case 0x8F:{// MOV dp,#imm
		pc++;
		int temp = READ_PC();
		pc++;
		
		{
			int i = dp + temp;
			WRITE( 0, i, (uint8_t) data );
		}
		goto loop;
	}
	
	case 0xC4: // MOV dp,a
		++pc;
		{
			int i = dp + data;
			WRITE( 0, i, a );
		}
		goto loop;
	
#define CASE( n )   case n:

// Define common address modes based on opcode for immediate mode. Execution
// ends with data set to the address of the operand.
#define ADDR_MODES_( op )\
	CASE( op - 0x02 ) /* (X) */\
		data = x + dp;\
		pc--;\
		goto end_##op;\
	CASE( op + 0x0F ) /* (dp)+Y */\
		READ_PROG16( data + dp, data );\
		data += y;\
		goto end_##op;\
	CASE( op - 0x01 ) /* (dp+X) */\
		READ_PROG16( ((uint8_t) (data + x)) + dp, data );\
		goto end_##op;\
	CASE( op + 0x0E ) /* abs+Y */\
		data += y;\
		goto abs_##op;\
	CASE( op + 0x0D ) /* abs+X */\
		data += x;\
	CASE( op - 0x03 ) /* abs */\
	abs_##op:\
		pc++;\
		data += 0x100 * READ_PC();\
		goto end_##op;\
	CASE( op + 0x0C ) /* dp+X */\
		data = (uint8_t) (data + x);

#define ADDR_MODES_NO_DP( op )\
	ADDR_MODES_( op )\
		data += dp;\
	end_##op:

#define ADDR_MODES( op )\
	ADDR_MODES_( op )\
	CASE( op - 0x04 ) /* dp */\
		data += dp;\
	end_##op:

// 1. 8-bit Data Transmission Commands. Group I

	ADDR_MODES_NO_DP( 0xE8 ) // MOV A,addr
		a = nz = READ( 0, data );
		goto inc_pc_loop;
	
	case 0xBF:{// MOV A,(X)+
		int temp = x + dp;
		x = (uint8_t) (x + 1);
		a = nz = READ( -1, temp );
		goto loop;
	}
	
	case 0xE8: // MOV A,imm
		a  = data;
		nz = data;
		goto inc_pc_loop;
	
	case 0xF9: // MOV X,dp+Y
		data = (uint8_t) (data + y);
	case 0xF8: // MOV X,dp
		READ_DP_TIMER( 0, data, x = nz );
		goto inc_pc_loop;
	
	case 0xE9: // MOV X,abs
		READ_PC16( data );
		++pc;
		data = READ( 0, data );
	case 0xCD: // MOV X,imm
		x  = data;
		nz = data;
		goto inc_pc_loop;
	
	case 0xFB: // MOV Y,dp+X
		data = (uint8_t) (data + x);
	case 0xEB: // MOV Y,dp
		// 70% from timer
		pc++;
		READ_DP_TIMER( 0, data, y = nz );
		goto loop;
	
	case 0xEC:{// MOV Y,abs
		int temp;
		READ_PC16( temp );
		pc += 2;
		y = nz = READ( 0, temp );
		//y = nz = READ( 0, temp );
		goto loop;
	}
	
	case 0x8D: // MOV Y,imm
		y  = data;
		nz = data;
		goto inc_pc_loop;
	
// 2. 8-BIT DATA TRANSMISSION COMMANDS, GROUP 2

	ADDR_MODES_NO_DP( 0xC8 ) // MOV addr,A
		WRITE( 0, data, a );
		goto inc_pc_loop;
	
	{
		int temp;
	case 0xCC: // MOV abs,Y
		temp = y;
		goto mov_abs_temp;
	case 0xC9: // MOV abs,X
		temp = x;
mov_abs_temp:
		{
			int temp2;
			READ_PC16( temp2 );
			WRITE( 0, temp2, temp );
		}
		pc += 2;
		goto loop;
	}
	
	case 0xD9: // MOV dp+Y,X
		data = (uint8_t) (data + y);
	case 0xD8: // MOV dp,X
		WRITE( 0, data + dp, x );
		goto inc_pc_loop;
	
	case 0xDB: // MOV dp+X,Y
		data = (uint8_t) (data + x);
	case 0xCB: // MOV dp,Y
		WRITE( 0, data + dp, y );
		goto inc_pc_loop;

// 3. 8-BIT DATA TRANSMISSIN COMMANDS, GROUP 3.
	
	case 0x7D: // MOV A,X
		a  = x;
		nz = x;
		goto loop;
	
	case 0xDD: // MOV A,Y
		a  = y;
		nz = y;
		goto loop;
	
	case 0x5D: // MOV X,A
		x  = a;
		nz = a;
		goto loop;
	
	case 0xFD: // MOV Y,A
		y  = a;
		nz = a;
		goto loop;
	
	case 0x9D: // MOV X,SP
		x = nz = sp;
		goto loop;
	
	case 0xBD: // MOV SP,X
		sp = x;
		goto loop;
	
	//case 0xC6: // MOV (X),A (handled by MOV addr,A in group 2)
	
	case 0xAF: // MOV (X)+,A
		WRITE_DP( 0, x, a + no_read_before_write  );
		x++;
		goto loop;
	
// 5. 8-BIT LOGIC OPERATION COMMANDS
	
#define LOGICAL_OP( op, func )\
	ADDR_MODES( op ) /* addr */\
		data = READ( 0, data );\
	case op: /* imm */\
		nz = a func##= data;\
		goto inc_pc_loop;\
	{   unsigned addr;\
	case op + 0x11: /* X,Y */\
		data = READ_DP( -2, y );\
		addr = x + dp;\
		goto addr_##op;\
	case op + 0x01: /* dp,dp */\
		data = READ_DP( -3, data );\
	case op + 0x10:{/*dp,imm*/\
		pc++;\
		uint8_t addr2 = READ_PC();\
		pc++;\
		addr = addr2 + dp;\
	}\
	addr_##op:\
		nz = data func READ( -1, addr );\
		WRITE( 0, addr, nz );\
		goto loop;\
	}
	
	LOGICAL_OP( 0x28, & ); // AND
	
	LOGICAL_OP( 0x08, | ); // OR
	
	LOGICAL_OP( 0x48, ^ ); // EOR
	
// 4. 8-BIT ARITHMETIC OPERATION COMMANDS

	ADDR_MODES( 0x68 ) // CMP addr
		data = READ( 0, data );
	case 0x68: // CMP imm
		nz = a - data;
		c = ~nz;
		nz &= 0xFF;
		goto inc_pc_loop;
	
	case 0x79: // CMP (X),(Y)
		data = READ_DP( -2, y );
		nz = READ_DP( -1, x ) - data;
		c = ~nz;
		nz &= 0xFF;
		goto loop;
	
	case 0x69: // CMP dp,dp
		data = READ_DP( -3, data );
	case 0x78: // CMP dp,imm
		pc++;
		nz = READ_DP( -1, READ_PC() ) - data;
		c = ~nz;
		nz &= 0xFF;
		goto inc_pc_loop;
	
	case 0x3E: // CMP X,dp
		data += dp;
		goto cmp_x_addr;
	case 0x1E: // CMP X,abs
		READ_PC16( data );
		pc++;
	cmp_x_addr:
		data = READ( 0, data );
	case 0xC8: // CMP X,imm
		nz = x - data;
		c = ~nz;
		nz &= 0xFF;
		goto inc_pc_loop;
	
	case 0x7E: // CMP Y,dp
		data += dp;
		goto cmp_y_addr;
	case 0x5E: // CMP Y,abs
		READ_PC16( data );
		pc++;
	cmp_y_addr:
		data = READ( 0, data );
	case 0xAD: // CMP Y,imm
		nz = y - data;
		c = ~nz;
		nz &= 0xFF;
		goto inc_pc_loop;
	
	{
		int addr;
	case 0xB9: // SBC (x),(y)
	case 0x99: // ADC (x),(y)
		pc--; // compensate for inc later
		data = READ_DP( -2, y );
		addr = x + dp;
		goto adc_addr;
	case 0xA9: // SBC dp,dp
	case 0x89: // ADC dp,dp
		data = READ_DP( -3, data );
	case 0xB8: // SBC dp,imm
	case 0x98: // ADC dp,imm
		pc++;
		addr = READ_PC() + dp;
	adc_addr:
		nz = READ( -1, addr );
		goto adc_data;
		
// catch ADC and SBC together, then decode later based on operand
#undef CASE
#define CASE( n ) case n: case (n) + 0x20:
	ADDR_MODES( 0x88 ) // ADC/SBC addr
		data = READ( 0, data );
	case 0xA8: // SBC imm
	case 0x88: // ADC imm
		addr = -1; // A
		nz = a;
	adc_data: {
		int flags;
		if ( opcode >= 0xA0 ) // SBC
			data ^= 0xFF;
		
		flags = data ^ nz;
		nz += data + (c >> 8 & 1);
		flags ^= nz;
		
		psw = (psw & ~(v40 | h08)) |
				(flags >> 1 & h08) |
				((flags + 0x80) >> 2 & v40);
		c = nz;
		if ( addr < 0 )
		{
			a = (uint8_t) nz;
			goto inc_pc_loop;
		}
		WRITE( 0, addr, /*(uint8_t)*/ nz );
		goto inc_pc_loop;
	}
	
	}
	
// 6. ADDITION & SUBTRACTION COMMANDS

#define INC_DEC_REG( reg, op )\
		nz  = reg op;\
		reg = (uint8_t) nz;\
		goto loop;

	case 0xBC: INC_DEC_REG( a, + 1 ) // INC A
	case 0x3D: INC_DEC_REG( x, + 1 ) // INC X
	case 0xFC: INC_DEC_REG( y, + 1 ) // INC Y
	
	case 0x9C: INC_DEC_REG( a, - 1 ) // DEC A
	case 0x1D: INC_DEC_REG( x, - 1 ) // DEC X
	case 0xDC: INC_DEC_REG( y, - 1 ) // DEC Y

	case 0x9B: // DEC dp+X
	case 0xBB: // INC dp+X
		data = (uint8_t) (data + x);
	case 0x8B: // DEC dp
	case 0xAB: // INC dp
		data += dp;
		goto inc_abs;
	case 0x8C: // DEC abs
	case 0xAC: // INC abs
		READ_PC16( data );
		pc++;
	inc_abs:
		nz = (opcode >> 4 & 2) - 1;
		nz += READ( -1, data );
		WRITE( 0, data, /*(uint8_t)*/ nz );
		goto inc_pc_loop;
	
// 7. SHIFT, ROTATION COMMANDS

	case 0x5C: // LSR A
		c = 0;
	case 0x7C:{// ROR A
		nz = (c >> 1 & 0x80) | (a >> 1);
		c = a << 8;
		a = nz;
		goto loop;
	}
	
	case 0x1C: // ASL A
		c = 0;
	case 0x3C:{// ROL A
		int temp = c >> 8 & 1;
		c = a << 1;
		nz = c | temp;
		a = (uint8_t) nz;
		goto loop;
	}
	
	case 0x0B: // ASL dp
		c = 0;
		data += dp;
		goto rol_mem;
	case 0x1B: // ASL dp+X
		c = 0;
	case 0x3B: // ROL dp+X
		data = (uint8_t) (data + x);
	case 0x2B: // ROL dp
		data += dp;
		goto rol_mem;
	case 0x0C: // ASL abs
		c = 0;
	case 0x2C: // ROL abs
		READ_PC16( data );
		pc++;
	rol_mem:
		nz = c >> 8 & 1;
		nz |= (c = READ( -1, data ) << 1);
		WRITE( 0, data, /*(uint8_t)*/ nz );
		goto inc_pc_loop;
	
	case 0x4B: // LSR dp
		c = 0;
		data += dp;
		goto ror_mem;
	case 0x5B: // LSR dp+X
		c = 0;
	case 0x7B: // ROR dp+X
		data = (uint8_t) (data + x);
	case 0x6B: // ROR dp
		data += dp;
		goto ror_mem;
	case 0x4C: // LSR abs
		c = 0;
	case 0x6C: // ROR abs
		READ_PC16( data );
		pc++;
	ror_mem: {
		int temp = READ( -1, data );
		nz = (c >> 1 & 0x80) | (temp >> 1);
		c = temp << 8;
		WRITE( 0, data, nz );
		goto inc_pc_loop;
	}

	case 0x9F: // XCN
		nz = a = (a >> 4) | (uint8_t) (a << 4);
		goto loop;

// 8. 16-BIT TRANSMISION COMMANDS

	case 0xBA: // MOVW YA,dp
		a = READ_DP( -2, data );
		nz = (a & 0x7F) | (a >> 1);
		y = READ_DP( 0, (uint8_t) (data + 1) );
		nz |= y;
		goto inc_pc_loop;
	
	case 0xDA: // MOVW dp,YA
		WRITE_DP( -1, data, a );
		WRITE_DP( 0, (uint8_t) (data + 1), y + no_read_before_write  );
		goto inc_pc_loop;
	
// 9. 16-BIT OPERATION COMMANDS

	case 0x3A: // INCW dp
	case 0x1A:{// DECW dp
		int temp;
		// low byte
		data += dp;
		temp = READ( -3, data );
		temp += (opcode >> 4 & 2) - 1; // +1 for INCW, -1 for DECW
		nz = ((temp >> 1) | temp) & 0x7F;
		WRITE( -2, data, /*(uint8_t)*/ temp );
		
		// high byte
		data = (uint8_t) (data + 1) + dp;
		temp = (uint8_t) ((temp >> 8) + READ( -1, data ));
		nz |= temp;
		WRITE( 0, data, temp );
		
		goto inc_pc_loop;
	}
		
	case 0x7A: // ADDW YA,dp
	case 0x9A:{// SUBW YA,dp
		int lo = READ_DP( -2, data );
		int hi = READ_DP( 0, (uint8_t) (data + 1) );
		int result;
		int flags;
		
		if ( opcode == 0x9A ) // SUBW
		{
			lo = (lo ^ 0xFF) + 1;
			hi ^= 0xFF;
		}
		
		lo += a;
		result = y + hi + (lo >> 8);
		flags = hi ^ y ^ result;
		
		psw = (psw & ~(v40 | h08)) |
				(flags >> 1 & h08) |
				((flags + 0x80) >> 2 & v40);
		c = result;
		a = (uint8_t) lo;
		result = (uint8_t) result;
		y = result;
		nz = (((lo >> 1) | lo) & 0x7F) | result;
		
		goto inc_pc_loop;
	}
	
	case 0x5A: { // CMPW YA,dp
		int temp = a - READ_DP( -1, data );
		nz = ((temp >> 1) | temp) & 0x7F;
		temp = y + (temp >> 8);
		temp -= READ_DP( 0, (uint8_t) (data + 1) );
		nz |= temp;
		c  = ~temp;
		nz &= 0xFF;
		goto inc_pc_loop;
	}
	
// 10. MULTIPLICATION & DIVISON COMMANDS

	case 0xCF: { // MUL YA
		unsigned temp = y * a;
		a = (uint8_t) temp;
		nz = ((temp >> 1) | temp) & 0x7F;
		y = temp >> 8;
		nz |= y;
		goto loop;
	}
	
	case 0x9E: // DIV YA,X
	{
		unsigned ya = y * 0x100 + a;
		
		psw &= ~(h08 | v40);
		
		if ( y >= x )
			psw |= v40;
		
		if ( (y & 15) >= (x & 15) )
			psw |= h08;
		
		if ( y < x * 2 )
		{
			a = ya / x;
			y = ya - a * x;
		}
		else
		{
			a = 255 - (ya - x * 0x200) / (256 - x);
			y = x   + (ya - x * 0x200) % (256 - x);
		}
		
		nz = (uint8_t) a;
		a = (uint8_t) a;
		
		goto loop;
	}
	
// 11. DECIMAL COMPENSATION COMMANDS
	
	case 0xDF: // DAA
		SUSPICIOUS_OPCODE( "DAA" );
		if ( a > 0x99 || c & 0x100 )
		{
			a += 0x60;
			c = 0x100;
		}
		
		if ( (a & 0x0F) > 9 || psw & h08 )
			a += 0x06;
		
		nz = a;
		a = (uint8_t) a;
		goto loop;
	
	case 0xBE: // DAS
		SUSPICIOUS_OPCODE( "DAS" );
		if ( a > 0x99 || !(c & 0x100) )
		{
			a -= 0x60;
			c = 0;
		}
		
		if ( (a & 0x0F) > 9 || !(psw & h08) )
			a -= 0x06;
		
		nz = a;
		a = (uint8_t) a;
		goto loop;
	
// 12. BRANCHING COMMANDS

	case 0x2F: // BRA rel
		pc += (BOOST::int8_t) data;
		goto inc_pc_loop;
	
	case 0x30: // BMI
		BRANCH( (nz & nz_neg_mask) )
	
	case 0x10: // BPL
		BRANCH( !(nz & nz_neg_mask) )
	
	case 0xB0: // BCS
		BRANCH( c & 0x100 )
	
	case 0x90: // BCC
		BRANCH( !(c & 0x100) )
	
	case 0x70: // BVS
		BRANCH( psw & v40 )
	
	case 0x50: // BVC
		BRANCH( !(psw & v40) )
	
	#define CBRANCH( cond )\
	{\
		pc++;\
		if ( cond )\
			goto cbranch_taken_loop;\
		CYCLES( -2 );\
		goto inc_pc_loop;\
	}
	
	case 0x03: // BBS dp.bit,rel
	case 0x23:
	case 0x43:
	case 0x63:
	case 0x83:
	case 0xA3:
	case 0xC3:
	case 0xE3:
		CBRANCH( READ_DP( -4, data ) >> (opcode >> 5) & 1 )
	
	case 0x13: // BBC dp.bit,rel
	case 0x33:
	case 0x53:
	case 0x73:
	case 0x93:
	case 0xB3:
	case 0xD3:
	case 0xF3:
		CBRANCH( !(READ_DP( -4, data ) >> (opcode >> 5) & 1) )
	
	case 0xDE: // CBNE dp+X,rel
		data = (uint8_t) (data + x);
		// fall through
	case 0x2E:{// CBNE dp,rel
		int temp;
		// 61% from timer
		READ_DP_TIMER( -4, data, temp );
		CBRANCH( temp != a )
	}
	
	case 0x6E: { // DBNZ dp,rel
		unsigned temp = READ_DP( -4, data ) - 1;
		WRITE_DP( -3, (uint8_t) data, /*(uint8_t)*/ temp + no_read_before_write  );
		CBRANCH( temp )
	}
	
	case 0xFE: // DBNZ Y,rel
		y = (uint8_t) (y - 1);
		BRANCH( y )
	
	case 0x1F: // JMP [abs+X]
		READ_PC16( pc );
		pc += x;
		// fall through
	case 0x5F: // JMP abs
		READ_PC16( pc );
		goto loop;
	
// 13. SUB-ROUTINE CALL RETURN COMMANDS
	
	case 0x0F:{// BRK
		int temp;
		int ret_addr = pc;
		SUSPICIOUS_OPCODE( "BRK" );
		READ_PROG16( 0xFFDE, pc ); // vector address verified
		PUSHADDR16( ret_addr );
		GET_PSW( temp );
		psw = (psw | b10) & ~i04;
		PUSH( temp );
		goto loop;
	}
	
	case 0x4F:{// PCALL offset
		int ret_addr = pc + 1;
		pc = 0xFF00 | data;
		PUSHADDR16( ret_addr );
		goto loop;
	}
	
	case 0x01: // TCALL n
	case 0x11:
	case 0x21:
	case 0x31:
	case 0x41:
	case 0x51:
	case 0x61:
	case 0x71:
	case 0x81:
	case 0x91:
	case 0xA1:
	case 0xB1:
	case 0xC1:
	case 0xD1:
	case 0xE1:
	case 0xF1: {
		int ret_addr = pc;
		READ_PROG16( 0xFFDE - (opcode >> 3), pc );
		PUSHADDR16( ret_addr );
		goto loop;
	}
	
// 14. STACK OPERATION COMMANDS

	{
		int temp;
	case 0x7F: // RET1
		POP( temp );
		{
			int addr;
			POPADDR16( addr );
			pc = addr;
		}
		SET_PSW( temp );
		goto loop;

	case 0x8E: // POP PSW
		POP( temp );
		SET_PSW( temp );
		goto loop;
	}
	
	case 0x0D: { // PUSH PSW
		int temp;
		GET_PSW( temp );
		PUSH( temp );
		goto loop;
	}

	case 0x2D: // PUSH A
		PUSH( a );
		goto loop;
	
	case 0x4D: // PUSH X
		PUSH( x );
		goto loop;
	
	case 0x6D: // PUSH Y
		PUSH( y );
		goto loop;
	
	case 0xAE: // POP A
		POP( a );
		goto loop;
	
	case 0xCE: // POP X
		POP( x );
		goto loop;
	
	case 0xEE: // POP Y
		POP( y );
		goto loop;
	
// 15. BIT OPERATION COMMANDS

	case 0x02: // SET1
	case 0x22:
	case 0x42:
	case 0x62:
	case 0x82:
	case 0xA2:
	case 0xC2:
	case 0xE2:
	case 0x12: // CLR1
	case 0x32:
	case 0x52:
	case 0x72:
	case 0x92:
	case 0xB2:
	case 0xD2:
	case 0xF2: {
		int bit = 1 << (opcode >> 5);
		int mask = ~bit;
		if ( opcode & 0x10 )
			bit = 0;
		data += dp;
		WRITE( 0, data, (READ( -1, data ) & mask) | bit );
		goto inc_pc_loop;
	}
		
	case 0x0E: // TSET1 abs
	case 0x4E: // TCLR1 abs
		READ_PC16( data );
		pc += 2;
		{
			unsigned temp = READ( -2, data );
			nz = (uint8_t) (a - temp);
			temp &= ~a;
			if ( opcode == 0x0E )
				temp |= a;
			WRITE( 0, data, temp );
		}
		goto loop;
	
	case 0x4A: // AND1 C,mem.bit
		c &= MEM_BIT( 0 );
		pc += 2;
		goto loop;
	
	case 0x6A: // AND1 C,/mem.bit
		c &= ~MEM_BIT( 0 );
		pc += 2;
		goto loop;
	
	case 0x0A: // OR1 C,mem.bit
		c |= MEM_BIT( -1 );
		pc += 2;
		goto loop;
	
	case 0x2A: // OR1 C,/mem.bit
		c |= ~MEM_BIT( -1 );
		pc += 2;
		goto loop;
	
	case 0x8A: // EOR1 C,mem.bit
		c ^= MEM_BIT( -1 );
		pc += 2;
		goto loop;
	
	case 0xEA: // NOT1 mem.bit
		READ_PC16( data );
		pc += 2;
		{
			unsigned temp = READ( -1, data & 0x1FFF );
			temp ^= 1 << (data >> 13);
			WRITE( 0, data & 0x1FFF, temp );
		}
		goto loop;
	
	case 0xCA: // MOV1 mem.bit,C
		READ_PC16( data );
		pc += 2;
		{
			unsigned temp = READ( -2, data & 0x1FFF );
			unsigned bit = data >> 13;
			temp = (temp & ~(1 << bit)) | ((c >> 8 & 1) << bit);
			WRITE( 0, data & 0x1FFF, temp + no_read_before_write  );
		}
		goto loop;
	
	case 0xAA: // MOV1 C,mem.bit
		c = MEM_BIT( 0 );
		pc += 2;
		goto loop;
	
// 16. PROGRAM PSW FLAG OPERATION COMMANDS

	case 0x60: // CLRC
		c = 0;
		goto loop;
		
	case 0x80: // SETC
		c = ~0;
		goto loop;
	
	case 0xED: // NOTC
		c ^= 0x100;
		goto loop;
		
	case 0xE0: // CLRV
		psw &= ~(v40 | h08);
		goto loop;
	
	case 0x20: // CLRP
		dp = 0;
		goto loop;
	
	case 0x40: // SETP
		dp = 0x100;
		goto loop;
	
	case 0xA0: // EI
		SUSPICIOUS_OPCODE( "EI" );
		psw |= i04;
		goto loop;
	
	case 0xC0: // DI
		SUSPICIOUS_OPCODE( "DI" );
		psw &= ~i04;
		goto loop;
	
// 17. OTHER COMMANDS

	case 0x00: // NOP
		goto loop;
	
	case 0xFF:{// STOP
		// handle PC wrap-around
		unsigned addr = pc - 1;
		if ( addr >= 0x10000 )
		{
			addr &= 0xFFFF;
			pc = addr;
			dprintf( "SPC: PC wrapped around\n" );
			goto loop;
		}
	}
	// fall through
	case 0xEF: // SLEEP
		SUSPICIOUS_OPCODE( "STOP/SLEEP" );
		--pc;
		throw FSL_EXCEPTION("stop/sleep was hit");
		goto stop;
	} // switch
	
	assert( 0 );
}   
out_of_time:
	CYCLES( -cycle_table [opcode] ); // undo partial execution of opcode
stop:
	
	// Uncache registers
	regs.pc = (uint16_t) pc;
	regs.sp = ( uint8_t) sp;
	regs.a  = ( uint8_t) a;
	regs.x  = ( uint8_t) x;
	regs.y  = ( uint8_t) y;
	{
		int temp;
		GET_PSW( temp );
		regs.psw = (uint8_t) temp;
	}
}

return targetCycles;
}
