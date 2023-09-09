#pragma once

#include <cstdint>
#include <fstream>
#include <memory>
#include <pthread.h>
#include <stdexcept>
#include <stdint.h>
#include <algorithm>
#include <cassert>
#include <iostream>
#include <functional>
#include <sys/types.h>

#include "consts.h"

#include "binTrie.h"


class IO
{
  std::bitset<4> Byte2BCD();
};

// @BLOAT I don't understarnd but it works without it
typedef struct MemWordPos{
  private:
  Word& littleEndianMem; // lest significant byte in littleEndin, THE LEFT ONE
  public:
  MemWordPos(Byte& less):littleEndianMem{(Word&)less}{}
  
  template<class T> T operator=(T) = delete;
  Word operator=(Word w){ // unpack bigEndian to LittleEndian
    // littleEndianMem = w<<8; // this overwrites the entire Word
    // littleEndianMem|= w>>8; // this to above
    littleEndianMem = w;
    return w;
  }
  operator Word() const{ // convertion operator
    return littleEndianMem;
    // Word res; 
    // res = littleEndianMem<<8; // this overwrites the entire Word
    // res|= littleEndianMem>>8;   // this to above
    // return res;
  }
} MemWordPos;

struct CPU;

class OpcodeTrie : public BitTrie<std::function<void(CPU*, Word opcode)>>
{
public:
  void call(CPU& cpu, Word opcode)
  {
    auto ins = find<8>({opcode});
    if(!ins) 
      UNDEFINED(opcode);
    else     
      std::__invoke(*ins, &cpu, opcode);
  }

  

public:
  [[deprecated("not implemented yet")]]
  void addGeneral(const char opcode[8])
  {
    // add all possibilities from `0011***1`:
    // 00110001
    // 00110011
    // 00110101
    // 00110111
    // ...
    // 00111111
    
    for(int i=0;i<8;i++)
    {
      // if(opcode[i] == '*')
       
    }
  }

  void UNDEFINED(std::bitset<8> op){
    #ifndef UNDEFINED_INS_AS_NOP
    throw std::invalid_argument("[INSTRUCTION SET] Loaded undefined opcode {"+op.to_string()+"}");
    #endif
  }
  
};

class Mem
{
public:
  static constexpr uint32_t MAX_MEM = 1024 * 1024; // 2^20 1MB
  Byte Data[MAX_MEM]{};

  void Reset(){
    std::fill(Data, Data+MAX_MEM-1, 0);
  }

  Byte& operator[](uint32_t i)
  {
    if(i>=MAX_MEM) throw std::out_of_range("[MEMORY] Invalid memory address");
    return Data[i];
  }

  Byte& getByte(uint32_t i)
  {
    return Data[i];
  }


private:
  // 

public:
  Word& getWord(uint32_t i)
  {
    if(i>=MAX_MEM) throw std::out_of_range("[MEMORY] Invalid memory address");
    return reinterpret_cast<Word&>(Data[i]);
  }

  void Load(std::ifstream& file, uint32_t offset=0){
    if(offset>=MAX_MEM) throw std::out_of_range("[MEMORY] Invalid memory address");
    if(!file.is_open()) throw std::invalid_argument("[MEMORY] Invalid binary file provided");
    file.read(reinterpret_cast<char*>(&(Data[offset])), MAX_MEM - offset);
  }
};

class CPU
{
public:
  Mem& mem;
  OpcodeTrie opcodeInstructions{};
  
  // Data Registers
  union{struct{Byte AL; Byte AH;}; Word AX{};};       // Accumulator register
  union{struct{Byte BL; Byte BH;}; Word BX{};};       // Base register
  union{struct{Byte CL; Byte CH;}; Word CX{};};       // Counter register
  union{struct{Byte DL; Byte DH;}; Word DX{};};       // Data register

  // Address Registers
  Word SP{};    // Stack pointer
  Word BP{};    // Base pointer
  Word SI{};    // Source pointer
  Word DI{};    // Destination pointer
  Word IP{};    // Instruction pointer

  struct Flags_t{
    // Control Flags
    Bit   :4;
    Bit TF:1;   // Trap
    Bit DF:1;   // Direction
    Bit IF:1;   // Interrupt enable
    // Status Flags
    Bit OF:1;   // Overflow
    Bit SF:1;   // Sign
    Bit ZF:1;   // Zero
    Bit   :1;
    Bit AF:1;   // Auxiliry Carry
    Bit   :1;
    Bit PF:1;   // Parity
    Bit   :1;
    Bit CF:1;   // Carry 

    // xoring each bit to the least significant, and then test it
    bool getWordParity(Word n);
    bool getByteParity(Byte n);
    // OF SF ZF AF PF CF
    Word mark_WordAdd(Word a, Word b, Bit c=0);
    template<class T> Byte mark_WordAdd(T, T, Bit=0) = delete;
    
    Word mark_WordSub(Word a, Word b, Bit c=0);
    
    Byte mark_ByteAdd(Byte a, Byte b, Bit c=0);
    template<class T> Byte mark_ByteAdd(T,T,Bit=0) = delete;

    Byte mark_ByteSub(Byte a, Byte b, Bit c=0);
    
    Word mark_WordInc(Word a, Word c=1);
    // Same as Add but without CF
    Byte mark_ByteInc(Byte a, Byte c=1);
  };
  
  union{
    Word flagsWord{};
    Flags_t Flags;
  };
  // Segment Registers
  Word CS{};    // Code Segment
  Word DS{};    // Data Segment
  Word SS{};    // Stack Segment
  Word ES{};    // Extra Segment
  
  CPU(Mem& memory):mem(memory)
  {
    // Reset to startup state
    Reset();

    // bind functions to upcodes

    // 4 bit
    opcodeInstructions.add<4>({
      {0b1011, &CPU::MOV_reg_imm},  
      {0b101000, &CPU::MOV_ac_mem}
    });
    // 5 bit
    opcodeInstructions.add<5>({
      {0b0101'0, &CPU::PUSH_reg},
      {0b0101'1, &CPU::POP_reg},       
      {0b1001'0, &CPU::XCHG_ac_reg},
      {0b0100'0, &CPU::INC_reg16},
      {0b0100'1, &CPU::DEC_reg16},
    });
    // 6 bit
    opcodeInstructions.add<6>({
      {0b1100'01, &CPU::MOVS},
      {0b1000'11, &CPU::MOV_seg},
      {0b1000'10, &CPU::MOV_reg},
      {0b1001'11, &CPU::PUSHF},
      {0b0000'00, &CPU::ADD_memORreg_reg},
      {0b1000'00, &CPU::ADD_ADC_SUB_SBB_memORreg_imm},//
      {0b0001'00, &CPU::ADC_memORreg_reg},
      {0b0010'10, &CPU::SUB_memORreg_reg},
      {0b0001'10, &CPU::SBB_memORreg_reg},
      {0b0011'10, &CPU::CMP_memregORregmem},
    });

    opcodeInstructions.add<7>({
      {0b1000'011, &CPU::XCHG_reg_memORreg},
      {0b1110'110, &CPU::IN},
      {0b1110'010, &CPU::IN_addr},
      {0b1110'111, &CPU::OUT},
      {0b1110'011, &CPU::OUT_addr},
      {0b0000'010, &CPU::ADD_ac_imm},
      {0b0001'010, &CPU::ADC_ac_imm},
      {0b1111'111, &CPU::INC_DEC_memORreg8},
      {0b1000'110, &CPU::SUB_ac_imm},
      {0b0001'110, &CPU::SBB_ac_imm},
      {0b1111'011, &CPU::NEG_regORmem},
      {0b0011'110, &CPU::CMP_ac_imm},
    });
    
    opcodeInstructions.add<8>({
      {0b1111'1111, &CPU::PUSH_mem},
      // PUSH ES, CS, SS, DS
      {0b000'00'000, &CPU::PUSH_ES},
      {0b000'01'000, &CPU::PUSH_CS},
      {0b000'10'000, &CPU::PUSH_SS},
      {0b000'11'000, &CPU::PUSH_DS},
      // POP ES, CS, SS, DS
      {0b000'00'111, &CPU::POP_ES},
      {0b000'01'111, &CPU::POP_CS},
      {0b000'10'111, &CPU::POP_SS},
      {0b000'11'111, &CPU::POP_DS},
      {0b1101'0111, &CPU::XLAT},
      {0b1000'1101, &CPU::LEA},
      {0b1100'0101, &CPU::LDS},
      {0b1100'0100, &CPU::LES},
      {0b1001'1111, &CPU::LAHF},
      {0b1001'1110, &CPU::SAHF},
      {0b1001'1100, &CPU::PUSHF},
      {0b1001'1101, &CPU::POPF},
      {0b0011'0111, &CPU::AAA},
      {0b0010'0111, &CPU::DAA},

    });

  }

  /******************
  * DATA MANIPULATION
  ******************/
  
  // Moves
  void MOVS(Word);
  void MOV_seg(Word);
  void MOV_reg(Word);
  void MOV_ac_mem(Word);
  void MOV_reg_imm(Word);

  // pushes
  void PUSH_reg(Word);
  void PUSH_mem(Word);
  void PUSH_ES(Word); 
  void PUSH_CS(Word); 
  void PUSH_SS(Word);
  void PUSH_DS(Word); 
  void PUSHF(Word);

  // popes
  void POP_reg(Word);
  void POP_mem(Word);
  void POP_ES(Word); 
  void POP_CS(Word); 
  void POP_SS(Word);
  void POP_DS(Word); 
  void POPF(Word);

  // XCHG (exchage byte or word)
  void XCHG_ac_reg(Word);
  void XCHG_reg_memORreg(Word);

  // XLAT (index a table in memory)
  void XLAT(Word);

  // IN (load AL/AX from IO on addr from DX or imm)
  void IN(Word);
  void IN_addr(Word);
  // OUT (output AL/AX to IO on addr from DX or imm)
  void OUT(Word);
  void OUT_addr(Word);

  // LEA (load effective address)
  void LEA(Word);
  // LDS (load data segment register)
  void LDS(Word);
  // LES (load extra segment register)
  void LES(Word);

  // LAHF (load AH register from flags)
  void LAHF(Word);
  // SAHF (store AH reg to frags)
  void SAHF(Word);

  /***********
  * ARTHMETIC
  ***********/
  void ADD_memORreg_reg(Word);
  void ADD_ac_imm(Word);
  void ADD_ADC_SUB_SBB_memORreg_imm(Word);
  // ACD (add word with carry flag)
  void ADC_memORreg_reg(Word);
  void ADC_ac_imm(Word);

  // INC
  void INC_reg16(Word);
  void INC_DEC_memORreg8(Word);

  // AAA (asci adjust for addition)
  void AAA(Word);

  // DAA (decimal adjust for addition)
  void DAA(Word);

  void SUB_memORreg_reg(Word);
  void SUB_ac_imm(Word);
  void SUB_memORreg_imm(Word);
  
  // SBB (subtract byte or word with borrow)
  void SBB_memORreg_reg(Word);
  void SBB_ac_imm(Word);
  void SBB_mem_imm(Word, Byte);

  // DEC (decrement)
  void DEC_reg16(Word);
  
  // LOGICAL

  // NEG
  void NEG_regORmem(Word);

  void CMP_memregORregmem(Word);
  void CMP_ac_imm(Word);
  void CMP_reg_mem(Word);
  void CMP_reg_imm(Word, Byte);
  void CMP_mem_imm(Word, Byte);
  
  void Reset()
  {
    CS = 0xFFFF;
    IP = 0x0000;
  }

  void PrintRegisters()
  {
    std::cout.setf(std::ios::hex, std::ios::basefield);
    std::cout.setf(std::ios::uppercase);
    std::cout << "AX: " << std::bitset<16>(AX) << ": 0x"<< AX << " (" << "AL:0x" << +AL << ", AH:0x" << +AH << ")" << '\n';
    std::cout << "BX: " << std::bitset<16>(BX) << ": 0x"<< BX << " (" << "BL:0x" << +BL << ", BH:0x" << +BH << ")" << '\n';
    std::cout << "CX: " << std::bitset<16>(CX) << ": 0x"<< CX << " (" << "CL:0x" << +CL << ", CH:0x" << +CH << ")" << '\n';
    std::cout << "DX: " << std::bitset<16>(DX) << ": 0x"<< DX << " (" << "DL:0x" << +DL << ", DH:0x" << +DH << ")" << '\n';
    
    std::cout << "SP: " << std::bitset<16>(SP) << ": 0x"<< SP << '\n';
    std::cout << "BP: " << std::bitset<16>(BP) << ": 0x"<< BP << '\n';
    std::cout << "SI: " << std::bitset<16>(SI) << ": 0x"<< SI << '\n';
    std::cout << "DI: " << std::bitset<16>(DI) << ": 0x"<< DI << '\n';
    std::cout << "IP: " << std::bitset<16>(IP) << ": 0x"<< IP << '\n';

    std::cout << "CS: " << std::bitset<16>(CS) << ": 0x"<< CS << '\n';    
    std::cout << "DS: " << std::bitset<16>(DS) << ": 0x"<< DS << '\n';    
    std::cout << "SS: " << std::bitset<16>(SS) << ": 0x"<< SS << '\n';    
    std::cout << "ES: " << std::bitset<16>(ES) << ": 0x"<< ES << std::endl;    
    std::cout.setf(std::ios::dec, std::ios::basefield);
    std::cout.unsetf(std::ios::uppercase);
  }

  Word& getSegReg(std::bitset<2> regCode)
  {
    switch(regCode.to_ulong())
    {
      case 0b00: return ES;
      case 0b01: return CS;
      case 0b10: return SS;
      case 0b11: return DS;
    }
    std::cerr << "[ERROR] invalid segment register code" << std::endl;
    return ES;
  }
  
  Word& getWordReg(std::bitset<3> regCode)
  {
    switch(regCode.to_ulong())
    {
      case 0b000:return AX;
      case 0b001:return CX;
      case 0b010:return DX;
      case 0b011:return BX;
      case 0b100:return SP;
      case 0b101:return BP;
      case 0b110:return SI;
      case 0b111:return DI;
    }
    std::cerr << "[ERROR] invalid register code" << std::endl;
    return AX;
  }
  
  Byte& getByteReg(std::bitset<3> regCode)
  {
    switch(regCode.to_ulong())
    {
      case 0b000:
        return AL;
      case 0b001:
        return CL;
      case 0b010:
        return DL;
      case 0b011:
        return BL;
      case 0b100:
        return AH;
      case 0b101:
        return CH;
      case 0b110:
        return DH;
      case 0b111:
        return BH;
    }
    std::cerr << "[ERROR] invalid register code" << std::endl;
    return AL;
  }

  void pushWord(Word word)
  {
    SP-=2;
    mem.getWord(physicalAddr(SS, SP)) = word;
  }
  Word popWord()
  {
    Word res = mem.getWord(physicalAddr(SS, SP));
    SP+=2;
    return res;
  }

  uint32_t physicalAddr(Word base, Word offset)
  {
    return (base << 4) + offset; 
  }
  template<class T>
  uint32_t physicalAddr(T,T)= delete;

  Word calcEffectiveAddr(const std::bitset<2> mod, const std::bitset<3> rm)
  {
    assert(mod.to_ulong() != 0b11 && "[ERROR] both arguments are registers, can't calculate address");
    
    uint32_t addr = 0;
    switch(rm.to_ulong())
    {
      case 0b000:
        addr = mem[BX] + mem[SI];
        break;
      case 0b001:
        addr = mem[BX] + mem[DI];
        break;
      case 0b010:
        addr = mem[BP] + mem[SI];
        break;
      case 0b011:
        addr = mem[BP] + mem[DI];
        break;
      case 0b100:
        addr = mem[SI];
        break;
      case 0b101:
        addr = mem[DI];
        break;
      case 0b110:
        if(mod == 0b00) 
        {
          addr = FetchWord();
          break;
        }
        addr = mem[BP];      
        break;
      case 0b111:
        addr = mem[BX];      
        break;
    }

    switch(mod.to_ulong())
    {
      case 0b01:        
        addr += FetchByte();
        break;
      case 0b10:
        addr += FetchWord();
        break;
    }
    return addr;
  }
  
  uint32_t calcAddr(std::bitset<2> mod, std::bitset<3> rm)
  {
    // segment register
    switch(rm.to_ulong())
    {
      case 0b000:
      case 0b001:
      case 0b100:
      case 0b101:
      case 0b111:
        return physicalAddr(DS, calcEffectiveAddr(mod, rm));
      case 0b010:
      case 0b011:
      case 0b110:
        return physicalAddr(SS, calcEffectiveAddr(mod, rm));
    }    
    std::cerr << "[ERROR] invalid address" << std::endl;
    return 0;
  }

  /* Instruction format:
  *  - OpCode : 6
  *  - Direction : 1 (0: to register, 1: to memory at register addr)
  *  - Word : 1 (0: byte, 1:word)
  *  - S : 1 (0: direct byte, 1: direct word)
  *  - mod : 2 (one (10 or 01) or both (11) arguments are addresses to memory )
  *  - reg : 3 (register code or operand extension)
  *  - r/m : 3 (register/memory,  register used as an arugment or for calculating memory segment offset for an arguemnt)
  *  - 
  */
private:

  Byte FetchByte()
  {
    // c(ycles--;
    return mem[physicalAddr(CS, IP++)];
  }
  Word FetchWord()
  {
    Byte res = mem[physicalAddr(CS, IP++)];
    return res + (mem[physicalAddr(CS, IP++)] << 8);
  }
  
public:  
  void ExecuteNext()
  {
    Byte ins = FetchByte();
    opcodeInstructions.call(*this, ins);    
  }

};