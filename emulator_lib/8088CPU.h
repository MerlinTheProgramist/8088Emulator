#pragma once

#include <cstdint>
#include <exception>
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
#include <type_traits>

#include "consts.h"
#include "macros.h"



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

  private:
    // xoring each bit to the least significant, and then test it
    template<typename T>
    bool getParity(T n);
    
  public:
    // OF SF ZF AF PF CF
    template<typename T>
    T mark_ADD(T a, T b, Bit c=0);
    template<class T, class T1> Byte mark_ADD(T, T1, Bit=0) = delete;

    template<typename T>
    T mark_SUB(T a, T b, Bit c=0);
    template<class T, class T1> Byte mark_SUB(T, T1, Bit=0) = delete;

    template<typename T>
    T mark_AND(T a, T b);
    template<class T, class T1> Byte mark_AND(T, T1) = delete;
    
    template<typename T>
    T mark_OR(T a, T b);
    template<class T, class T1> Byte mark_OR(T, T1) = delete;
    
    template<typename T>
    T mark_XOR(T a, T b);
    template<class T, class T1> Byte mark_XOR(T, T1) = delete;
      
    // Same as Add but without CF
    template<typename T, typename sT = std::make_signed_t<T>>
    T mark_inc(T a, sT c=1);
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
  Byte LookupByte(Word offset=0)
  {
    return mem[physicalAddr(CS, IP+offset)];
  }
  
  Word FetchWord()
  {
    Byte res = mem[physicalAddr(CS, IP++)];
    return res + (mem[physicalAddr(CS, IP++)] << 8);
  }
  
public:  
  void ExecuteNext()
  {
    Byte op = FetchByte();

    // #define OP_CASE(bits) case reverse(bits) 
    switch(op>>4){
    // 4 bit
      case 0b1011:       MOV_reg_imm(op); 
        return;  
    }
    // 5 bit
    switch(op>>3){
      case 0b01010:     PUSH_reg(op); 
        return;
      case 0b01011:     POP_reg(op); 
        return;       
      case 0b10010:     XCHG_ac_reg(op); 
        return;
      case 0b01000:     INC_reg16(op); 
        return;
      case 0b01001:     DEC_reg16(op); 
        return;
    }
    // 6 bit
    switch(op>>2){
      case 0b100011:    MOV_seg(op); 
        return;
      case 0b100010:    MOV_reg(op); 
        return;
      case 0b101000:     MOV_ac_mem(op);
        return;
      case 0b100111:    PUSHF(op); 
        return;
      case 0x100000: // regORmem_imm
      {
        Byte arg = FetchByte();
        Bit s = (op & S_MASK);
        Bit w = (op & WORD_MASK);

        if(s && !w)
          throw std::invalid_argument("[INSTRUCTION SET] `S` option can't be set when `Word` is disabled ");
        
        if(op & WORD_MASK)
        {
          Word& dest = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
          Word src = s?FetchByte():FetchWord();
          switch((arg>>3)&0b111)
          {
            case 0b000: dest = Flags.mark_ADD(dest, src); // ADD 
              return;
            case 0b001: dest = Flags.mark_OR(dest, src);  // OR  
              return;
            case 0b010: dest = Flags.mark_ADD(dest, src, Flags.CF); // ADC
              return;
            case 0b011: dest = Flags.mark_SUB(dest, src, Flags.CF); // SBB
              return;
            case 0b100: dest = Flags.mark_AND(dest, src); // AND 
              return;
            case 0b101: dest = Flags.mark_SUB(dest, src); // SUB
              return; 
            case 0b110: dest = Flags.mark_XOR(dest, src); // XOR
              return;
            case 0b111: Flags.mark_SUB(dest, src); // CMP 
              return;
          }
        }else{
          Byte& dest = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
          Byte src = FetchByte();
          switch((arg>>3)&0b111)
          {
            switch((arg>>3)&0b111)
            {
              case 0b000: dest = Flags.mark_ADD(dest, src); // ADD 
                return;
              case 0b001: dest = Flags.mark_OR(dest, src);  // OR  
                return;
              case 0b010: dest = Flags.mark_ADD(dest, src, Flags.CF); // ADC
                return;
              case 0b011: dest = Flags.mark_SUB(dest, src, Flags.CF); // SBB
                return;
              case 0b100: dest = Flags.mark_AND(dest, src); // AND 
                return;
              case 0b101: dest = Flags.mark_SUB(dest, src); // SUB
                return; 
              case 0b110: dest = Flags.mark_XOR(dest, src); // XOR
                return;
              case 0b111: Flags.mark_SUB(dest, src); // CMP 
                return;
            }
          }
        }
        throw std::invalid_argument("IMPOSSIBLE");
      }
      case 0b000000:    ADD_memORreg_reg(op); 
        return;
      case 0b000100:    ADC_memORreg_reg(op); 
        return;
      case 0b001010:    SUB_memORreg_reg(op); 
        return;
      case 0b000110:    SBB_memORreg_reg(op); 
        return;
      case 0b001110:    CMP_memregORregmem(op); 
        return;
      }

      switch(op>>1){
      case 0b1010010:    MOVS(op); 
        return;
      case 0b1000011:   XCHG_reg_memORreg(op); 
        return;
      case 0b1110110:   IN(op); 
        return;
      case 0b1110010:   IN_addr(op); 
        return;
      case 0b1110111:   OUT(op); 
        return;
      case 0b1110011:   OUT_addr(op); 
        return;
      case 0b0000010:   ADD_ac_imm(op); 
        return;
      case 0b0001010:   ADC_ac_imm(op); 
        return;
      case 0b1111111:   // INC_DEC_memORreg8(op); 
      {
        Byte arg = FetchByte();
        
        if(op & WORD_MASK){
          Word& operand = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
          switch((arg>>3)&0b111)
          {
            case 0b000: //INC r/m16
              operand = Flags.mark_inc(operand, 1);      
              return;
            case 0b001: // DEC r/m16
              operand = Flags.mark_inc(operand, -1);      
              return;
            case 0b110: // PUSH r/m16
              pushWord(operand);
              return;
          }
        }else{
          Byte& operand = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
          switch((arg>>3)&0b111)
          {
            case 0b000: //INC r/m16
              operand = Flags.mark_inc(operand, 1);      
              return;
            case 0b001: // DEC r/m16
              operand = Flags.mark_inc(operand, -1);      
              return;
          }
        }
        throw std::invalid_argument("Invalid opcode arguments");  
      }
        return;
      case 0b1000110:   SUB_ac_imm(op); 
        return;
      case 0b0001110:   SBB_ac_imm(op); 
        return;
      case 0b1111011:   //NEG_regORmem(op); 
      {
        Byte arg = FetchByte();
        if(op & WORD_MASK)
          switch((arg>>3)&0b111)
          {
            case 0b010: //NOT
              return;
            case 0b011: //NEG
              return;
            case 0b100: //MUL
              return;
            case 0b101: //IMUL
              return;
            case 0b110: // DIV
              return;
            case 0b111: //IDIV
              return;
          }
        else
          switch((arg>>3)&0b111)
          {
            case 0b010:
              return;
            case 0b011:
              return;
            case 0b100:
              return;
            case 0b101:
              return;
            case 0b110:
              return;
            case 0b111:
              return;
          }
        throw std::invalid_argument("Invalid opcode arguments"); 
      }
        return;
      case 0b0011110:   CMP_ac_imm(op); 
        return;
      }
      switch(op>>8){
      // PUSH ES, CS, SS, DS
      case 0b00000000: PUSH_ES(op); 
        return;
      case 0b00001000: PUSH_CS(op); 
        return;
      case 0b00010000: PUSH_SS(op); 
        return;
      case 0b00011000: PUSH_DS(op); 
        return;
      // POP ES, CS, SS, DS
      case 0b00000111: POP_ES(op); 
        return;
      case 0b00001111: POP_CS(op); 
        return;
      case 0b00010111: POP_SS(op); 
        return;
      case 0b00011111: POP_DS(op); 
        return;
      return;

      case 0b11010111:  XLAT(op); 
        return;
      case 0b10001101:  LEA(op); 
        return;
      case 0b11000101:  LDS(op); 
        return;
      case 0b11000100:  LES(op); 
        return;
      case 0b10011111:  LAHF(op); 
        return;
      case 0b10011110:  SAHF(op); 
        return;
      case 0b10011100:  PUSHF(op); 
        return;
      case 0b10011101:  POPF(op); 
        return;
      case 0b00110111:  AAA(op); 
        return;
      case 0b00100111:  DAA(op); 
        return;
      }
      return;
    }
};