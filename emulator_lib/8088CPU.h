#pragma once

#include <cmath>
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
#include "flags.h"


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
  static constexpr uint32_t MAX_MEM = 1<<20; // 2^20 1MB
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
  void MOVS(Byte);
  void MOV_seg(Byte);
  void MOV_reg(Byte);
  void MOV_ac_mem(Byte);
  void MOV_reg_imm(Byte);

  // pushes
  void PUSH_reg(Byte);
  void PUSH_mem(Byte);
  void PUSH_ES(Byte); 
  void PUSH_CS(Byte); 
  void PUSH_SS(Byte);
  void PUSH_DS(Byte); 
  void PUSHF(Byte);

  // popes
  void POP_reg(Byte);
  void POP_mem(Byte);
  void POP_ES(Byte); 
  void POP_CS(Byte); 
  void POP_SS(Byte);
  void POP_DS(Byte); 
  void POPF(Byte);

  // XCHG (exchage byte or word)
  void XCHG_ac_reg(Byte);
  void XCHG_reg_memORreg(Byte);

  // XLAT (index a table in memory)
  void XLAT(Byte);

  // IN (load AL/AX from IO on addr from DX or imm)
  void IN(Byte);
  void IN_addr(Byte);
  // OUT (output AL/AX to IO on addr from DX or imm)
  void OUT(Byte);
  void OUT_addr(Byte);

  // LEA (load effective address)
  void LEA(Byte);
  // LDS (load data segment register)
  void LDS(Byte);
  // LES (load extra segment register)
  void LES(Byte);

  // LAHF (load AH register from flags)
  void LAHF(Byte);
  // SAHF (store AH reg to frags)
  void SAHF(Byte);

  /***********
  * ARTHMETIC
  ***********/
  void ADD_memORreg_reg(Byte);
  void ADD_ac_imm(Byte);
  void ADD_ADC_SUB_SBB_memORreg_imm(Byte);
  // ACD (add word with carry flag)
  void ADC_memORreg_reg(Byte);
  void ADC_ac_imm(Byte);

  // INC
  void INC_reg16(Byte);
  void INC_DEC_memORreg8(Byte);

  // AAA (asci adjust for addition)
  void AAA(Byte);

  // DAA (decimal adjust for addition)
  void DAA(Byte);

  void SUB_memORreg_reg(Byte);
  void SUB_ac_imm(Byte);
  void SUB_memORreg_imm(Byte);
  
  // SBB (subtract byte or word with borrow)
  void SBB_memORreg_reg(Byte);
  void SBB_ac_imm(Byte);
  void SBB_mem_imm(Byte, Byte);

  // DEC (decrement)
  void DEC_reg16(Byte);
  
  // LOGICAL

  // NEG
  void NEG_regORmem(Byte);

  void CMP_memregORregmem(Byte);
  void CMP_ac_imm(Byte);
  void CMP_reg_imm(Byte, Byte);


  void AAS(Byte);
  void DAS(Byte);
  
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
              operand = Flags.mark_inc(operand, (sWord)1);      
              return;
            case 0b001: // DEC r/m16
              operand = Flags.mark_inc(operand, (sWord)-1);      
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
              operand = Flags.mark_inc(operand, (sByte)1);      
              return;
            case 0b001: // DEC r/m16
              operand = Flags.mark_inc(operand, (sByte)-1);      
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
              if(op & WORD_MASK)
              {
                Word& src = ((arg>>6)==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
                std::tie(DX, AX) = Flags.mark_WordMUL(AX, src);
              }else{
                Byte& src = ((arg>>6)==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
                AX = Flags.mark_ByteMUL(AL, src);
              }
              return;
            case 0b101: //IMUL
              if(op & WORD_MASK)
              {
                Word& src = ((arg>>6)==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
                std::tie(DX, AX) = Flags.mark_WordMUL<true>(AX, src);
              }else{
                Byte& src = ((arg>>6)==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
                AX = Flags.mark_ByteMUL<true>(AL, src);
              }
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

      switch(op){
      // PUSH ES, CS, SS, DS
      case 0b00'000'110: PUSH_ES(op); 
        return;
      case 0b00'001'110: PUSH_CS(op); 
        return;
      case 0b00'010'110: PUSH_SS(op); 
        return;
      case 0b00'011'110: PUSH_DS(op); 
        return;
      // POP ES, CS, SS, DS
      case 0b00'000'111: POP_ES(op); 
        return;
      case 0b00'001'111: POP_CS(op); 
        return;
      case 0b00'010'111: POP_SS(op); 
        return;
      case 0b00'011'111: POP_DS(op); 
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
      case 0b00111111:  AAS(op);
        return;
      case 0b00101111:  DAS(op);
        return;
      }
      return;
    }
};