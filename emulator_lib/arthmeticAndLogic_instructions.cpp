#include "8088CPU.h"
#include "consts.h"
#include "macros.h"
#include <memory>

void CPU::ADD_memORreg_reg(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& reg1 = getWordReg(arg);
      Word& reg2 = getWordReg(arg>>3);
      if(op & DIR_MASK)
         reg1 = Flags.mark_ADD(reg1,reg2);
      else
        reg2 = Flags.mark_ADD(reg2,reg1);
    }else{
      auto&& mem_place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
         mem_place = Flags.mark_ADD(mem_place,reg);
      else
        reg = Flags.mark_ADD(reg,mem_place);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ADD(place, reg);
    else
       reg = Flags.mark_ADD(reg, place);
  }
}

void CPU::ADD_ac_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_ADD(AX,FetchWord());
  else
    AL = Flags.mark_ADD(AL,FetchByte());
}

// ADD: 1000'00sw 11'000'reg
// ADC: 1000'00sw 11'010'reg
// SUB: 1000'00sw 11'101'reg
// SBB: 1000'00sw 11'011'reg
void CPU::ADD_ADC_SUB_SBB_memORreg_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  const Bit s = op & S_MASK;
  const Byte arg = FetchByte();
  const Bit sub = (arg & 0b0000'1000);

  if(((arg>>3)&0b111)==0b111){
    CMP_reg_imm(op);
    return;
  }
  
  // ADC_SBB
  Byte carry = (arg & 0b0001'0000) & Flags.CF; 

  // SUB
  Word src = sub?(-1):(1);
    
  if(arg>>6==0b11)
  {
    // SBB
    if(arg>>3==0b11011) carry*=-1; 

    src *= (s)?FetchWord():FetchByte();
    if(op & WORD_MASK)
    {
      Word dest = getWordReg(arg);
      dest = Flags.mark_ADD(dest,src, carry);
    }else{
      Byte dest = getByteReg(arg);
      dest = Flags.mark_ADD(dest,(Byte)src, carry);
    }
  }else{
    src *= (s)?FetchWord():FetchByte();
    if(op & WORD_MASK){
      Word dest = mem.getWord(calcAddr(arg>>6, arg));
      dest = Flags.mark_ADD(dest,src, carry);
    }else{
      Byte dest = mem.getByte(calcAddr(arg>>6, arg));
      dest = Flags.mark_ADD(dest,(Byte)src, carry);
    }
  }
}

void CPU::ADC_memORreg_reg(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& place = getWordReg(arg);
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        place = Flags.mark_ADD(place,reg, Flags.CF);
      else
        reg = Flags.mark_ADD(reg,place, Flags.CF);
    }else{
      auto&& place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        place = Flags.mark_ADD(place,reg, Flags.CF);
      else
        reg = Flags.mark_ADD(reg,place, Flags.CF);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ADD(place, reg, Flags.CF);
    else
       reg = Flags.mark_ADD(reg, place, Flags.CF);
  }
}

void CPU::ADC_ac_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_ADD(AX,FetchWord(), Flags.CF);
  else
    AL = Flags.mark_ADD(AL,FetchByte(), Flags.CF);
}


void CPU::INC_reg16(Byte op){
  std::cout << "call: " << __func__ << std::endl;
  Word& dest = getWordReg(op);
  dest = Flags.mark_inc(dest, (Word)1);
}

void CPU::INC_DEC_memORreg8(Byte op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  
  sByte sign = ((arg>>3)!=0b001)?-1:1; // 0b000 for INC
    
  if(arg >> 6 == 0b11)
  {
    Byte& dest = getByteReg(arg);
    dest = Flags.mark_inc(dest, sign);
  }
  else
  {
    if(op & WORD_MASK)
    {
      auto&& dest = mem.getWord(calcAddr(arg>>6, arg));
      dest = Flags.mark_inc(dest, sign);
    }else{
      Byte& dest = mem.getByte(calcAddr(arg>>6, arg));
      dest = Flags.mark_inc(dest, sign);
    }
  }
}

void CPU::AAA(Byte){
  std::cout << "call: " << __func__ << std::endl;
  if((AL & NIB_MASK)>9 || Flags.AF){
    AL = Flags.mark_ADD(AL,(Byte)6);
    AH = Flags.mark_ADD(AH,(Byte)1);
    AL &= NIB_MASK;
    Flags.AF = Flags.CF = 1;
  }
}

void CPU::DAA(Byte){
  std::cout << "call: " << __func__ << std::endl;
  if((AL & (NIB_MASK)) > 9 || Flags.AF){
    AL = Flags.mark_ADD(AL,(Byte)6);
    Flags.AF = 1;
    if(AL > 0x9f || Flags.CF)
    {
      AL = Flags.mark_ADD(AL, (Byte)0x60);
      Flags.CF = 1;
    }
  }
}

void CPU::SUB_memORreg_reg(Byte op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& reg1 = getWordReg(arg);
      Word& reg2 = getWordReg(arg>>3);
      if(op & DIR_MASK)
        reg1 = Flags.mark_ADD<Word>(reg1, static_cast<Word>(-reg1), Flags.CF);
      else
        reg2 = Flags.mark_ADD<Word>(reg2, static_cast<Word>(-reg1), Flags.CF);
    }else{
      auto&& mem_place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        mem_place = Flags.mark_ADD<Byte>(mem_place, static_cast<Byte>(-reg), Flags.CF);
      else
        reg = Flags.mark_ADD<Byte>(reg, static_cast<Byte>(-mem_place), Flags.CF);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ADD<Byte>(place, static_cast<Byte>(-reg), Flags.CF);
    else
       reg = Flags.mark_ADD<Byte>(reg, static_cast<Byte>(-place), Flags.CF);
  }
}

void CPU::SUB_ac_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_ADD(AX, static_cast<Word>(-FetchWord()));
  else
    AL = Flags.mark_ADD(AL, static_cast<Byte>(-FetchByte()));
}

void CPU::SUB_memORreg_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& reg1 = getWordReg(arg);
      Word& reg2 = getWordReg(arg>>3);
      if(op & DIR_MASK)
        reg1 = Flags.mark_ADD(reg1, static_cast<Word>(-reg2));
      else
        reg2 = Flags.mark_ADD(reg2, static_cast<Word>(-reg1));
    }else{
      auto&& mem_place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        mem_place = Flags.mark_ADD(mem_place, static_cast<Word>(-reg));
      else
        reg = Flags.mark_ADD(reg, static_cast<Word>(-mem_place));
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ADD(place, static_cast<Byte>(-reg));
    else
       reg = Flags.mark_ADD(reg, static_cast<Byte>(-place));
  }
}

void CPU::SBB_memORreg_reg(Byte op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    Word& reg = getWordReg(arg>>3);

    Word& memORreg = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    if(op & DIR_MASK) 
      memORreg = Flags.mark_ADD(memORreg, static_cast<Word>(-reg), -Flags.CF);
    else 
      reg = Flags.mark_ADD(reg, static_cast<Word>(-memORreg), -Flags.CF);
  }
  else
  {
    Byte& memORreg = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       memORreg = Flags.mark_ADD(memORreg, static_cast<Byte>(-reg), -Flags.CF);
    else
       reg = Flags.mark_ADD(reg, static_cast<Byte>(-memORreg), -Flags.CF);
  }
}

void CPU::SBB_ac_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_ADD(AX, static_cast<Word>(-FetchWord()), -Flags.CF);
  else
    AL = Flags.mark_ADD(AL, static_cast<Byte>(-FetchByte()), -Flags.CF);
}
void CPU::SBB_mem_imm(Byte op){
  std::cout << "call:" << __func__ << std::endl;

  Byte arg = FetchWord();
  if(op & WORD_MASK){
    Word dest = mem.getWord(calcAddr(arg>>6, arg));
    if(op & S_MASK)
      dest = Flags.mark_SUB(dest, FetchWord(), Flags.CF);
    else
      dest = Flags.mark_SUB(dest, static_cast<Word>(FetchByte()), Flags.CF);
  }else{
    Byte dest = mem.getByte(calcAddr(arg>>6, arg));
    if(op & S_MASK)
      dest = Flags.mark_SUB(dest, FetchByte(), Flags.CF);
    else
      dest = Flags.mark_SUB(dest, FetchByte(), Flags.CF);    
  }
  
}

void CPU::DEC_reg16(Byte op){
  std::cout << "call: " << __func__ << std::endl;
  Word& dest = getWordReg(op);
  dest = Flags.mark_inc(dest, -1);
}


void CPU::NEG_regORmem(Byte op){
  Byte arg = FetchByte();
  if(op & WORD_MASK){
    Word& place = ((arg>>6)==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    if(place == 0)
      Flags.CF = 0;
    else
      place = Flags.mark_ADD<Word>(0, static_cast<Word>(-place));
  }else{
    Byte& place = ((arg>>6)==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    if(place == 0)
      Flags.CF = 0;
    else
      place = Flags.mark_ADD<Word>(0, static_cast<Word>(-place));
  }
}

void CPU::CMP_memregORregmem(Byte op){
    Byte arg = FetchByte();
  
    if(op & WORD_MASK)
    {
      Word place = ((arg>>6)==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
      Word reg = getWordReg(arg>>3);
      if(op & DIR_MASK) 
        Flags.mark_SUB(reg, place);
      else
        Flags.mark_SUB(place, reg);
    }else{
      Byte place = ((arg>>6)==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
      Byte reg = getByteReg(arg>>3);
      if(op & DIR_MASK) 
        Flags.mark_SUB(reg, place);
      else
        Flags.mark_SUB(place, reg);
    }
}

void CPU::CMP_reg_imm(Byte op){
  Byte arg = FetchByte();
  Bit s = arg & S_MASK;
  if(op & WORD_MASK)
  {
    Word reg = getWordReg(arg);
    Word imm = s?FetchWord():FetchByte();
    Flags.mark_ADD(reg, static_cast<Word>(-imm));
  }else{
    Byte reg = getByteReg(arg);
    Byte imm = s?FetchByte():FetchByte();
    Flags.mark_ADD(reg, static_cast<Byte>(-imm));
  }
}

void CPU::CMP_ac_imm(Byte op){
  if(op & WORD_MASK)
    Flags.mark_SUB(AX, FetchWord());
  else
    Flags.mark_SUB(AL, FetchByte());
}

void CPU::CMP_mem_imm(Byte op){
  Byte arg = FetchWord();
  Addr addr = calcAddr(arg>>6, arg);
  if(op & WORD_MASK){
    Word imm = mem.getWord(addr);
    if(op & S_MASK)
      Flags.mark_SUB(imm, FetchWord());
    else
      Flags.mark_SUB(imm, static_cast<Word>(FetchByte()));
  }else{
    Byte imm = mem.getByte(addr);
    if(op & S_MASK)
      Flags.mark_SUB(imm, FetchByte());
    else
      Flags.mark_SUB(imm, FetchByte());
  }
}

void CPU::AAS(Byte){
  if((AL & NIB_MASK) > 9 || Flags.AF == 1)
  {
    AL = Flags.mark_SUB(AL, (Byte)6);
    AH = Flags.mark_inc(AH, -1);
    AL &= NIB_MASK;

    Flags.AF = Flags.CF = 1;
  }
}

void CPU::DAS(Byte){
  if((AL & NIB_MASK) > 9 || Flags.AF == 1)
  {
    AL = Flags.mark_SUB(AL, (Byte)6);
    Flags.AF = 1;
    if(AL > 0x9F || Flags.CF == 1)
    {
      AL = Flags.mark_SUB(AL, (Byte)0x60);
      Flags.CF = 1;
    }
  }
}