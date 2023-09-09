#include "8088CPU.h"
#include "consts.h"
#include "macros.h"

void CPU::ADD_memORreg_reg(Word op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& reg1 = getWordReg(arg);
      Word& reg2 = getWordReg(arg>>3);
      if(op & DIR_MASK)
         reg1 = Flags.mark_WordAdd(reg1,reg2);
      else
        reg2 = Flags.mark_WordAdd(reg2,reg1);
    }else{
      auto&& mem_place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
         mem_place = Flags.mark_WordAdd(mem_place,reg);
      else
        reg = Flags.mark_WordAdd(reg,mem_place);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ByteAdd(place, reg);
    else
       reg = Flags.mark_ByteAdd(reg, place);
  }
}

void CPU::ADD_ac_imm(Word op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_WordAdd(AX,FetchWord());
  else
    AL = Flags.mark_ByteAdd(AL,FetchByte());
}

// ADD: 1000'00sw 11'000'reg
// ADC: 1000'00sw 11'010'reg
// SUB: 1000'00sw 11'101'reg
// SBB: 1000'00sw 11'011'reg
void CPU::ADD_ADC_SUB_SBB_memORreg_imm(Word op){
  std::cout << "call:" << __func__ << std::endl;
  const Bit s = op & S_MASK;
  const Byte arg = FetchByte();
  const Bit sub = (arg & 0b0000'1000);

  if(((arg>>3)&0b111)==0b111){
    CMP_reg_imm(op, arg);
    return;
  }
  
  // ADC_SBB
  Bit carry = (arg & 0b0001'0000) & Flags.CF; 

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
      dest = Flags.mark_WordAdd(dest,src, carry);
    }else{
      Word dest = getByteReg(arg);
      dest = Flags.mark_ByteAdd(dest,(Byte)src, carry);
    }
  }else{
    src *= (s)?FetchWord():FetchByte();
    if(op & WORD_MASK){
      Word dest = mem.getWord(calcAddr(arg>>6, arg));
      dest = Flags.mark_WordAdd(dest,src, carry);
    }else{
      Word dest = mem.getByte(calcAddr(arg>>6, arg));
      dest = Flags.mark_ByteAdd(dest,(Byte)src, carry);
    }
  }
}

void CPU::ADC_memORreg_reg(Word op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& place = getWordReg(arg);
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        place = Flags.mark_WordAdd(place,reg, Flags.CF);
      else
        reg = Flags.mark_WordAdd(reg,place, Flags.CF);
    }else{
      auto&& place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        place = Flags.mark_WordAdd(place,reg, Flags.CF);
      else
        reg = Flags.mark_WordAdd(reg,place, Flags.CF);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ByteAdd(place, reg, Flags.CF);
    else
       reg = Flags.mark_ByteAdd(reg, place, Flags.CF);
  }
}

void CPU::ADC_ac_imm(Word op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_WordAdd(AX,FetchWord(), Flags.CF);
  else
    AL = Flags.mark_ByteAdd(AL,FetchByte(), Flags.CF);
}


void CPU::INC_reg16(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Word& dest = getWordReg(op);
  dest = Flags.mark_WordInc(dest, 1);
}

void CPU::INC_DEC_memORreg8(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  
  sByte sign = ((arg>>3)!=0b001)?-1:1; // 0b000 for INC
    
  if(arg >> 6 == 0b11)
  {
    Byte& dest = getByteReg(arg);
    dest = Flags.mark_ByteInc(dest, sign);
  }
  else
  {
    if(op & WORD_MASK)
    {
      auto&& dest = mem.getWord(calcAddr(arg>>6, arg));
      dest = Flags.mark_WordInc(dest, sign);
    }else{
      Byte& dest = mem.getByte(calcAddr(arg>>6, arg));
      dest = Flags.mark_ByteInc(dest, sign);
    }
  }
}

void CPU::AAA(Word){
  std::cout << "call: " << __func__ << std::endl;
  if((AL & NIB_MASK)>9 || Flags.AF){
    AL = Flags.mark_ByteAdd(AL,6);
    AH = Flags.mark_WordAdd(AH,1);
    AL &= NIB_MASK;
    Flags.AF = Flags.CF = 1;
  }
}

void CPU::DAA(Word){
  std::cout << "call: " << __func__ << std::endl;
  if((AL & (NIB_MASK)) > 0x9F || Flags.AF){
    AL = Flags.mark_ByteAdd(AL,6);
    Flags.AF = 1;
    if(AL > 0x9f || Flags.CF)
    {
      AL = Flags.mark_ByteAdd(AL, 0x60);
      Flags.CF = 1;
    }
  }
}

void CPU::SUB_memORreg_reg(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& reg1 = getWordReg(arg);
      Word& reg2 = getWordReg(arg>>3);
      if(op & DIR_MASK)
        reg1 = Flags.mark_WordAdd(reg1,-reg2, Flags.CF);
      else
        reg2 = Flags.mark_WordAdd(reg2, -reg1, Flags.CF);
    }else{
      auto&& mem_place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        mem_place = Flags.mark_WordAdd(mem_place,-reg, Flags.CF);
      else
        reg = Flags.mark_WordAdd(reg, -mem_place, Flags.CF);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ByteAdd(place, -reg, Flags.CF);
    else
       reg = Flags.mark_ByteAdd(reg, -place, Flags.CF);
  }
}

void CPU::SUB_ac_imm(Word op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_WordAdd(AX,-FetchWord());
  else
    AL = Flags.mark_ByteAdd(AL,-FetchByte());
}

void CPU::SUB_memORreg_imm(Word op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    REG_OR_MEM(arg){
      Word& reg1 = getWordReg(arg);
      Word& reg2 = getWordReg(arg>>3);
      if(op & DIR_MASK)
        reg1 = Flags.mark_WordAdd(reg1, -reg2);
      else
        reg2 = Flags.mark_WordAdd(reg2, -reg1);
    }else{
      auto&& mem_place{mem.getWord(calcAddr(arg>>6, arg))};
      Word& reg = getWordReg(arg>>3);
      if(op & DIR_MASK)
        mem_place = Flags.mark_WordAdd(mem_place, -reg);
      else
        reg = Flags.mark_WordAdd(reg, -mem_place);
    }
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ByteAdd(place, -reg);
    else
       reg = Flags.mark_ByteAdd(reg, -place);
  }
}

void CPU::SBB_memORreg_reg(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    Word& reg = getWordReg(arg>>3);

    Word& memORreg = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    if(op & DIR_MASK) 
      memORreg = Flags.mark_WordAdd(memORreg,-reg, -Flags.CF);
    else 
      reg = Flags.mark_WordAdd(reg, -memORreg, -Flags.CF);
  }
  else
  {
    Byte& memORreg = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       memORreg = Flags.mark_ByteAdd(memORreg, -reg, -Flags.CF);
    else
       reg = Flags.mark_ByteAdd(reg, -memORreg, -Flags.CF);
  }
}

void CPU::SBB_ac_imm(Word op){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK)
    AX = Flags.mark_WordAdd(AX, -FetchWord(), -Flags.CF);
  else
    AL = Flags.mark_ByteAdd(AL, -FetchByte(), -Flags.CF);
}
void CPU::SBB_mem_imm(Word op, Byte arg){
  std::cout << "call:" << __func__ << std::endl;
  if(op & WORD_MASK){
    Word dest = mem.getWord(calcAddr(arg>>6, arg));
    if(op & S_MASK)
      dest = Flags.mark_WordSub(dest, FetchWord(), Flags.CF);
    else
      dest = Flags.mark_WordSub(dest, FetchByte(), Flags.CF);
  }else{
    Byte dest = mem.getByte(calcAddr(arg>>6, arg));
    if(op & S_MASK)
      dest = Flags.mark_ByteSub(dest, FetchByte(), Flags.CF);
    else
      dest = Flags.mark_ByteSub(dest, FetchByte(), Flags.CF);    
  }
  
}

void CPU::DEC_reg16(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Word& dest = getWordReg(op);
  dest = Flags.mark_WordInc(dest, -1);
}


void CPU::NEG_regORmem(Word op){
  Byte arg = FetchByte();
  if(op & WORD_MASK){
    Word& place = ((arg>>6)==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    if(place == 0)
      Flags.CF = 0;
    else
      place = Flags.mark_WordAdd((Word)0, -place);
  }else{
    Byte& place = ((arg>>6)==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    if(place == 0)
      Flags.CF = 0;
    else
      place = Flags.mark_ByteAdd((Word)0, -place);
  }
}

void CPU::CMP_reg_mem(Word op){
    Byte arg = FetchByte();
    if(op & WORD_MASK)
    {
      Word place = ((arg>>6)==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
      Word reg = getWordReg(arg>>3);
      if(op & DIR_MASK) 
        Flags.mark_WordAdd(reg, -place);
      else
        Flags.mark_WordAdd(place, -reg);
    }else{
      Byte place = ((arg>>6)==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
      Byte reg = getByteReg(arg>>3);
      if(op & DIR_MASK) 
        Flags.mark_ByteAdd(reg, -place);
      else
        Flags.mark_ByteAdd(place, -reg);
    }
}

void CPU::CMP_reg_imm(Word op, Byte arg){
  Bit s = arg & S_MASK;
  if(op & WORD_MASK)
  {
    Word reg = getWordReg(arg);
    Word imm = s?FetchWord():FetchByte();
    Flags.mark_WordAdd(reg, -imm);
  }else{
    Byte reg = getByteReg(arg);
    Byte imm = s?FetchByte():FetchByte();
    Flags.mark_ByteAdd(reg, -imm);
  }
}

void CPU::CMP_mem_imm(Word op, Byte arg){
  
}
