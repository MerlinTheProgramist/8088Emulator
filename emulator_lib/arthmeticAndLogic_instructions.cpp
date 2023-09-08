#include "8088CPU.h"
#include "consts.h"

void CPU::ADD_memORreg_reg(Word op){
  std::cout << "call:" << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
  {
    Word& place = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    Word& reg = getWordReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_WordAdd(place,reg);
    else
      reg = Flags.mark_WordAdd(reg,place);
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
  const Bit s = S_MASK;
  const Byte arg = FetchByte();
  const Bit sub = (arg & 0b0000'1000);
  
  // ADC_SBB
  Bit carry = (arg & 0b0001'0000) & Flags.CF; 
  // SUB
  if(sub) carry*=-1;
  Word src = sub?(-1):(1);
    
  if(arg>>6==0b11)
  {
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
    Word& place = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    Word& reg = getWordReg(arg>>3);
    if(op & DIR_MASK)
      place = Flags.mark_WordAdd(place,reg, Flags.CF);
    else
      reg = Flags.mark_WordAdd(reg,place, Flags.CF);
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

void CPU::INC_memORreg8(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  if(arg >> 6 == 0b11)
  {
    Byte& dest = getByteReg(arg);
    dest = Flags.mark_ByteInc(dest, 1);
  }
  else
  {
    if(op & WORD_MASK)
    {
      Word& dest = mem.getWord(calcAddr(arg>>6, arg));
      dest = Flags.mark_WordInc(dest, 1);
    }else{
      Byte& dest = mem.getByte(calcAddr(arg>>6, arg));
      dest = Flags.mark_ByteInc(dest, 1);
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
    Word& place = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    Word& reg = getWordReg(arg>>3);
    if(op & DIR_MASK)
      place = Flags.mark_WordAdd(place,-reg, Flags.CF);
    else
      reg = Flags.mark_WordAdd(reg, -place, Flags.CF);
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
    Word& place = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    Word& reg = getWordReg(arg>>3);
    if(op & DIR_MASK)
      place = Flags.mark_WordAdd(place, -reg);
    else
      reg = Flags.mark_WordAdd(reg, -place);
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
    Word& place = (arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg));
    Word& reg = getWordReg(arg>>3);
    if(op & DIR_MASK)
      place = Flags.mark_WordAdd(place,-reg, -Flags.CF);
    else      reg = Flags.mark_WordAdd(reg, -place, -Flags.CF);
  }
  else
  {
    Byte& place = (arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg));
    Byte& reg =  getByteReg(arg>>3);
    if(op & DIR_MASK)
       place = Flags.mark_ByteAdd(place, -reg, -Flags.CF);
    else
       reg = Flags.mark_ByteAdd(reg, -place, -Flags.CF);
  }
}

// void CPU::