#include "8088CPU.h"
#include "consts.h"

#include <bitset>

void CPU::MOVS(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  bool word = opcode & WORD_MASK;

  uint32_t src = physicalAddr(DS, SI);
  uint32_t dest = physicalAddr(ES, DI);
  
  if(word)
    mem.getWord(dest) = mem.getWord(src);
  else
    mem[dest] = mem[src];
    
  if(Flags.DF == 0){
    SI+= word?2:1;
    DI+= word?2:1;
  }
  else{
    SI-= word?2:1;
    DI-= word?2:1;
  }
}

void CPU::MOV_reg(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  bool dir = opcode & DIR_MASK;
  bool word = opcode & WORD_MASK;

  Byte arg = FetchByte();
  if(word){
    Word& place = (arg>>6 == 0b11)? getWordReg(arg) : mem.getWord(calcAddr(arg >> 6, arg));
    Word& reg = getWordReg(opcode >> 3);
    if(!dir)
      reg = place;
    else
      place = reg;
  }else{
    Byte& place = (arg>>6 == 0b11)? getByteReg(arg) : mem.getByte(calcAddr(arg >> 6, arg));
    Byte& reg = getByteReg(opcode >> 3);
    if(!dir)
      reg = place;
    else
      place = reg;
  }
  
}

void CPU::MOV_seg(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  bool dir = opcode & DIR_MASK;

  Byte arg = FetchByte();
  
  Word& place = (arg>>6 == 0b11)? getWordReg(arg) : mem.getWord(calcAddr(arg >> 6, arg));
 
  Word& segreg = getSegReg(arg >> 3);

  if(segreg == CS)
    std::cerr << "[ERROR] MOV segreg can't access CS register" << std::endl;
  
  if(!dir)// 8E
    segreg = place;
  else // 8C
    place = segreg;
}

void CPU::MOV_ac_mem(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  bool word = opcode & WORD_MASK;
  bool dir = opcode & 0b0000'0010;
  if(word)
  {
    Word& word = mem.getWord(FetchWord());
    if(!dir) // ac <- mem
      AX = word;
    else 
      word = AX;
  }
  else
  {
    Byte& byte = mem[FetchWord()];
    if(dir == 0) // ac <- mem
      AL = byte;
    else
      byte = AL;
  }
}

void CPU::MOV_reg_imm(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  bool word = opcode & 0b0000'1000;  // special case of word mask
  uint8_t reg = opcode & 0b0000'0111;
  // bool dest = ins & 0b0000'0100;
  if(word)
    getWordReg(reg) = FetchWord();
  else // byte
    getByteReg(reg) = FetchByte();
}

void CPU::PUSHF(Word){
  std::cout << "call: " << __func__ << std::endl;
  pushWord(flagsWord);
}


void CPU::PUSH_mem(Word){
  std::cout << "call: " << __func__ << std::endl;
 Byte arg = FetchByte();
 pushWord( mem.getWord(calcAddr(arg >> 6, arg)));
}
void CPU::PUSH_reg(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  pushWord(getWordReg(opcode));
}

void CPU::PUSH_ES(Word){
  std::cout << "call: " << __func__ << std::endl;
  pushWord(getSegReg(ES));
}
void CPU::PUSH_CS(Word){
  std::cout << "call: " << __func__ << std::endl;
  pushWord(getSegReg(CS));
}
void CPU::PUSH_SS(Word){
  std::cout << "call: " << __func__ << std::endl;
  pushWord(getSegReg(SS));
}
void CPU::PUSH_DS(Word){
  std::cout << "call: " << __func__ << std::endl;
  pushWord(getSegReg(DS));
}

void CPU::POPF(Word){
  std::cout << "call: " << __func__ << std::endl;
  flagsWord = popWord();
}
void CPU::POP_mem(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  mem.getWord(calcAddr(opcode>>6, opcode)) = popWord();
}
void CPU::POP_reg(Word opcode){
  std::cout << "call: " << __func__ << std::endl;
  getWordReg(opcode) = popWord();
}
void CPU::POP_ES(Word){
  std::cout << "call: " << __func__ << std::endl;
  getSegReg(ES) = popWord();
}
void CPU::POP_CS(Word){
  std::cout << "call: " << __func__ << std::endl;
  getSegReg(CS) = popWord();
}
void CPU::POP_SS(Word){
  std::cout << "call: " << __func__ << std::endl;
  getSegReg(SS) = popWord();
}
void CPU::POP_DS(Word){
  std::cout << "call: " << __func__ << std::endl;
  getSegReg(DS) = popWord();
}



void CPU::XCHG_ac_reg(Word op){
  std::cout << "call: " << __func__ << std::endl;
  if(op & WORD_MASK)
    std::swap(AX, getWordReg(op));
  else
    std::swap(AL, getByteReg(op));
}
void CPU::XCHG_reg_memORreg(Word op){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  if(op & WORD_MASK)
    std::swap((arg>>6==0b11)?getWordReg(arg):mem.getWord(calcAddr(arg>>6, arg)), getWordReg(arg>>3));
  else
    std::swap((arg>>6==0b11)?getByteReg(arg):mem.getByte(calcAddr(arg>>6, arg)), getByteReg(arg>>3));
}

void CPU::XLAT(Word){
  std::cout << "call: " << __func__ << std::endl;
  AL = mem.getByte(physicalAddr(DS, BX+AL));
}

void CPU::LEA(Word){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  getWordReg(arg>>3) = calcEffectiveAddr(arg>>6, arg);
}

void CPU::LDS(Word){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  getWordReg(arg>>3) = mem.getWord(calcAddr(arg>>6, arg));
  DS = mem.getWord(calcAddr(arg>>6, arg) + 2);
}

void CPU::LES(Word){
  std::cout << "call: " << __func__ << std::endl;
  Byte arg = FetchByte();
  getWordReg(arg>>3) = mem.getWord(calcAddr(arg>>6, arg));
  ES = mem.getWord(calcAddr(arg>>6, arg) + 2);
}

void CPU::LAHF(Word){
  std::cout << "call: " << __func__ << std::endl;
  AH = flagsWord; // less signification portion only
}

void CPU::SAHF(Word){
  std::cout << "call: " << __func__ << std::endl;
  constexpr Word mask = ~((1<<8)-1);
  flagsWord &= mask;
  flagsWord |= AH;
}


void CPU::IN(Word op){
  std::cout << "call: " << __func__ << std::endl;

}
void CPU::IN_addr(Word op){
  std::cout << "call: " << __func__ << std::endl;

}
void CPU::OUT(Word op){
  std::cout << "call: " << __func__ << std::endl;

}
void CPU::OUT_addr(Word op){
  std::cout << "call: " << __func__ << std::endl;
}

