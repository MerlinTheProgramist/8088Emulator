#include "8088CPU.h"

bool CPU::Flags_t::getWordParity(Word n){
  n ^= n >> 8;
  n ^= n >> 4;
  n ^= n >> 2;
  n ^= n >> 1;
  return (~n) & 1;
}
bool CPU::Flags_t::getByteParity(Byte n){
  n ^= n >> 4;
  n ^= n >> 2;
  n ^= n >> 1;
  return (~n) & 1;
}
// OF SF ZF AF PF CF
Word CPU::Flags_t::mark_WordAdd(Word a, Word b, Bit c){
  Word res = a+b+c;
  OF = ((sWord)a<0 && (sWord)b<0 && (sWord)res>0) /*underflow*/ || ((sWord)a>0 && (sWord)b>0 && (sWord)res<0);
  SF = (sWord)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getWordParity(res);
  CF = (res < a); 
  return res;
}

// OF SF ZF AF PF CF
Word CPU::Flags_t::mark_WordSub(Word a, Word b, Bit c){
  Word res = a-b-c;
  OF = ((sWord)a<0 && (sWord)b<0 && (sWord)res>0) /*underflow*/ || ((sWord)a>0 && (sWord)b>0 && (sWord)res<0);
  SF = (sWord)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getWordParity(res);
  CF = (res < a); 
  return res;
}

Byte CPU::Flags_t::mark_ByteAdd(Byte a, Byte b, Bit c){
  Byte res = a+b+c;
  OF = ((sByte)a<0 && (sByte)b<0 && (sByte)res>0) /*underflow*/ || ((sByte)a>0 && (sByte)b>0 && (sByte)res<0);
  SF = (sByte)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getByteParity(res);
  CF = (res < a);
  return res; 
}

Byte CPU::Flags_t::mark_ByteSub(Byte a, Byte b, Bit c){
  Byte res = a-b-c;
  OF = ((sByte)a<0 && (sByte)b<0 && (sByte)res>0) /*underflow*/ || ((sByte)a>0 && (sByte)b>0 && (sByte)res<0);
  SF = (sByte)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getByteParity(res);
  CF = (res < a);
  return res; 
}

// Same as Add but without CF
Word CPU::Flags_t::mark_WordInc(Word a, Word c){
  Word res = a+c;
  OF = ((sWord)a<0 && (sWord)res>0) /*underflow*/ || ((sWord)a>0 && (sWord)res<0);
  SF = (sWord)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getWordParity(res);
  return res;
}
// Same as Add but without CF
Byte CPU::Flags_t::mark_ByteInc(Byte a, Byte c){
  Byte res = a+c;
  OF = ((sByte)a<0 && (sByte)res>0) /*underflow*/ || ((sByte)a>0 && (sByte)res<0);
  SF = (sByte)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getByteParity(res);
  return res;
}
