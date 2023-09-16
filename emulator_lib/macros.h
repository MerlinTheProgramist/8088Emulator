#pragma once
#include "consts.h"

static constexpr Byte lookup[16] = {
  0x0, 0x8, 0x4, 0xc, 0x2, 0xa, 0x6, 0xe,
  0x1, 0x9, 0x5, 0xd, 0x3, 0xb, 0x7, 0xf, 
};

constexpr Byte reverse(Byte n){
 return (lookup[n&0b1111] << 4) | lookup[n>>4];
}


#define REG_OR_MEM(arg) if(arg>>6==0b11)
 //          {\
 //            Word& place = getWordReg(arg);\
 //            (func)\
 //          }else\
 //            MemWordPos& place{mem.getWord(calcAddr(arg>>6, arg))};\
 //            (func)\
 //          }

