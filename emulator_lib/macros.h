#pragma once

#define REG_OR_MEM(arg) if(arg>>6==0b11)
 //          {\
 //            Word& place = getWordReg(arg);\
 //            (func)\
 //          }else\
 //            MemWordPos& place{mem.getWord(calcAddr(arg>>6, arg))};\
 //            (func)\
 //          }

