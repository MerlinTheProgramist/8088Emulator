#include "8088CPU.h"
#include <type_traits>

template<>
bool CPU::Flags_t::getParity<Word>(Word n){
  n ^= n >> 8;
  n ^= n >> 4;
  n ^= n >> 2;
  n ^= n >> 1;
  return (~n) & 1;
}
template<> 
bool CPU::Flags_t::getParity<Byte>(Byte n){
  n ^= n >> 4;
  n ^= n >> 2;
  n ^= n >> 1;
  return (~n) & 1;
}
// OF SF ZF AF PF CF
template<typename T>
T CPU::Flags_t::mark_ADD(T a, T b, Bit c){
  using sT = std::make_signed<T>;
  T res = a+b+c;
  OF = ((sT)a<0 && (sT)b<0 && (sT)res>0) /*underflow*/ || ((sT)a>0 && (sT)b>0 && (sT)res<0);
  SF = (sT)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getTParity(res);
  CF = (res < a); 
  return res;
}

// OF SF ZF AF PF CF
template<typename T>
T CPU::Flags_t::mark_SUB(T a, T b, Bit c){
  using sT = std::make_signed<T>;
  T res = a-b-c;
  OF = ((sT)a<0 && (sT)b<0 && (sT)res>0) /*underflow*/ || ((sT)a>0 && (sT)b>0 && (sT)res<0);
  SF = (sT)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getTParity(res);
  CF = (res < a); 
  return res;
}

// Same as Add but without CF
template<typename T, typename sT>
T CPU::Flags_t::mark_inc(T a, sT c){
  T res = a+c;
  OF = ((sT)a<0 && (sT)res>0) /*underflow*/ || ((sT)a>0 && (sT)res<0);
  SF = (sT)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getTParity(res);
  return res;
}
