#include "consts.h"
#include <type_traits>

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
  template<typename T, bool SIGN=false>
  T mark_ADD(T a, T b, Bit c=0);
  // template<class T, class T1> Byte mark_ADD(T, T1, Bit=0) = delete;

  template<typename T, bool SIGN=false>
  T mark_SUB(T a, T b, Bit c=0);
  // template<class T, class T1> Byte mark_SUB(T, T1, Bit=0) = delete;

  template<typename T, bool SIGN=false>
  T mark_AND(T a, T b);
  // template<class T, class T1> Byte mark_AND(T, T1) = delete;
  
  template<typename T, bool SIGN=false>
  T mark_OR(T a, T b);
  // template<class T, class T1> Byte mark_OR(T, T1) = delete;
  
  template<typename T, bool SIGN=false>
  T mark_XOR(T a, T b);

  template<bool SIGN=false>
  Word mark_ByteMUL(Byte a, Byte b);
  template<bool SIGN=false>
  std::pair<Word, Word> mark_WordMUL(Word a, Word b);
  // template<class T, class T1> Byte mark_XOR(T, T1) = delete;
    
  // Same as Add but without CF
  template<typename T, typename sT = std::make_signed_t<T>>
  T mark_inc(T a, sT c=1);
};



template<>
inline bool Flags_t::getParity<Word>(Word n){
  n ^= n >> 8;
  n ^= n >> 4;
  n ^= n >> 2;
  n ^= n >> 1;
  return (~n) & 1;
}
template<> 
inline bool Flags_t::getParity<Byte>(Byte n){
  n ^= n >> 4;
  n ^= n >> 2;
  n ^= n >> 1;
  return (~n) & 1;
}
// OF SF ZF AF PF CF
template<typename T, bool SIGN>
T Flags_t::mark_ADD(T a, T b, Bit c){
  using sT = std::make_signed_t<T>;
  T res = a+b+c;
  OF = ((sT)a<0 && (sT)b<0 && (sT)res>0) /*underflow*/ || ((sT)a>0 && (sT)b>0 && (sT)res<0);
  SF = (sT)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getParity(res);
  CF = (res < a); 
  return res;
}

// OF SF ZF AF PF CF
template<typename T, bool SIGN>
T Flags_t::mark_SUB(T a, T b, Bit c){
  using sT = std::make_signed_t<T>;
  T res = a-b-c;
  
  OF = ((sT)a<0 && (sT)b<0 && (sT)res>0) /*underflow*/ || ((sT)a>0 && (sT)b>0 && (sT)res<0);
  SF = (sT)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getParity(res);
  CF = (res < a); 
  return res;
}

// Same as Add but without CF
template<typename T, typename sT>
T Flags_t::mark_inc(T a, sT c){
  T res = a+c;
  OF = ((sT)a<0 && (sT)res>0) /*underflow*/ || ((sT)a>0 && (sT)res<0);
  SF = (sT)res < 0;
  ZF = (res == 0);
  AF = (res & NIB_MASK) < (a & NIB_MASK);
  PF = getParity(res);
  return res;
}

template<bool SIGN>
std::pair<Word, Word> Flags_t::mark_WordMUL(Word a, Word b){
  uint32_t res = SIGN?(static_cast<sWord>(a)*static_cast<sWord>(b)):(a*b);
  CF = OF = SIGN?(res!=(res&0xFFFF)):((res & 0xFFFF0000 ) != 0);
  return {res>>16, res};
}

template<bool SIGN>
Word Flags_t::mark_ByteMUL(Byte a, Byte b){
  Word res = SIGN?(static_cast<sByte>(a)*static_cast<sByte>(b)):(a*b);
  CF = OF = SIGN?(res!=(res&0xFF)):(res & 0xFF00 ) != 0;
  return res;
}
