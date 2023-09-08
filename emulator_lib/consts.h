#pragma once

#include <stdint.h>
#include <bitset>
#include <limits>

using Bit = bool;
using Byte = uint8_t; 
using Word = uint16_t;
// Signed
using sByte = int8_t;
using sWord = int16_t;
// Limits
constexpr Byte  Byte_MAX = std::numeric_limits<Byte>::max();
constexpr sByte sByte_MAX = std::numeric_limits<sByte>::max();
constexpr Word  Word_MAX = std::numeric_limits<Word>::max();
constexpr sWord sWord_MAX = std::numeric_limits<sWord>::max();
constexpr Byte  Nib_MAX = 0b1111;

constexpr Byte WORD_MASK = 0b0000'0001;
constexpr Byte DIR_MASK = 0b0000'0010, S_MASK = 0b0000'0010;
constexpr Byte NIB_MASK = 0b0000'1111; 

