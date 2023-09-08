#include <catch2/catch_all.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>
#include <catch2/generators/catch_generators_random.hpp>
#include <istream>
#include <string>
#include "../emulator_lib/8088CPU.h"

constexpr char fileName[]{"test_binary.bin"};



TEST_CASE("Mem", "")
{
  Mem mem{};

  SECTION("Provide invalid file")
  {
    std::ifstream invalidStream{""};
    REQUIRE_THROWS(mem.Load(invalidStream));
  }

  SECTION("Load Some Data")
  {
    // auto data = GENERATE(take(3,Catch::Generators::random((Word)0, Word_MAX)));
    // Word data = GENERATE(as<Word>{} ,0, 0b11101, Word_MAX);
    Word data = 0b1010101010101010;
    // prepare file to read
    {
    std::ofstream out{fileName, std::ios::binary | std::ios::out};
    REQUIRE(out.is_open());
    out.write(reinterpret_cast<char*>(&data), sizeof(data)); 
    }
    // open file 
    std::ifstream binInput{fileName, std::ios::binary};
    // Load to Mem
    mem.Load(binInput);
    REQUIRE(reinterpret_cast<Word*>(mem.Data)[0] == data);
  }    
}

TEST_CASE("CPU")
{
  Mem mem{};
  CPU cpu{mem};
  int i = cpu.CS<<4;
  
  SECTION("PUSH POP"){

    mem[i  ] = 0b10110000; // MOV dest: AL
    Word x = mem[i+1] = GENERATE(1,10, Word_MAX);// value
    cpu.ExecuteNext();
    
    mem[i+2] = 0b01010'000; // PUSH reg: AX
    cpu.ExecuteNext();

    REQUIRE(mem.getWord(cpu.SP) == x);
  
    mem[i+3] = 0b01011'011; // POP reg: BX

    cpu.ExecuteNext();
    // value is still in memory but we don't care
    REQUIRE(mem.getWord(cpu.SP) != x); 
  }

  SECTION("ADD")
  {
    
  }
}

