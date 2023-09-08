#include "../emulator_lib/8088CPU.h"

int main()
{ 
  Mem mem{};
  CPU cpu{mem};

  cpu.SS = 0x100;
  
  int i = cpu.CS<<4;
  mem[i  ] = 0b10110000; // MOV dest: AL
  mem[i+1] = 6;    // value

  mem[i+2] = 0b01010'000; // PUSH reg: AX
  
  mem[i+3] = 0b01011'011; // POP reg: BX

  cpu.ExecuteNext();
  cpu.ExecuteNext();

  cpu.PrintRegisters();

  std::cout << "Stack: \n";
  for(int i=0;i<10;i++)
    std::cout << i << ". " << std::bitset<8>(mem[cpu.physicalAddr(cpu.SS,cpu.SP-i)]) << '\n';
  std::cout << std::endl;
  
  cpu.ExecuteNext();
  cpu.PrintRegisters();
  return 0;
}
