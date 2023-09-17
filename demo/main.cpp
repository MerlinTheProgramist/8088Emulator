#include "../emulator_lib/8088CPU.h"


int main()
{ 
  Mem mem{};
  CPU cpu{mem};

  
  cpu.SS = 0x100;
  
  int i = (cpu.CS<<4)+cpu.IP;
  mem[i  ] = 0b10111000; // MOV dest: AL
  mem[++i] = 0xAD;    // value
  mem[++i] = 0xDE;
  cpu.ExecuteNext();

  mem[++i] = 0b01010'000; // PUSH reg: AX
  cpu.ExecuteNext();

  cpu.PrintRegisters();


  std::cout << "Stack: \n";
  for(int i=0;i<10;i++)
      std::cout << i << ". " << std::hex << +mem[cpu.physicalAddr(cpu.SS,cpu.SP+i)] << std::endl;
  std::cout << std::endl;
  
  mem[++i] = 0b01011'011; // POP reg: BX
  cpu.ExecuteNext();
  cpu.PrintRegisters();
  for(int i=-2;i<10-2;i++)
    std::cout << i << ". " << std::hex << +mem[cpu.physicalAddr(cpu.SS,cpu.SP+i)] << std::endl;
  std::cout << std::endl;
  return 0;
}
