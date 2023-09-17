LIB_NAME := ./lib_emulator.a
all:
	g++ ${CFLAGS} -c arthmeticAndLogic_instructions.cpp -o arthmAndLogic.o
	g++ ${CFLAGS} -c dataTransmission_instuctions.cpp -o dataTrans.o
	ar rcs ${LIB_NAME} arthmAndLogic.o dataTrans.o flags.o
	
	echo -e "\033[0;32m[MAKE] Compiled 8088emulator lib Succesfully"
