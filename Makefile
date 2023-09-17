
export CFLAGS := -Wall -Wextra -pedantic -Weffc++ --std=c++17
export LIB_NAME := lib_emulator.a

ifdef DEBUG
CFLAGS := ${CFLAGS} -ggdb
CMAKE_FLAGS := -DCMAKE_BUILD_TYPE=RelWithDebInfo

endif

lib:
	cd emulator_lib; \
	${MAKE} all -s;

demo:
	echo ${CFLAGS};\
	cd demo; \
	${MAKE} all;

test:
	@# TFLAGS :=
	@cd tests; \
	cmake ${CMAKE_FLAGS} .; \
	cmake --build .; \
	./test; \


all: lib test demo

.PHONY: all lib demo test debug

