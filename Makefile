CC=fsharpc
CF=-O --tailcalls+ --crossoptimize+ --debug-

FILES= \
	src/Misc.fs \
	src/Units.fs \
	src/Constants.fs \
	src/BitLogic.fs \
	src/MemoryCell.fs \
	src/Rom.fs \
	src/Tile.fs \
	src/Ram.fs \
	src/Clock.fs \
	src/IORegisters.fs \
	src/Interrupts.fs \
	src/Timer.fs \
	src/Gpu.fs \
	src/Input.fs \
	src/Mmu.fs \
	src/Register.fs \
	src/Instruction.fs \
	src/Cpu.fs \
	src/Gameboy.fs \
	src/Debugger.fs \
	src/GameboyWindow.fs \
	src/Program.fs


fungbc.exe: $(FILES)
	$(CC) $(CF) $(FILES) -o fungbc.exe


