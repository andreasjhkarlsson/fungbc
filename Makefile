EXECUTABLE = fungbc.exe
COMPILER = fsharpc
FLAGS = -O --tailcalls+ --crossoptimize+ --debug-
SRC = src/

FILES= \
	$(SRC)Misc.fs \
	$(SRC)Units.fs \
	$(SRC)Constants.fs \
	$(SRC)BitLogic.fs \
	$(SRC)MemoryCell.fs \
	$(SRC)Rom.fs \
	$(SRC)Tile.fs \
	$(SRC)Ram.fs \
	$(SRC)Clock.fs \
	$(SRC)IORegisters.fs \
	$(SRC)Interrupts.fs \
	$(SRC)Timer.fs \
	$(SRC)Gpu.fs \
	$(SRC)Input.fs \
	$(SRC)Mmu.fs \
	$(SRC)Register.fs \
	$(SRC)Instruction.fs \
	$(SRC)Cpu.fs \
	$(SRC)Gameboy.fs \
	$(SRC)Debugger.fs \
	$(SRC)GameboyWindow.fs \
	$(SRC)Program.fs

release: $(EXECUTABLE)

debug: FLAGS = --debug+ --optimize- --tailcalls- --crossoptimize-
debug: $(EXECUTABLE)

$(EXECUTABLE): $(FILES)
	$(COMPILER) $(FLAGS) $(FILES) -o $@

.PHONY: clean
clean:
	rm $(EXECUTABLE)
