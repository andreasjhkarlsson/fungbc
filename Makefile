EXECUTABLE = fungbc.exe
COMPILER = fsharpc
FLAGS = -O --tailcalls+ --crossoptimize+ --debug-
SRC = src/

RES = res/
RESCOMPILER = resgen
RESX = $(RES)resources.resx
RESOURCES = $(RES)Resources.resources

FILES= \
	$(SRC)Misc.fs \
	$(SRC)Units.fs \
	$(SRC)Constants.fs \
	$(SRC)Resource.fs \
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

$(EXECUTABLE): $(FILES) $(RESOURCES)
	$(COMPILER) $(FLAGS) $(FILES) --resource:$(RESOURCES) -o $@

$(RESOURCES): $(RESX)
	cd $(SRC); $(RESCOMPILER) ../$(RESX) ../$(RESOURCES)

.PHONY: clean
clean:
	rm $(EXECUTABLE)
	rm $(RESOURCES)
