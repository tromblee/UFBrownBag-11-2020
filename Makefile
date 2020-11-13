all:
	dasm *.asm -f3 -v0 -ocart.bin -lcart.lst -scart.sym

run:
	/Applications/Stella.app/Contents/MacOS/Stella cart.bin