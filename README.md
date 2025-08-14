# NotSoSimple8085

Board build with 8085 CPU PPI 2 * UART and COUNTER TIMER
The ATmega328 can handle the SDCARD to load and store files for the basic interpreter

I need help to get this working for fig-FORTH also ........

All KiCAD schematics and PCB is also available

Use a standard Arduino UNO to program the ATMEGA328 chip ! You can compile and burn the .INO file with the standard arduino IDE

https://www.arduino.cc/en/software/

The assembler i use to compile the asm files is ASMX 2.0  available at https://www.retrotechnology.com/memship/asmx.html

![WhatsApp Image 2025-08-14 at 17 45 14_44ab0a71](https://github.com/user-attachments/assets/972310ff-5b2c-4795-985a-4b49a625a096)


The EPROM Layout i use is as follow

			
	EPROM SPACE 		
			
0000	2FFF		BASIC INTERPRETER

3000	30FF		FIG-FORTH LOADER

3100	5FFF		FIG-FORTH

6000	7FFF		MONITOR


![WhatsApp Image 2025-08-14 at 13 00 33_66d2199a](https://github.com/user-attachments/assets/79fc8dae-f72e-4ccc-8bc6-3f35c1d37a2b)


![WhatsApp Image 2025-08-14 at 13 00 32_babeecaa](https://github.com/user-attachments/assets/abf9addb-267c-491f-a4df-c55dcb9b2668)






If there are some copyrights or wathever i really don't care about it !
i found everything on the internet for FREE !



