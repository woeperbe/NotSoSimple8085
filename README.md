# NotSoSimple8085

Board build with 8085 CPU PPI 2 * UART and COUNTER TIMER
The ATmega328 can handle the SDCARD to load and store files for the basic interpreter

I need help to get this working for fig-FORTH also ........

All KiCAD schematics and PCB is also available

Use a standard Arduino UNO to program the ATMEGA328 chip ! You can compile and burn the .INO file with the standard arduino IDE

https://www.arduino.cc/en/software/

The assembler i use to compile the asm files is ASMX 2.0  available at https://www.retrotechnology.com/memship/asmx.html



<img width="2441" height="1704" alt="Screenshot 2025-08-03 075859" src="https://github.com/user-attachments/assets/96895ca6-4d7f-4914-bfae-1ee7afd97078" />


The EPROM Layout i use is as follow

			
	EPROM SPACE 		
			
0000	2FFF		BASIC INTERPRETER

3000	30FF		FIG-FORTH LOADER

3100	5FFF		FIG-FORTH

6000	7FFF		MONITOR




<img width="688" height="692" alt="image" src="https://github.com/user-attachments/assets/ffa6785f-8d12-4707-9790-85751cf9a932" />


<img width="2268" height="1623" alt="Screenshot 2025-08-03 075805" src="https://github.com/user-attachments/assets/2d3493c9-6f8f-41c2-b8a8-1108d471b30a" />

<img width="2298" height="1783" alt="Screenshot 2025-08-03 075502" src="https://github.com/user-attachments/assets/c3a93f8f-cb5c-4d9f-aa00-77b849914f5c" />



If there are some copyrights or wathever i really don't care about it !
i found everything on the internet for FREE !



