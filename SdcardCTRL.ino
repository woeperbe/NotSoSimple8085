#include <SdFat.h>
#include <avr/wdt.h>

#define		MAJOR_VERSION   3
#define		MINOR_VERSION   5
#define		PATCH_VERSION   2

#define		SDCARD_READY		4
#define		ACTIVITY_LED	2
#define		TEMPBUFFER_SIZE 128


/*  PROTOTYPE PINS

#define		DTR_PIN         A1
#define	    DSR_PIN         A2
#define     RTS_PIN         A3
#define     CTS_PIN         A4
*/

// JLCPCB Version 2 Pins
#define		DTR_PIN         A2
#define	    DSR_PIN         A4

#define     RTS_PIN         A1
#define     CTS_PIN         A3

/* JLCPCB Version 3 Pins
#define	    DSR_PIN         A0
#define     RTS_PIN         A1
#define		DTR_PIN         A2
#define     CTS_PIN         A3
#define     SDA_PIN         A4
#define		SCL_PIN         A5
*/

const char compile_date[] = __DATE__ " " __TIME__;

SdFat SD;

const int	chipSelect = 10;
char		label[20];
int			count;
int			timeout;
bool		CTS;
uint32_t	position;
int			bytes;
int			linelen;
int			written;
char		tempbuffer[TEMPBUFFER_SIZE];
char		TempString[80];
char		filename[20];
uint32_t	cardSectorCount = 0;
uint8_t		sectorBuffer[512];


/******************************************************
******* SETUP *****************************************
*******************************************************
*/
void setup() {
	
	Serial.begin(9600, SERIAL_8E2);
	Serial.flush();
	
	pinMode(SDCARD_READY, OUTPUT);
	digitalWrite(SDCARD_READY, LOW);
	
	pinMode(CTS_PIN, INPUT_PULLUP);
	pinMode(RTS_PIN, OUTPUT);

	pinMode(DSR_PIN, OUTPUT);
	digitalWrite(DSR_PIN, LOW); // DSR is low

	pinMode(DTR_PIN, INPUT_PULLUP);

	pinMode(ACTIVITY_LED, OUTPUT);
	digitalWrite(ACTIVITY_LED, LOW);


	if (SD.begin(chipSelect)) digitalWrite(SDCARD_READY, HIGH);

}

/******************************************************
******* LOOP *****************************************
*******************************************************
*/
void loop() {

	digitalWrite(ACTIVITY_LED, LOW);
	Serial.println();
	Serial.print(F("SD:>"));

	digitalWrite(RTS_PIN, LOW);
	char* cmd = read_user_cmd(tempbuffer, TEMPBUFFER_SIZE);
	digitalWrite(RTS_PIN, HIGH);

	if (strncmp_P(cmd, PSTR("dir"), 3) == 0)
	{
		ShowDirectory();
	}
	else if (strncmp_P(cmd, PSTR("files"), 5) == 0)
	{
		ShowDirectory();
		Serial.println();
		Serial.print("%\b ");
	}
	else if (strncmp_P(cmd, PSTR("read "), 5) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		timeout = 0;
		strcpy(filename, cmd + 5);
		strupr(filename);
		File dataFile = SD.open(filename);
		delay(1000);
		if (dataFile) {
			while (dataFile.available()) {
				memset(tempbuffer, 0, TEMPBUFFER_SIZE);
				bytes = dataFile.readBytesUntil('\r', tempbuffer, TEMPBUFFER_SIZE);
				strcat(tempbuffer, "\r");
				for (position = 0; position < strlen(tempbuffer); position++) {
					wait_for_CTS();
					Serial.print(tempbuffer[position]);
					if (tempbuffer[position] == 13) delay(20);
					delay(5);
					if (timeout == 15000) break;
				};
			}
			dataFile.close();
			Serial.println();
		}
		else {
			Serial.println("File not found");
		}
	}
	else if (strncmp_P(cmd, PSTR("dump "), 5) == 0)
	{
		int offset = 0;
		digitalWrite(ACTIVITY_LED, HIGH);
		strcpy(filename, cmd + 5);
		strupr(filename);
		File dataFile = SD.open(filename);
		if (dataFile) {
			while (dataFile.available()) {
				memset(tempbuffer, 0, TEMPBUFFER_SIZE);
				bytes = dataFile.readBytes(tempbuffer, TEMPBUFFER_SIZE);
				dump_buffer(offset, tempbuffer, bytes); offset += bytes;
			}
		}
	}
	else if (strncmp_P(cmd, PSTR("write "), 6) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		strcpy(filename, cmd + 6);
		strupr(filename);
		File myFile = SD.open(filename, FILE_WRITE);
		if (myFile)
			while (true)
			{
				memset(tempbuffer, 0, TEMPBUFFER_SIZE);
				Serial.setTimeout(300000);
				digitalWrite(RTS_PIN, LOW);
				linelen = Serial.readBytesUntil('\r', tempbuffer, TEMPBUFFER_SIZE);
				digitalWrite(RTS_PIN, HIGH);
				written = myFile.print(tempbuffer);
				myFile.print("\r");
				if (tempbuffer[0] == 0) break;
				linelen++;
				written++;
			};
		myFile.close();
		digitalWrite(RTS_PIN, LOW);
	}
	else if (strncmp_P(cmd, PSTR("del "), 4) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		if (!SD.remove(cmd + 4))
			Serial.println("Error deleting file");
		Serial.print("%\b ");

	}
	else if (strcmp_P(cmd, PSTR("format")) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		SD.format();
		if (!SD.begin(chipSelect)) Serial.print("SD Card Error");
		Serial.print("Card is ");
		SD.printFatType(&Serial);
		Serial.println();
		Serial.print("%\b ");
	}
	else if (strcmp_P(cmd, PSTR("reset")) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		 asm volatile("  jmp 0");	
	}
	else if (strcmp_P(cmd, PSTR("version")) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		sprintf(TempString, "SDCard Controller Ver %d.%d.%d Build %s", MAJOR_VERSION, MINOR_VERSION, PATCH_VERSION, compile_date);
		Serial.println(TempString);
	}
	else if (strcmp_P(cmd, PSTR("?")) == 0)
	{
		digitalWrite(ACTIVITY_LED, HIGH);
		Serial.print(F("Valid commands: reset, format, dir, read, write, del, dump, version"));
		Serial.println();
	}
	else if (cmd[0] != 0)
	{
		Serial.print(F("What ? "));
		Serial.print(cmd);
	}

}

/******************************************************
******* SETUP *****************************************
*******************************************************
*/
char* read_user_cmd(void* buffer, int buflen)
{
	char* buf = (char*)buffer;
	byte l = 0;
	int i;
	do
	{
		i = Serial.read();

		if ((i == 13 || i == 10))
		{
			Serial.println(); break;
		}
		else if (i == 27)
		{
			l = 0; Serial.println(); break;
		}
		else if (i == 8)
		{
			if (l > 0)
			{
				Serial.write(8); Serial.write(' '); Serial.write(8); l--;
			}
		}
		else if (isprint(i) && l < buflen - 1)
		{
			buf[l++] = i; Serial.write(i);
		}
	} while (true);

	while (l > 0 && isspace(buf[l - 1])) l--;
	buf[l] = 0;
	return buf;
}


/******************************************************
******* WAIT FOR CTS *********************************
*******************************************************
*/
static bool wait_for_CTS(void)
{
	timeout = 0;

	CTS = digitalRead(CTS_PIN);
	digitalWrite(LED_BUILTIN, !CTS);
	if (CTS == 0) return(false);
	// Wait for CTS to go low
	do {
		CTS = digitalRead(CTS_PIN);
		digitalWrite(LED_BUILTIN, CTS);
		delay(1);
		timeout++;
		if (timeout == 15000) return (true);
	} while (CTS != 0);
	return(false);
}

/******************************************************
******* DUMP HEX BYTE *********************************
*******************************************************
*/
void print_hex(byte b)
{

	sprintf(TempString, "%02X", b);

	char* buf = TempString;
	while (*buf)
	{
		wait_for_CTS();
		Serial.write(*buf++);
	}
}

/******************************************************
******* DUMP BUFFER  **********************************
*******************************************************
*/
void dump_buffer(int offset, char* buf, int n)
{
	int i = 0;
	while (i < n)
	{
		print_hex((offset + i) / 256);
		print_hex((offset + i) & 255);
		wait_for_CTS();
		Serial.write(':');

		for (int j = 0; j < 16; j++)
		{
			if ((j & 7) == 0) {
				wait_for_CTS();
				Serial.write(' ');
			};
			if (i + j < n) print_hex(buf[i + j]); else {
				wait_for_CTS();
				Serial.print(F("  "));
			};
			wait_for_CTS();
			Serial.write(' ');
		}
		wait_for_CTS();
		Serial.write(' ');
		for (int j = 0; j < 16; j++)
		{
			if ((j & 7) == 0) {
				wait_for_CTS();
				Serial.write(' ');
			};
			if (i + j < n) {
				wait_for_CTS();
				Serial.write(isprint(buf[i + j]) ? buf[i + j] : '.');
			}
			else {
				wait_for_CTS();
				Serial.write(' ');
			};
		}
		wait_for_CTS();
		Serial.println();
		i += 16;
	}
}

/******************************************************
******* SHOW DIRECTORY ********************************
*******************************************************
*/
static void ShowDirectory(void) {
	digitalWrite(ACTIVITY_LED, HIGH);
	count = 0;
	char FileName[20];
	if (!SD.begin(chipSelect)) Serial.print("SD Card Error");
	File root = SD.open("/");
	uint32_t TotalSpace = 0;
	while (true)
	{
		File myFile = root.openNextFile();
		if (!myFile) {
			// no more files
			break;
		}
		if (!myFile.isDirectory()) {
			int length = myFile.getName(FileName, 13);
			Serial.print(FileName);
			if (length < 8) Serial.print("\t");
			Serial.print("\t\t");
			Serial.println(myFile.size(), DEC);
			TotalSpace += myFile.size();
		}
		else count--;
		count++;
		myFile.close();
	}
	if (count == 0) Serial.println(F("\t\tNo files.\r\n"));
	if (count != 0) {
		Serial.print("Total of ");
		Serial.print(TotalSpace);
		Serial.print(" bytes in ");
		Serial.print(count, DEC);
		Serial.print(" File(s)\r\n");
	};

	unsigned long long  Freespace = SD.bytesPerCluster() * SD.freeClusterCount();
	uint32_t FRee;
	Serial.print("Free Space is ");
	if (Freespace > 1000000) {
		FRee = Freespace / 1000000;
		Serial.print(FRee);
		Serial.print(" MBytes");
		return;
	};
	if (Freespace > 1000) {
		FRee = Freespace / 1000;
		Serial.print(FRee);
		Serial.print(" KBytes");
		return;
	};
	FRee = Freespace;
	Serial.print(FRee);
	Serial.print(" Bytes");
}
