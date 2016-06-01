;// Reading a File							(ReadFile.asm)

;// Opens, reads, and displays a text file using
;// Procedures from Irvine32.lib.
INCLUDE Irvine32.inc						;// Inlcude Irvine32 library for use
INCLUDE macros.inc							;// Include macros library for use
;// Globals
FILE_BUFFER_SIZE = 1200000					;// Buffer for read file
LONGEST_ENG_WORD = 45						;// Longest known english word in any major dictionary is 45 characters long
LONGEST_EMORDLAP = 8						;// 8 because longest known emordnilap is 8 (desserts/stressed) and therefore no need 
										;// to keep searching past 8 lettered words
TRUE	 EQU  1	;// on
FALSE EQU  0	;// of
;//---------------------------------- DATA SEGMENT ----------------------------------------
.data
;// Buffers
FileBuffer   BYTE FILE_BUFFER_SIZE DUP(0)		;// Buffer to hold entire file
LineBuffer   BYTE LONGEST_ENG_WORD DUP(0)		;// Buffer to hold current line (1 Word/line) 
ReducedBuff  BYTE LENGTHOF FileBuffer DUP(0)
ReportBuffer BYTE SIZEOF ReducedBuff DUP(0)
;// File stuff								;// therefore 1 line == 1 word)
ReportFile BYTE "Report.txt",0				;// Name of report file
FileName	 BYTE 80 DUP(0)					;// Name of File user will input to read from
FileHandle HANDLE ?							;// File handle to check if successful open/close

;// Other needed VARS
CurrentWord	BYTE SIZEOF LineBuffer - 2 DUP(0)
CurrentWordRev BYTE SIZEOF CurrentWord DUP(0)

;// Report Header
;//
;// ReportHeader is a formatted string so that if opening successful,
;// write the header first thing
ReportHeader BYTE "Tomas Ochoa"						,0dh,0ah,
			   "Ochota39@Suny.Oneonta.Edu"			,0dh,0ah,
			   "CSCI 231: Assembly Language Programming" ,0dh,0ah,	
			   "12 November 2015"					,0dh,0ah,	
			   "Program 5: Emordnilaps and File I/O"	,0dh,0ah,
												 0dh,0ah,
			   "Emordnilaps in file :"				,0dh,0ah,
												 0dh,0ah

;// ReportHeader is a formatted string so that if opening successful,
;// write the header first thing												 0dh,0ah				

CharCount	   DWORD 0		;// a count to check how long a word is 
WordCount	   DWORD 0		;// a count to check how many words in buffer
ReturnCode   DWORD 0
BytesWritten DWORD 0

EndOfFile		 DWORD ?
StartOfFile	 DWORD ?
LastWordAddress DWORD ?

CurrentESILocation DWORD ?
CurrentEDILocation DWORD ?
saveLoc DWORD ?
TempLine BYTE LONGEST_ENG_WORD + 2 DUP (0) 
TempWord BYTE LONGEST_ENG_WORD DUP (0)
;//---------------------------------- CODE SEGMENT ----------------------------------------
.code
;// ---- Prototypes ----
Get_Line	PROTO,
	fileBuff:PTR BYTE,
	lineBuff:PTR BYTE  

Word_In_Line PROTO, 
	lineBuff:PTR BYTE,
	stringVar:PTR BYTE

Reverse_Word  PROTO,
	CurWord:PTR BYTE,
	RevWord:PTR BYTE 

Search_Buffer PROTO,
	SearchThis:PTR BYTE,
	SearchFrom:PTR BYTE

Amount_of_Words PROTO,
	SearchFileBuffer:PTR BYTE

Copy_Word_DogAlpha PROTO, 
	WordToCopy: PTR BYTE,
	SaveWordIn: PTR BYTE

Copy_Word_DogAlpha_Six PROTO, 
	WordToCopy2: PTR BYTE,
	SaveWordIn2: PTR BYTE

;// ---- Main ---- 
main PROC

;// Prompt user for name of file to read from
	mWrite  "Enter file name: "				
	mReadString  FileName 
	
;// Open File for reading
	mov	edx, OFFSET FileName 
	call OpenInputFile 
	mov  FileHandle, eax 
	
 ;// Check if opening was successful
	cmp  eax, INVALID_HANDLE_VALUE 
	jne  Open_OK
	mWrite <"Error Opening File...",0dh,0ah>
	jmp  Quit 
	
Open_OK: 
;// Read File to buffer 
	mov  edx, OFFSET FileBuffer 					;// points to buffer
	mov	ecx, FILE_BUFFER_SIZE					;// max bytes to read 
	call ReadFromFile							;// read the file 
	jnc	Check_Buff_Size						;// If carry buff too small 
	mWrite "Error reading file..."				;// If it didnt jump then error 
	mov	eax, FileHandle 
	call CloseFile 
	jmp	Quit 

Check_Buff_Size:
	cmp  eax, FILE_BUFFER_SIZE					;// buffer large enough?
	jb	Buff_Size_OK 
	mWrite <"Error: Buffer to small for the file...",0dh,0ah>
	jmp  Quit 

Buff_Size_OK:
	mWrite "File size: "
	call WriteDec								;// Display file size
	call Crlf 
	call Crlf 

;// Close reading file 
	mov	eax, FileHandle
	call CloseFile 

;//----------------------------------------------------------------------------------------
;// BEGIN REDUCE BUFFER
;// 
;// FACTS: 
;//	- The longest emordnilap known is 8 letters long
;// 	- This means that any word longer than 8 Do not qualify
;//  - If they dont quailify, that means we can remove it from the buffer 
;//	- To do this, let us read each word from the buffer and check if its > 8
;//  - If so copy go to next word
;//  - If not copy it to reduced buffer 

;// First find how many words are there in the FileBuffer 
	INVOKE Amount_of_Words, ADDR FileBuffer		;// Will return amount in ebx 
		mov	WordCount, ebx 				;// and address od EOF in edx	 
		mov	StartOfFile,eax	
		mov	EndOfFile, edx 

;// Initialize the reduced buffer
	mov	esi, OFFSET FileBuffer				;// esi => FileBuffer 
	mov	edi, OFFSET ReducedBuff				;// edi => ReducedBuff
	mov  CurrentESILocation, esi				;// save address to currentESIlocation 
	mov	CurrentEDILocation, edi				;// save address to currentEDIlocation 
		
;// Set Outer Loop for the FileBuffer 
	mov	ecx, WordCount				;// count

Outer_Loop_1: 		
;// First get the current line in buffer and store that line to 'LineBuffer'
	INVOKE Get_Line, CurrentESILocation, ADDR LineBuffer 				

;// Excract the word from the buffer and store it in variable 'CurrentWord'
	INVOKE Word_In_Line, ADDR LineBuffer, ADDR CurrentWord 

;// Get the length of the current word (in eax)
	INVOKE Str_Length, ADDR CurrentWord 
		mov	CharCount, eax 

;// Check if CharCount > 8
	cmp	CharCount, 8
	ja	Above_Eight
	jmp	EqualLess_Eight 	
	
	EqualLess_Eight: ;// if < 8
	;// Copy word to buffer
	;// MAKE OWN STR COPY TO ADD 0d0a
		INVOKE Copy_Word_DogAlpha, ADDR CurrentWord, CurrentEDILocation 	;// Store word in reduced buff 
		add	edi, CharCount			;// edi => last letter
		add 	edi, 2				;// edi => first letter of next (skipt '\r' & '\n')
		mov	CurrentEDILocation, edi 	;// save current location
		;// Get the CurrentESILocation correct
		add 	esi, CharCount			;// esi => last letter
		add 	esi, 2				;// esi => first letter of next (skipt '\r' & '\n')
		mov	CurrentESILocation, esi	;// save current location
		dec	ecx 
		mov	al, [esi]
		cmp  al, 0
		je	pause_it
		jne	Outer_Loop_1			;// loop	
	Above_Eight:	;// If > 8 
	;// Skip word (move esi to next word)
		add 	esi, CharCount			;// esi => last letter
		add 	esi, 2				;// esi => first letter of next (skipt '\r' & '\n')
		mov	CurrentESILocation, esi	;// save current location
		dec	ecx 
		mov	al, [esi]
		cmp  al, 0
		je	pause_it
		jne	Outer_Loop_1			;// loop

;// END OF BUFFER REDUCTION
;//----------------------------------------------------------------------------------------
;// BEGIN CHECKING FOR EMORDNILAPS 
;//
;// NOTES: 
;//		- I have a reduced file buffer with all the words == || < 8
;//		- To check for emordnilaps
;//			- Get the line in the buffer 
;//			- Get the word from that line
;//			- Copy and reverse that word 
;//			- Use that reversed copy and search for it in the reduced buffer
;//				- If word found 
;//					- It is an emordnilap
;//					- Write it to a report buffer 
;//					- go to next word 
;//				- If word not found
;//					- It is not an emordnilap
;//					- Do nothing (in terms of report buffer)
;//					- go to next word 
;//			-Repeat 
pause_it:
;// First get amount of word from Reduced Buffer 
	INVOKE Amount_of_Words, ADDR ReducedBuff	;// Will return amount in ebx 
		mov	WordCount, ebx 				;// and address od EOF in edx	 
		mov	StartOfFile,eax	
		mov	EndOfFile, edx 

;// Initialize esi and edi 
	mov	esi, OFFSET ReducedBuff				;// esi => ReducedBuff 
	mov	edi, OFFSET ReportBuffer				;// edi => ReportBuff

;// Save the initial locations of esi and edi and set the loop counter
	mov  CurrentESILocation, esi				;// save address to currentESIlocation 
	mov	CurrentEDILocation, edi				;// save address to currentEDIlocation 
	mov	ecx, WordCount						;// count

;// Check for emordnilaps and if one is found, write it to a seperate buffer for the report file
Emordnilap_Loop:
	;// Get line in reduced buffer 
		INVOKE Get_Line, CurrentESILocation, ADDR LineBuffer 		
		
	;// Get Word from the reduced buffer 
		INVOKE Word_In_Line, ADDR LineBuffer, OFFSET CurrentWord 
		
	;// Reverse the word 
		INVOKE Reverse_Word, ADDR CurrentWord, ADDR CurrentWordRev 
		
	;// Search the reversed word inside the reduced buffer 
		INVOKE Search_Buffer, ADDR CurrentWordRev, ADDR ReducedBuff 
			mov	ReturnCode, eax				;// If found, eax == 0001, else eax == 0000
	
	;// Check Return code to see if found or not 
		.IF (ReturnCode == TRUE)

		;// Copy the word to the report buffer
			INVOKE Copy_Word_DogAlpha_Six, ADDR CurrentWord, CurrentEDILocation 	;// Store word in reduced buff 

		;// Get the length of the Current word 
			INVOKE Str_Length, ADDR CurrentWord		;// save char count from eax 
				mov	CharCount, eax 

		;// Get the CurrentEDILocation correct 
			add	edi, CharCount			;// edi => last letter
			add 	edi, 2				;// edi => first letter of next (skipt '\r' & '\n')
			mov	CurrentEDILocation, edi 	;// save current location

		;// Get the CurrentESILocation correct
			add 	esi, CharCount			;// esi => last letter
			add 	esi, 2				;// esi => first letter of next (skipt '\r' & '\n')
			mov	CurrentESILocation, esi	;// save current location

		;// loop up (loop only does -128 to +128 so out of range if use menmonic loop)
			dec	ecx 
			jne	Emordnilap_Loop

		.ELSE
		;// Dont Write to report buffer 
			INVOKE Str_Length, ADDR CurrentWord		;// save char count from eax 
				mov	CharCount, eax 

		;// Get the CurrentESI location correct
			add 	esi, CharCount			;// esi => last letter
			add 	esi, 2				;// esi => first letter of next (skipt '\r' & '\n')
			mov	CurrentESILocation, esi	;// save current location

		;// Get the right edi location
			mov	edi, CurrentEDILocation  

		;// loop up (loop only does -128 to +128 so out of range if use menmonic loop)
			dec	ecx 
			jne	Emordnilap_Loop
		.ENDIF

;// STOP CHECKING FOR EMORDNILAPS 
;//----------------------------------------------------------------------------------------
;// BEGIN WRITE TO FILE 

;// Create new File for writing to
	mov	edx, OFFSET ReportFile 
	call	CreateOutputFile 
	mov	FileHandle, eax 
	
;// Check for errors
	cmp	eax, INVALID_HANDLE_VALUE 				;// Error oppening?
	jne	Create_OK								;// If not, skip
	mWrite <"Error creating file...",0dh,0ah>
	mov	eax, FileHandle 						;// else continue to close and exit
	call CloseFile 
	jmp	Quit 

Create_OK:
;// Write the header first 
	mov	eax, FileHandle 
	mov	edx, OFFSET ReportHeader  
	mov	ecx, LENGTHOF ReportHeader 
	call WriteToFile 
	mov	BytesWritten, eax						;// Save characters written
	
;// Modify the report buffer to show 6 words per line 
	mov	esi, OFFSET ReportBuffer 
	mov  ecx, LENGTHOF ReportBuffer		;// use as outer loop counter 
	mov	edx, 0						;// use edx as inner loop counter 

starter:
	mov	al, [esi]
	cmp	al, 2ch		;// is al = ','? 
	je	count_six 	;// yes, go to count six
	inc	esi			;// go to next character 
	cmp  al, 0
	je  end_buff 
	LOOP starter		;// loop 
	
;// count every six words 
	count_six:		
		add  edx, 1	;// word in line 
		inc	esi 		;// esi = 'space'
		inc	esi		;// esi = '[next char]'
		sub  ecx, 2	;// manually skipped 2, compensate loop	
		cmp	edx, 6	;// counted 6 words?
		je	Add_CRLF	;// yes, go to add a nl 	
	LOOP starter		;// loop

;// replace the ',' and '[space]' with '\r' and '\n'
	Add_CRLF:
	;// go back to the space (compensate loop counter)	
		dec esi		
		add ecx, 1			
	;// replace the '[space]' with '\n'
		mov bl, 0ah 
		mov [esi], bl 
	;// go back to the comma (compensate loop counter)		
		dec esi
		add ecx, 1	 
	;// replace the ',' with '\r'
		mov bl, 0dh 
		mov [esi], bl 
		mov	edx, 0		;// reset inner counter 
	LOOP starter 

;// end of buffer 
end_buff:	

;// Get how many ACTUAL ascii characters there are in the report buffer
	INVOKE Str_length, ADDR ReportBuffer 
		add BytesWritten, eax 

;// Now Write the emordnilaps to the report buffer 
	mov	eax, FileHandle 
	mov	edx, OFFSET ReportBuffer 
	mov	ecx, BytesWritten  
	call	WriteToFile 

;// Show the user how many bytes were written
	mov  BytesWritten, eax 
	mWrite "Bytes written to file: "
	mov	eax, BytesWritten
	call WriteDec 
	call Crlf 

;// Close the file for writing 
	mov	eax, FileHandle 
	call	CloseFile 

;// STOP WRITING TO FILE
;//----------------------------------------------------------------------------------------
;// Quit 
Quit:
	mWrite <"Goodbye!",0dh,0ah>
	call Crlf 
	exit 
main ENDP
;//-------------------------------------------- CUSTOM PROCEDURES ------------------------------------
;// Name:	  Amount_of_Words
;//
;// Purpose:  Purpose of this procedure is to check how many words there are in the buffer 
;// 
;// Recieves: SearchFileBuffe, The address of a buffer 
;//		    WrdAmnt, the variable where to store the count 		     
;//		    
;// Returns:  A count of how many words
;//----------------------------------------------------------------------------------------------
Amount_of_Words PROC USES esi edi ecx,
	SearchFileBuffer:PTR BYTE
	
	mov	esi, SearchFileBuffer 
	mov	ecx, FILE_BUFFER_SIZE 
	mov	ebx, 0
	
	;// Count how many words there are 
Start_one:
	mov	al, [esi]
	cmp	al, 0
	je	Is_Null 
	cmp	al, 0dh 
	je	Is_CR
	jne	Is_Char 
Is_CR:
	inc	esi 
	mov	al, [esi]
	cmp	al, 0ah 
	je	Is_LF 
	jne	Is_Char 
Is_LF: 
	add	ebx, 1 
	inc	esi 
	LOOP	Start_one 	
Is_Char:
	inc	esi 
	LOOP Start_one 
Is_Null:
	mov	edx, esi 
	mov	edi, SearchFileBuffer 
	mov	eax, edi 
	ret 
Amount_of_Words ENDP 
;//----------------------------------------------------------------------------------------------
;// Name:    Copy_Word_DogAlpha
;//
;// Purpose: This procedure is simply a redefinition of Irvine's Str_copy. I need my proc to add
;//		  a 0d0a at the end of string instead of 0000. This helps me during reduction, because
;//		  allthough in memory it looks ok, the 00 00s between words makes the program think the 
;//		  eof is there when in reality it should be a new line 
;//
;// Recives: WordToCopy, the address of word
;//		   SaveWordIn, address of where to save	
;//----------------------------------------------------------------------------------------------
Copy_Word_DogAlpha PROC USES eax ecx esi edi, 
	WordToCopy:PTR BYTE,
	SaveWordIn:PTR BYTE 
	
;// Get character count of word to copy and store it in ecx (count)
	INVOKE Str_Length, WordToCopy 
	mov	ecx, eax 

;// Load stings and copy
	mov	esi, WordToCopy		;// sournce
	mov	edi, SaveWordIn		;// target 
	cld						;// Direction = forward 
	rep	movsb				;// copy string 

;// After the run, edi = element after last letter so add a '\r' and '\n'
	mov	al, 0dh 
	mov  [edi], al 
	inc  edi 
	mov	al, 0ah
	mov	[edi], al 
;// Return to program call
	ret 
Copy_Word_DogAlpha ENDP
;//----------------------------------------------------------------------------------------------
;// Name:    Copy_Word_DogAlpha_Six
;//
;// Purpose: This procedure is simply a redefinition of Irvine's Str_copy. I need my proc to add
;//		  a 0d0a at the end of string instead of 0000. This helps me during reduction, because
;//		  allthough in memory it looks ok, the 00 00s between words makes the program think the 
;//		  eof is there when in reality it should be a new line 
;//
;// Recives: WordToCopy, the address of word
;//		   SaveWordIn, address of where to save	
;//----------------------------------------------------------------------------------------------
Copy_Word_DogAlpha_Six PROC USES eax ecx esi edi, 
	WordToCopy2:PTR BYTE,
	SaveWordIn2:PTR BYTE 
	
;// Get character count of word to copy and store it in ecx (count)
	INVOKE Str_Length, WordToCopy2 
	mov	ecx, eax 

;// Load stings and copy
	mov	esi, WordToCopy2		;// sournce
	mov	edi, SaveWordIn2		;// target 
	cld						;// Direction = forward 
	rep	movsb				;// copy string 

;// After the run, edi = element after last letter so add a '\r' and '\n'
	mov	al, 2ch 
	mov  [edi], al 
	inc  edi 
	mov	al, 20h
	mov	[edi], al 
;// Return to program call
	ret 
Copy_Word_DogAlpha_Six ENDP
;//----------------------------------------------------------------------------------------------
;// Name:    Get_Last_Word_ADDR
;//
;// Purpose: Gets the address of the last word of the file 
;//
;// Recives: EOFBuff, the address of the file null terminator of a buffer 
;//
;// Returns: edx, Reurns the address of the last word in the file 
;//----------------------------------------------------------------------------------------------
Get_Last_Word_ADDR PROC,
	EOFBuff:PTR BYTE 
	
	mov edi, EOFBuff 
	mov	ebx, 0
	dec	edi 			;// edi => '\n' 
	dec	edi 			;// edi => '\r'
gutter:
	dec	edi				;// edi => ['last letter of file']
	mov	al, [edi]		;// check if its a new line
	.IF(al != 0ah) 		;// if edi == ['\n']
		add	ebx, 1		;// we've reached the end of a new word 
		jmp gutter 
	.ELSE
	;// mark address of last word in buffer 
		inc	edi 		;// MEM: '\r','\n','[first char of word]' 
		mov	edx, edi 	;//   move edi  ^    to        ^            
		jmp	ender 
	.ENDIF
ender:
	ret 
Get_Last_Word_ADDR ENDP 
;//--------------------------------------------PROCEDURES----------------------------------------
;// Name:	 Get_Line 
;//
;// Purpose: Saves the current line at from any buffer to a dedicated line buffer 
;// 
;// Recieves: EDI, The address of the fileBuffer from where to search, and the address
;//		    of lineBuffer to store the current line 
;//----------------------------------------------------------------------------------------------
Get_Line	PROC USES eax ebx ecx esi edi,
	fileBuff:PTR BYTE,				;// FileBuffer
	lineBuff:PTR BYTE				;// LineBuffer
	
	mov	esi, fileBuff 
	mov	edi, lineBuff
	mov	ecx, LONGEST_ENG_WORD + 2	;// Since each line has 1word/line and longest
								;// english word in any major ditionary is 45 (+ 2 for 
								;// 'CR' and 'LF'), no need to search for a word longer than that
	;// Begin to load the line
Get_Line_Start:
	mov	al, [esi]		;// al = [contents of esi]
	cmp	al, 0		;// Compare al with 0
	je	Is_NULL		;// jump to label "Is_NULL" if al == 0

Check_CR:
	mov	al, [esi]
	cmp	al, 0dh		;// Compare al with '\r'
	jne  Copy_Element 
	je	Is_New_Line	;// jump to label "Is_New_Line" if al == '\r'
	

Copy_Element:
	mov	[edi], al
	inc	esi 
	inc	edi 
	LOOP	Check_CR 
	
Is_New_Line:			;// Current line is a new line 
	;// add a '\r'
	mov	bl, 0dh
	mov	[edi], bl 
	inc	edi 
	;// add a '\n'
	mov	bl, 0ah
	mov	[edi], bl 
	inc	edi 

Is_NULL:				;// Current line is null term 
	mov	bl, 0
	mov	[edi], bl 
	jmp here_1

here_1:	
	ret	
Get_Line	ENDP 
;//----------------------------------------------------------------------------------------------
;// Name:    Reverse_Word 
;//
;// Purpose: Reverses any string passed into the procedure
;//
;// Recives: CurWord, the address of the word to reverse 
;// 		   RevWord, the address of the variable to store it in
;//----------------------------------------------------------------------------------------------
Reverse_Word PROC USES eax ecx esi edi,
	CurWord: PTR BYTE,
	RevWord: PTR BYTE 

;// Get length of current word 
	INVOKE Str_Length, CurWord	;// String length is in eax 

;// Load Strings and set count 
	mov	esi, CurWord 			;// Source string 
	mov	edi, RevWord			;// EDI => RevWord 
	mov	ecx, eax				;// count = StrLength 

;// Get esi to point to last element
	add	esi,	eax				;// Add esi address by string length (esi => null )
	dec	esi					;// esi now => last character
	
;// Begin copying elements in reverse
Reverse_Loop:				
	mov	al, [esi]
	mov	[edi], al
	dec	esi 
	inc	edi 
	LOOP Reverse_Loop 

	;// WHen program gets to here, RevWord will be reversed but is not null terminated so do so
	mov	al, 0
	mov	[edi], al 

	;// Exit proc and return to program call
	ret
Reverse_Word ENDP
;//----------------------------------------------------------------------------------------------
;// Name:    Search_Buffer 
;//
;// Purpose: To search for a word in a buffer 
;//
;// Recives: esi edi, take in the file buffer and the word to look for in the file buffer 
;//
;// Returns: eax, a bool variable that says wether or not found 
;//----------------------------------------------------------------------------------------------
Search_Buffer	PROC USES ebx ecx edx esi edi,
	SearchThis:PTR BYTE,
	SearchFrom:PTR BYTE	

;// First get the amount of words in the buffer passed in and set return val as count
	INVOKE Amount_Of_Words, SearchFrom 	
		mov	ecx, ebx 
SBL1:
;// Get the current line of the buffer passed in
	INVOKE Get_Line, SearchFrom, ADDR TempLine
		
;// Get the word from the current buffer 
	INVOKE Word_In_Line, ADDR TempLine, ADDR TempWord
	
;// Compare the word in line with the word passed in 
	INVOKE Str_compare, ADDR TempWord, SearchThis 
	je found
	jne not_found
found:
	mov	eax, 0001
	ret 
not_found:
;// Move esi to next word
	mov	esi, SearchFrom 
	INVOKE Str_length, ADDR TempLine 
	add esi, eax 
	mov eax, 0000
	mov	SearchFrom, esi 
	LOOP SBL1 
	ret
Search_Buffer	ENDP 
;//----------------------------------------------------------------------------------------------
;// Name:    Set_ADDR_Word
;//
;// Purpose: Takes in 2 addresses, the first it an address of the begining of a word.
;// 		 The second is the address of a variable to store in 
;//
;// Recives: begginingADDR, the address of the first letter of a string
;//			 StringVar3,	the variable where to store 
;// Returns: 
;//----------------------------------------------------------------------------------------------
Set_ADDR_Word PROC,
	begginingADDR:PTR BYTE,
	StringVar3:PTR BYTE,
	StringVar3Length:DWORD
	
	mov	esi, begginingADDR 
	mov	edi, StringVar3
	mov	ecx, StringVar3Length 
	
loop_er:
		mov	al, [esi]
		mov	[edi], al 
		inc	esi 
		inc	edi 
	LOOP loop_er 
	mov	edi, 0
	ret 
Set_ADDR_Word ENDP 
;//----------------------------------------------------------------------------------------------
;// Name:    Word_In_Line
;//
;// Purpose: Copies the the word held in the LineBuffer to a string value passed into the procedure
;//
;// Recives: esi edi, the Line Buffer and the variable to store string in 
;//----------------------------------------------------------------------------------------------
Word_In_Line PROC USES eax ecx esi edi,
	lineBuff: PTR BYTE, 
	stringVar:PTR BYTE 		
	
	;// Get the word held in line buffer and copy it to stringVar
	mov	esi, lineBuff						;// ESI => LineBuffer (contains current line)
	mov	edi, stringVar 					;// EDI => currentWOrd 
	INVOKE Str_length, lineBuff				;// Invoke Str_Length to know how long lineBuff is 
	mov	ecx,	eax 							;// Save eax to ecx to set count 	
	sub	ecx, 2							;// - 2 for '\r' and '\n'

	;// Copy
LTW_Start:
	mov	al, [esi]
	mov	[edi], al 
	inc	esi 
	inc	edi 
	LOOP LTW_Start 

	;// At this point edi needs to be null terminated 
	mov	al, 0
	mov	[edi], al 
	;// Return to program call 
	ret  
Word_In_Line ENDP 
;//--------------------------------------------END PROCEDURES------------------------------------
END main