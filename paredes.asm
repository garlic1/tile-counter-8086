.model large
.stack
	BS EQU 08H 		; ASCII "Backspace"
	CR EQU 0DH 		; ASCII "Carriage Return" 
	LF EQU 0AH 		; ASCII "Line Feed"
	ESCAPE EQU 1AH	; ASCII "Escape"
	PONTO EQU 2EH	; ASCII "."
	
	MAX_STR EQU 13


.data	
;	  Color encoding in the standard palette:
;     0000 (0h) = Black             1000 (8h) = Dark Gray
;     0001 (1h) = Blue              1001 (9h) = Light Blue
;     0010 (2h) = Green             1010 (Ah) = Light Green
;     0011 (3h) = Cyan              1011 (Bh) = Light Cyan
;     0100 (4h) = Red               1100 (Ch) = Light Red
;     0101 (5h) = Magenta           1101 (Dh) = Light Magenta
;     0110 (6h) = Brown             1110 (Eh) = Yellow
;     0111 (7h) = Light Gray        1111 (Fh) = White
;     Note: colors from 8h to Fh can only be used for text, as
;     their codes have bit 7 = 1, which causes the text to blink.

	qtde_preto db 0
	qtde_azul  db 0
	qtde_verde db 0
	qtde_ciano  db 0
	
	qtde_vermelho db 0
	qtde_magenta  db 0
	qtde_marrom db 0
	qtde_cinzaclaro  db 0
	
	qtde_cinzaescuro db 0
	qtde_azulclaro  db 0
	qtde_verdeclaro db 0
	qtde_cianoclaro db 0
	
	qtde_vermelhoclaro db 0
	qtde_magentaclaro db 0
	qtde_amarelo db 0
	qtde_branco db 0	
	
	
	preto db "Black - ",0,0,0,0,0
	azul db "Blue - ",0,0,0,0,0
	verde db "Green - ",0,0,0,0,0
	ciano db "Cyan - ",0,0,0,0,0

	vermelho db "Red - ",0,0,0,0,0
	magenta db "Magenta - ",0,0,0,0,0
	marrom db "Brown - ",0,0,0,0,0
	cinzaclaro db "Light gray - ",0,0,0,0,0

	cinzaescuro db "Dark gray - ",0,0,0,0,0
	azulclaro db "Light blue - ",0,0,0,0,0
	verdeclaro db "Light green - ",0,0,0,0,0
	cianoclaro db "Light cyan - ",0,0,0,0,0

	vermelhoclaro db "Light red - ",0,0,0,0,0
	magentaclaro db "Light magenta - ",0,0,0,0,0
	amarelo db "Yellow - ",0,0,0,0,0
	
	string_saida1 db "The file ",0
	string_saida2 db " contains the following tile count:",CR,LF,0	
	
	string_rodape1 db "File ",0
	string_rodape2 db " - Total tiles by color:",0

;==========================================================================================================================

	FileName db 16 dup(?)
	FileBuffer db 128 dup(?)
	FileHandle dw 0
	FileNameSaida db 16 dup(?)
	FileHandleSaida dw 0
	FileBufferSaida dw 128 dup (?)
	
	Buffer db 128 dup (?)
	
	BufferWRWORD db 5 dup (?)
	
	caractere db 0

	msg_cr_lf db CR,LF,0
	msg_nome db "File name: ",0
	msg_erro db "Try again. Error: ",0
	msg_ok db "File open successfully.",CR,LF,0
	msg_fclose	db "Error when closing file.",CR,LF,0
	msg_fcreate db "Error when creating output file.",CR,LF,0
	msg_fread db "Error when reading from file.",CR,LF,0
    
    msg_titulo db "************ Gabriel Lima Chimifosk ************ Wall tile counter *************",0
	
	msg_enter	db "Shut down by user command.",CR,LF,0
	
	Sufixo1 db ".par",0
	Sufixo2 db ".rel",0
	
	;----------------
	;ERROR CODES
	error_01  db "Invalid function number",CR,LF,0
	error_02  db "File not found",CR,LF,0
	error_03  db "Path not found",CR,LF,0
	error_04  db "Too many open files (no handles left)",CR,LF,0
	error_05  db "Access denied",CR,LF,0
	error_0C  db "Invalid access mode (open mode is invalid)",CR,LF,0
	error_56  db "Invalid password",CR,LF,0
	;----------------
	
	
	;----------------------------
	;DRAWING PARAMETERS
	MAX_LINHAS equ 15
	MAX_COLUNAS equ 26
	
	BufferLinha db 16 dup (?)
	BufferColuna db 16 dup (?)
	num_linhas	dw 0
	num_colunas dw 0
	
	aux_coluna dw 0
	aux_linha dw 0
	
	tam_quadrado dw 0
	
	coord_x_inicial dw 0
	coord_y_inicial dw 0
	
	coord_x_aux dw 0
	coord_y_aux dw 0
	;--------------------
	
	aux_printa_rodape dw 0
	contador_printa_rodape dw 0

	aux_string_rodape db 8 dup (?)
	
	aux_tamquadrado dw 0

	sw_n	dw	0
	sw_f	db	0
	sw_m	dw	0
	
.code

.startup

	;initializes segments
	mov ax,@DATA
	mov ds,ax
	mov es,ax
	
inicio:
    call inicializatela

loop_gets:
	mov cx,16
	lea di,FileName
	call clrmem
	
	;stores the file name in *str
	call fgets
	
	;opens the file, returns carry = 0 in case it did not
	lea dx,FileName
	call fopen
	
	jnc loop_gets
	
	call calculalinhas
	call calculacolunas
	
	mov ah,00
	mov ah,00
	call calculatamquadrado
	
	call desenhatela

	call manipulacores
	
	call cria_arquivo_saida

	mov bx,FileHandle
	call fclose

	call contabiliza

	call preparaarquivosaida
	call escrevearquivosaida
	
	mov bx,FileHandleSaida
	call fclose
	
	call desenharodape
	
	call getKey
	call limpatelatexto
	call reiniciastrings
	jmp inicio
	
.exit

;---------------------
;Function: calculate_square_size
;	size <- 624/number_of_columns
;	aux <- number_of_rows * size
;	cmp aux,360
;	greater:
;		size = 360 / number_of_rows
;	smaller:
;		end
;---------------------
calculatamquadrado proc far

	mov bx,num_colunas	
	mov bh,0
	mov ax,624
	cmp bl,1
	je div_zero
	
	div bl				;ax<-624/num_col
	mov ah,0
	mov tam_quadrado,ax	;tam_quadrado <- ax
	
	mov ax,tam_quadrado
	mul num_linhas		;ax<-tam*num_linhas
	cmp ax,360
	jb tam_ok			;if(aux<360), ok
	
nao_divzero:
	mov bx,num_linhas	;else
	mov bh,0
	mov ax,360

	div bl				;ax<-360/num_lin
	mov ah,0
	mov tam_quadrado,ax	;tam_quadrado <- ax
tam_ok:
	ret
	
div_zero:
	mov bx,num_linhas
	mov bh,0
	cmp bl,1
	jne nao_divzero
	mov tam_quadrado,360
	jmp tam_ok
	
calculatamquadrado endp



;----------------------
;Function: draw_footer
;
;---------------------
desenharodape proc far

	mov dh,25 
	mov dl,3
	call setcursor
	
	lea bx,string_rodape1
	call printf_s
	lea bx,FileName
	call printf_s
	lea bx,string_rodape2
	call printf_s
	
	mov bx,24
	mov cx,31
	mov aux_printa_rodape,cx
	mov dx,430
	
	mov cx,15
	mov contador_printa_rodape,cx
loop_printarodape:
	mov contador_printa_rodape,cx
	
	mov cx,aux_printa_rodape
	call quadrado
	
	push ax
	push bx
	push dx
	mov cx,contador_printa_rodape
	mov ah,0
	mov al,0Fh
	sub ax,cx
	mov cx,aux_printa_rodape
	call pintaquadrado
	pop dx
	pop bx
	pop ax
	
	mov cx,aux_printa_rodape 
	add cx,40
	mov aux_printa_rodape,cx
	
	mov cx,contador_printa_rodape
	loop loop_printarodape
	
	
	mov dh,29
	mov dl,5	
	call setcursor
	lea bx,qtde_preto
	mov cx,15
loop_printarodape2:
push cx
	mov al,byte ptr [bx]
	mov ah,0
push bx
	lea bx,aux_string_rodape
	call sprintf_w
	lea bx,aux_string_rodape
	call printf_s
pop bx
	call getcursor
	mov al,byte ptr [bx]
	mov ah,0
	cmp ax,9
	ja ax_maiorque9
		add dl,4
		jmp setcursor_rodape
ax_maiorque9:
	add dl,3
setcursor_rodape:
	call setcursor
	inc bx	
pop cx
	loop loop_printarodape2
	
	ret
desenharodape endp


;-----------------------
;Function:fwritebyte
;	writes 1 byte to the file
;	DS:DX -> byte
;-----------------------
fwritebyte proc far
	push bx
	
	mov ah,40h
	mov al,00h
	mov bx,FileHandleSaida
	mov cl,01
	mov ch,0
	int 21h	
	mov FileHandleSaida,bx
	
	pop bx
	ret

fwritebyte endp


;-----------------------
;Funçao:fwritestring
;	Entry:
;		DS:BX -> string
;-----------------------
fwritestring proc far
	push cx
	push ax
	;lea bx,String1
	mov dx,bx
	call strlen ;ax <- strlen
	
	
	mov cx,ax	;cx <- strlen
loop_writeline:
	push cx
	call fwritebyte
	inc dx
	pop cx
	;call getKey
	loop loop_writeline
	pop ax
	pop cx
	ret
fwritestring endp

;------------------------------
;Function: void(create_output_file)
;	Outputs:
;		FileNameSaida <- output file name
;		FileHandleSaida <- output file handle
; memcpy(FileName, FileNameSaida)
;
; while(*str != '.' || *str != '\0')
; {
; 	cx++;
; }
;
; strcat(*[FileName + offset], *Suffix2, 5)
;-----------------------------

cria_arquivo_saida proc far

	lea bx,FileName
	call strlen ;ax <- strlen(FileName)

	mov cx,ax
	lea di,FileNameSaida 	;memcpy(FileName,FileNameSaida)
	lea si,FileName
	rep movsb

	lea bx,FileName
	call strlen ;ax <- strlen(FileName)

	;CONCATENATES .REL
	mov cx,ax
	mov al,PONTO
	lea di,FileNameSaida
	repnz scasb				; while PONTO not found, searches for PONTO

	lea bx,FileName
	call strlen ;ax <- strlen(FileName)

	sub ax,cx				; calculates ponto
	mov cx,ax
	dec cx

	lea di,FileNamesaida
	add di,cx				; deslocates di up until where ponto is

	mov cx,5
	lea si,Sufixo2
	rep movsb				; copies ".rel",0 over ".par",0

	;CREATES FILE
	lea dx,FileNameSaida	; int 21h (fcreate)
	call fcreate
	mov FileHandleSaida,ax
	
	;WRITES FIRST LINE
	lea bx,string_saida1
	call fwritestring
	
	lea bx,FileName
	call fwritestring
	
	lea bx,string_saida2
	call fwritestring

	ret

cria_arquivo_saida endp



;------------------------
;Function: print_colors
;
;------------------------
printacores proc far

	mov bl,qtde_preto
	mov bh,0
	mov cx,0Fh
loop_printacores:
	push cx
	
	mov ax,[bx]
	mov ah,0
	
	push bx
	lea bx,BufferWRWORD
	call sprintf_w

	pop bx
	
	pop cx
	loop loop_printacores
	
printacores endp

;---------------------
;Function: counts tiles and stores the number in memory positions
;
;
;---------------------
contabiliza proc far
	call zera
	
	mov ax,0
	mov cx,128
	lea di,FileBufferSaida
	rep stosb
	
	lea dx,FileName
	call fopen
	
	call calculalinhas
	call calculacolunas
	
	mov cx,num_linhas
loop_contabiliza:
	push cx
	call fgetLine
	lea bx,FileBufferSaida
	mov cx,num_colunas
	contabiliza_linha:
		push cx
		call getcor
		inc bx
		mov ah,0
		
		;*(qtde_preto+ax)++;
		push bx
		lea bx,qtde_preto
		add bx,ax
		add [bx],1
		pop bx
		
		pop cx
		loop contabiliza_linha
		
	pop cx
	loop loop_contabiliza
	
	mov bx,FileHandle
	call fclose
	
	call cria_arquivo_saida
	
	ret
contabiliza endp




;------------------
;Function:
;	manipulate_colors
;	paints square by square
;------------------
;
;
;aux_row <- initial_y_coord
;
;for(cx = num_rows; cx != 0; cx--) {
;	lea bx, Buffer
;	
;	aux_column <- initial_x_coord
;	push cx
;	for(cx = num_columns; cx != 0; cx--){
;		push cx
;
;		al <- get_color(bx)
;		bx++
;		
;		push bx
;		
;		bx <- square_size
;		cx <- aux_column
;		ah <- 0
;		paint_square(al, bx, cx, dx)
;
;		pop cx
;		pop bx
;
;		add aux_column, square_size
;	}
;	pop cx
;
;}
manipulacores proc far

mov ax,coord_y_inicial
mov aux_linha,ax

mov cx,num_linhas
loop_pintalinhas:
	push cx

mov ax,coord_x_inicial
mov aux_coluna,ax
	
push ax
push bx
push cx
push dx
call fgetLine
pop dx
pop cx
pop bx
pop ax

	lea bx,FileBufferSaida
	mov dx,aux_linha
	call pintalinha	;entra: bx->Buffer, dx -> linha atual
	
	mov dx,aux_linha
	add dx,tam_quadrado
	mov aux_linha,dx
	
	push bx
	mov bx,tam_quadrado
	;add aux_linha,bx
	pop bx
	
	pop cx
	LOOP loop_pintalinhas
	
	ret
	
manipulacores endp



pintalinha proc far

mov cx,num_colunas
loop_pintalinha:
	push cx

	call getcor	;bx -> pointeiro p/ byte| al<-byte
	inc bx		;buffer++
	
	push bx
	;ah -> 0 al -> codigo cor -- coord(cx,dx) -- bx -> lado
	mov bx,tam_quadrado
	mov cx,aux_coluna
	call pintaquadrado
	mov dx,aux_linha
	add aux_coluna,bx
	add aux_coluna,1
	;call getKey
	pop bx
	pop cx
	LOOP loop_pintalinha
	
	ret
pintalinha endp




;-------------------
;Function: get_color -> reads 1 byte and stores hex in al
;Input: bx <- pointer to byte
;
;-------------------
getcor proc far
	mov al,byte ptr [bx]
	cmp al,'9'
	ja maiorque9
	sub al,48
	jmp fim_getcor
	
maiorque9:
	sub al,55
fim_getcor:
	ret
getcor endp

;------------------
;Function: line_count
;
;------------------
calculalinhas proc far
	lea dx,Buffer
	call fread  	;reads 1 character and stores in buffer
	inc dx			;*buffer++
	mov bx,dx
	
	call fread
	mov bx,dx
	mov ax,[bx]
	cmp ax,','		; if buffer[1]==',' there is only one char
	je um_caractere
						; else
		mov al,[bx] 	; buffer[1] <- second char
		inc dx
		call fread
	
um_caractere:
	mov bx,dx
	mov [bx],0
	
	lea bx,Buffer
	call atoi
	mov num_linhas,ax
	ret
calculalinhas endp


;------------------
;Function: column_count
;------------------
calculacolunas proc far
	lea dx,Buffer
	call fread  	;reads 1 character and stores in buffer
	inc dx			;*buffer++
	mov bx,dx
	
	call fread
	mov bx,dx
	mov ax,[bx]
	cmp ax,CR		; if buffer[1]==CR there is only one char
	je um_caractere2
						; else
		mov al,[bx] 	; buffer[1] <- second char
		inc dx
		call fread		;CR
		call fread		;LF
		;call fread		;new line
		jmp fim_calculacolunas
um_caractere2:
	call fread	;LF
	;call fread	;new line
	
fim_calculacolunas:
	mov bx,dx
	mov [bx],0
	
	lea bx,Buffer
	call atoi
	mov ah,0
	mov num_colunas,ax
	ret
calculacolunas endp




;-------------
;Function: fread
;	Reads 1 byte from the file
;	and stores the content in Buffer
;	ds:dx -> buffer
;	advances 1 character
;------------
fread proc far

	mov cx,1
	mov bx,FileHandle
	mov ah,3fh
	int 21h
	jnc fim_fread
		lea bx,msg_fread
		call printf_s
		jmp fim_fread
fim_fread:
	mov ah,0
	ret
	
fread endp


;------------------------------
;Function: fgetLine reads a line from a file and stores it in LineBuffer
;------------------------------
fgetLine proc far
	
	mov cx,num_colunas
	lea dx,FileBufferSaida
loop_getline:
push cx
	call fread 			;*buffer <- char
	mov ax,FileBufferSaida
		cmp al,0 		;if(eof) goto end
		je fim_fgetline
		cmp al,CR		;if(CR) goto quase_end
		je fim_fgetline
	inc dx				;*buffer++
pop cx
	loop loop_getline
	
quasefim_fgetLine:
	call fread
	call fread
fim_fgetLine:
	mov bx,dx
	mov [bx],0
	ret
	
fgetLine endp

;-----------------------
;Function: fcreate(*str)
;	Entry:
;		DS:DX -> file name
;-----------------------
fcreate proc far
	
	mov cl,1
	mov ah,3ch
	mov al,0
	int 21h
	jnc continua_fcreate
		lea bx,msg_fcreate
		call printf_s
continua_fcreate:
	ret
	
fcreate endp

;-----------------------
;Function: fclose(FileHandle)
;	BX -> FileHandle
;-----------------------
fclose proc far
	
	mov ah,3eh
	mov al,0
	int 21h
	jnc continua_fclose
		lea bx,msg_fclose
		call printf_s
		jmp fim_fclose
continua_fclose:

fim_fclose:
	ret
	
fclose endp

;---------------------------------------
;Function: fopen(*str)
;	Opens the file with the name in str.
;	Input:
;		DS:DX -> *str
;	Output:
;		CF==1 if successful, AX = file handle
;		CF==0 on error,		 AX = error code (01h,02h,03h,04h,05h,0Ch,56h)
;---------------------------------------

fopen proc far
	
	mov ah,3dh
	mov al,0
	int 21h
	jnc continua_fopen
		push ax
		lea bx,msg_erro
		call printf_s
		pop ax
		call errorcode
		jmp fim_fopen
	
continua_fopen:
	mov FileHandle,ax
	
	;lea bx,msg_ok
	;call printf_s
	
	stc
fim_fopen:	
	ret
	
fopen endp

;=====================================================================
;=====================================================================
;----------------------
;Function: char *fgets(char *str, int n)
;	Reads characters from the keyboard and stores them in str (name).
;	If the user typed "filename.par", opens the file "filename.par"
;	Otherwise, concatenates "filename" with ".par" and opens the file "filename.par"
;----------------------
fgets proc far
	
	lea bx,msg_nome
	call printf_s
	
	mov	ah,0ah				; reads a line from keyboard
	lea	dx,FileBuffer
	mov	byte ptr FileBuffer,MAX_STR	; maximum string size
	int	21h
	
	mov al,FileBuffer+2
	cmp al,CR
	je fim_enter
	
	;source: buffer
	;destination: nome
	lea si,FileBuffer+2				; copies from buffer to filename
	lea di,FileName		
	mov cl,FileBuffer+1				; read char count
	mov ch,0
	
	mov	ax,ds						; adjusts movsb
	mov	es,ax
	rep movsb						; copies from buffer to name
	
	mov	byte ptr es:[di],0			; '\0' at the end of the string

	lea bx,FileName
	call strlen
	mov cx,ax
	
	mov al,PONTO
	lea di,FileName
	repne scasb						; while not found PONTO (period), searches for PONTO
	je nao_concatena				
	
	lea bx,FileName
	call strlen
	
	mov cl,05h
	lea si,Sufixo1					; concatenates string with ".par"
	lea di,FileName
	call strcat
	
nao_concatena:
	; lea bx,msg_cr_lf
	; call printf_s
	
	; lea bx,FileName
	; call printf_s
	
	lea bx,msg_cr_lf
	call printf_s

	ret
	
fgets endp

fim_enter:
	lea bx,msg_cr_lf
	call printf_s
	lea bx,msg_enter
	call printf_s
	.exit 1


;-------------------------------------
;Function: strcat(*src,*dest)
;	Concatenates *src string and *dest string
;	Entry:
;		AX		-> strlen(dest)
;		CL		-> strlen(src)
;		DS:SI	-> suffix
;		ES:DI	-> prefix
;-------------------------------------

strcat proc far
	
	
	mov ch,00h
	add di,ax
	rep movsb

	ret
	
strcat endp


;-------------------------------
;Function: int strlen(*str)
;	while(*str!='\0'){
;		*str++;
;		count++;
;	}
; 	return count;
;	
;	Entry: 
;		DS:BX -> *str
;	Output: 
;		AX -> strlen(*str)
;-------------------------------
strlen proc far
	
	mov ax,0

loop_strlen:
	cmp byte ptr [bx],00H
	je fim
	inc bx
	inc ax
	jmp loop_strlen

fim:
	ret
	
strlen endp

;--------------------
;Function: zeroes cx bytes of memory
;	Inputs:  CX -> n bytes
;			ES:DI -> address
;--------------------
clrmem proc far
	
	mov al,0
	rep stosb
	ret
	
clrmem endp

;-------------------------
;Function: char getchar()
;	Waits for a character to be typed and returns it.
;	Output: AL -> character
;-------------------------

getchar proc far

	mov ah,01H
	int 21h
	ret

getchar endp

;------------------
;Function: void printf_s(*str)
;	Receives the pointer to the start of a string and prints each character until reaching '\0'
;	Input: DS:BX -> pointer to string 
;------------------
printf_s proc far
	
	;se char == '\0', encerra
	mov dl,[bx]
	cmp dl,0
	je fim_printf_s
	
	;putchar
	mov ah,02h
	int 21h
	
	;percorre string (*str++)
	inc bx
	jmp printf_s
	
fim_printf_s:
	ret
	
printf_s endp

;------------------------------------
;Function: getcursor()
;	Gets the current position of the cursor
;	Output:
;		AX -> 0
;		CH -> Start scan line
;		CL -> End scan line
;		DH -> Row
;		DL -> Column
;------------------------------------
getcursor proc far

	mov ah,03h
	mov bh,00h
	int 10h
	ret

getcursor endp

;------------------------------------
;Function: setcursor()
;	Sets the cursor to a new position
;	Inputs:
;		DH -> Line
;		DL -> Column
;------------------------------------
setcursor proc far
	
	mov ah,02h
	mov bh,00h
	int 10h
	ret
	
setcursor endp

;-------------------------------------
;Function: print_backspace()
;	Deletes the current character and moves the cursor back by 1
;-------------------------------------
printa_backspace proc far

	push ax
	push cx
	push dx
	
	;"deletes" character
	mov dl," "
	mov ah,02h
	int 21h
	
	;gets current cursor position
	call getcursor
	;decrements column cursor by 1
	dec dl
	call setcursor
	
	pop dx
	pop cx
	pop ax
	ret
	
printa_backspace endp

;--------------------------------------------------------------------
;Function: Writes the value of AX to the screen
;		printf("%
;--------------------------------------------------------------------
printf_w	proc	near
	; sprintf_w(AX, BufferWRWORD)
	lea		bx,BufferWRWORD
	call	sprintf_w
	
	; printf_s(BufferWRWORD)
	lea		bx,BufferWRWORD
	call	printf_s
	
	ret
printf_w	endp

;
;--------------------------------------------------------------------
;Function: Converts an integer (n) to (string)
;		 sprintf(string->BX, "%d", n->AX)
;--------------------------------------------------------------------
sprintf_w	proc	near
	mov		sw_n,ax
	mov		cx,5
	mov		sw_m,10000
	mov		sw_f,0
	
sw_do:
	mov		dx,0
	mov		ax,sw_n
	div		sw_m
	
	cmp		al,0
	jne		sw_store
	cmp		sw_f,0
	je		sw_continue
sw_store:
	add		al,'0'
	mov		[bx],al
	inc		bx
	
	mov		sw_f,1
sw_continue:
	
	mov		sw_n,dx
	
	mov		dx,0
	mov		ax,sw_m
	mov		bp,10
	div		bp
	mov		sw_m,ax
	
	dec		cx
	cmp		cx,0
	jnz		sw_do

	cmp		sw_f,0
	jnz		sw_continua2
	mov		[bx],'0'
	inc		bx
sw_continua2:

	mov		byte ptr[bx],0
	ret		
sprintf_w	endp

;--------------------------------------------------------------------
;Function: Converts an ASCII-DECIMAL to HEX
;Input: (S) -> DS:BX -> Pointer to the source string
;Output:	(A) -> AX -> Resulting "Hex" value
;Algorithm:
;	A = 0;
;	while (*S != '\0') {
;		A = 10 * A + (*S - '0')
;		++S;
;	}
;	return
;--------------------------------------------------------------------
atoi	proc near

		; A = 0;
		mov		ax,0
		
atoi_2:
		; while (*S!='\0') {
		cmp		byte ptr[bx], 0
		jz		atoi_1

		; 	A = 10 * A
		mov		cx,10
		mul		cx

		; 	A = A + *S
		mov		ch,0
		mov		cl,[bx]
		add		ax,cx

		; 	A = A - '0'
		sub		ax,'0'

		; 	++S
		inc		bx
		
		;}
		jmp		atoi_2

atoi_1:
		; return
		ret

atoi	endp


;======================================================
;                   SCREEN HANDLING
;======================================================

;-----------------------------
;Function: initializes screen
;-----------------------------
inicializatela proc far

    call limpatelatexto
    
    ;cabeçalho
    mov dh,0
    mov dl,0
    call setcursor
    lea bx,msg_titulo
    call printf_s
	
    ;pede arquivo
    mov dh,1
    mov dl,0
    call setcursor	
	
	ret
	
inicializatela endp

desenhatela proc far

	
	call telagrafica
	
	mov dh,0
    mov dl,0
    call setcursor
    lea bx,msg_titulo
    call printf_s	
	
	;desenha grid
	;mov num_linhas,MAX_LINHAS	; numero linhas: MAX_LINHAS
	;mov num_colunas,MAX_COLUNAS ; numero colunas: MAX_COLUNAS	
	mov coord_x_inicial,10		
	mov coord_y_inicial,20		; coordenadas iniciais (10,20)
	;mov tam_quadrado,24
	
	mov ax,coord_x_inicial		; ax <- coord x inicial
	mov bx,tam_quadrado			; bx <- lado quadrado
	mov dx,coord_y_inicial		; dx <- coord y inicial
	call grid	
	

	; mov cx,coord_x_inicial
	; mov dx,coord_y_inicial
	; mov bx,tam_quadrado
	; mov al,03h
	; call pintaquadrado
	
	ret

desenhatela endp


;-----------------------------------
;Function: draws grid 
;	Inputs:	
;			AX -> initial x coordinate
;			BX -> square side length
;			num_rows -> number of rows
;			num_columns -> number of columns
;			initial_x_coord -> initial x coordinate			
;			DX -> initial y coordinate
;-----------------------------------
grid proc near

mov cx,num_linhas
loop_grid:
	push cx
	
	mov cx,num_colunas
	call desenhalinha
	;call getKey
	add dx,bx
	mov ax,coord_x_inicial
	
	pop cx
	loop loop_grid
	
	ret
grid endp

;----------------------------------
;Function: paint square
;	paints the lines from dx + 1 to dx + (bx - 1) with a line of length bx - 2
;	x coordinate of the lines: cx + 1 (constant)
;
;	Inputs:
;		BX -> square side length
;		CX -> x coordinate
;		DX -> y coordinate
;		AL -> color code
;----------------------------------
pintaquadrado proc far	
	
	sub bx,1
	inc cx
	mov coord_x_aux,cx
	mov cx,bx
loop_pinta:
	push cx
	
	mov cx,coord_x_aux
	inc dx
	call retaH
	
	pop cx
	loop loop_pinta
	
	ret
	
pintaquadrado endp

;-------------------------------------
;Function: draw line with 26 squares
;	Inputs: 
;			AX -> initial x coord
;			BX -> size
;			CX -> number of columns
;			DX -> initial y coord
;-------------------------------------
desenhalinha proc near

	mov coord_x_aux,ax
loop_linha:
	push cx
	
	mov cx,coord_x_aux
	call quadrado
	add cx,bx
	mov coord_x_aux,cx
	
	pop cx
	
	;call getKey
	loop loop_linha
	
	ret
desenhalinha endp

;------------------------------------
;Function: Clears the screen and sets it to text mode 80x25
;------------------------------------

limpatelatexto proc far

    mov	ah,0	; Seta modo da tela
	mov	al,3	; Text mode, monochrome, 80x25.
	int	10h
	ret 

limpatelatexto endp

;-----------------------------------
;Function: sets the screen to graphic mode
;       
;-----------------------------------
telagrafica proc far

    mov	ah,0	; Seta modo da tela
	mov	al,12h
	int	10h
	ret

telagrafica endp

;--------------------------------------------------------------------
;Function: Draws a white square on the graphic screen
;Input:	BX -> dimension of the SIDE of the square
;		CX -> Left side
;		DX -> Upper side
;--------------------------------------------------------------------
quadrado	proc	near

	mov al,0Fh
	call	retaH
	call	retaV

	push	cx
	add		cx,bx
	call	retaV
	pop		cx
	
	push	dx
	add		dx,bx
	call	retaH
	pop		dx
	
	ret
quadrado	endp

;--------------------------------------------------------------------
;Function: Draws a horizontal line on the graphic screen
;Input:	BX -> length of the line
;		CX -> starting column
;		DX -> starting row
;--------------------------------------------------------------------
retaH	proc	near
	push	bx
	push	cx

loopRetaH:
	call	setPixel
	inc		cx
	dec		bx
	jnz		loopRetaH
	
	pop		cx
	pop		bx
	ret
retaH	endp


;--------------------------------------------------------------------
;Function: Draws a vertical line on the graphic screen
;Input:	BX -> length of the line
;		CX -> starting column
;		DX -> starting row
;--------------------------------------------------------------------
retaV	proc	near
	push	bx
	push	dx

loopRetaV:
	call	setPixel	
	inc		dx
	dec		bx
	jnz		loopRetaV
	
	pop		dx
	pop		bx
	ret
retaV	endp

;--------------------------------------------------------------------
;Function: Sets the bit for the given coordinates
;Input:
;	DX - row
;	CX - column
; Use BIOS Write Pixel (INT 10H with AH=0CH)
; AL - Color to set (set bit 10000000b for XOR mode).
; BH - Video page number.
; CX - Pixel column number.
; DX - Pixel row number.
;--------------------------------------------------------------------
SetPixel	proc	near
	push	bx
	push	cx
	push	dx
	
	mov	bh,0
	mov	ah,0ch
	int	10h
	
	pop		dx
	pop		cx
	pop		bx
	
	ret
SetPixel	ENDP 

;--------------------------------------------------------------------
;Function: Waits for a character from the keyboard
;Output: AL => character read from the keyboard
;Note:
;	al = Int21(7)
;--------------------------------------------------------------------
getKey	proc	near
	mov		ah,7
	int		21H
	ret
getKey	endp


;----------------
;Function: Reports an error
;	Inputs:
;		AX -> error code
;----------------
errorcode proc far
	mov ah,0
	
	cmp al,01h
	je print_error_01

	cmp al,02h
	je print_error_02
	
	cmp al,03h
	je print_error_03
	
	cmp al,04h
	je print_error_04
	
	cmp al,05h
	je print_error_05
	
	cmp al,0ch
	je print_error_0c
	
	cmp al,56h
	je print_error_56
	
fim_error:
	call printf_s
	ret
	
print_error_01:
	lea bx,error_01
	jmp fim_error
print_error_02:
	lea bx,error_02
	jmp fim_error
print_error_03:
	lea bx,error_03
	jmp fim_error
print_error_04:
	lea bx,error_04
	jmp fim_error
print_error_05:
	lea bx,error_05
	jmp fim_error
print_error_0c:
	lea bx,error_0c
	jmp fim_error
print_error_56:
	lea bx,error_56
	jmp fim_error
	
errorcode endp

;---------------------
;Function: prepare_output_file
;	concatenates the strings to be written to the output file
;	with the number of tiles of that color
;---------------------
preparaarquivosaida proc far
	
	mov ah,0
	mov al,qtde_preto
	lea bx,BufferWRWORD
	call sprintf_w
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax	;cx <- strlen (src)
	lea bx,preto
	call strlen;ax <- strlen(dest)
	lea di,preto
	lea si,BufferWRWORD
	call strcat
	;--------------------------
	
	mov ah,0
	mov al,qtde_azul
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,azul
	call strlen;ax <- strlen(dest)
	lea di,azul
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_verde
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,verde
	call strlen;ax <- strlen(dest)
	lea di,verde
	lea si,BufferWRWORD
	call strcat

	mov ah,0
	mov al,qtde_ciano
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,ciano
	call strlen;ax <- strlen(dest)
	lea di,ciano
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_vermelho
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,vermelho
	call strlen;ax <- strlen(dest)
	lea di,vermelho
	lea si,BufferWRWORD
	call strcat

	mov ah,0
	mov al,qtde_magenta
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,magenta
	call strlen;ax <- strlen(dest)
	lea di,magenta
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_marrom
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,marrom
	call strlen;ax <- strlen(dest)
	lea di,marrom
	lea si,BufferWRWORD
	call strcat
	
	;8
	mov ah,0
	mov al,qtde_cinzaclaro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,cinzaclaro
	call strlen;ax <- strlen(dest)
	lea di,cinzaclaro
	lea si,BufferWRWORD
	call strcat

	mov ah,0
	mov al,qtde_cinzaescuro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,cinzaescuro
	call strlen;ax <- strlen(dest)
	lea di,cinzaescuro
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_azulclaro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,azulclaro
	call strlen;ax <- strlen(dest)
	lea di,azulclaro
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_verdeclaro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,verdeclaro
	call strlen;ax <- strlen(dest)
	lea di,verdeclaro
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_cianoclaro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,cianoclaro
	call strlen;ax <- strlen(dest)
	lea di,cianoclaro
	lea si,BufferWRWORD
	call strcat

	mov ah,0
	mov al,qtde_vermelhoclaro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,vermelhoclaro
	call strlen;ax <- strlen(dest)
	lea di,vermelhoclaro
	lea si,BufferWRWORD
	call strcat
	
	mov ah,0
	mov al,qtde_magentaclaro
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,magentaclaro
	call strlen;ax <- strlen(dest)
	lea di,magentaclaro
	lea si,BufferWRWORD
	call strcat

	mov ah,0
	mov al,qtde_amarelo
	lea bx,BufferWRWORD
	call sprintf_w	
	lea bx,BufferWRWORD
	call strlen
	mov cx,ax
	lea bx,amarelo
	call strlen;ax <- strlen(dest)
	lea di,amarelo
	lea si,BufferWRWORD
	call strcat
	
	ret
preparaarquivosaida endp

;----------------------
;zeroes
;----------------------
zera proc far
	lea di,qtde_preto
	mov ch,0
	mov	cl,15
	mov	ax,0
	rep stosb
	ret

zera endp
;-----------------
;Function: write_output_file
;
;------------------
escrevearquivosaida proc far
	;0
	lea bx,preto
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,azul
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,verde
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,ciano
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,vermelho
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,magenta
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,marrom
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	;8
	lea bx,cinzaclaro
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,cinzaescuro
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,azulclaro
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,verdeclaro
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,cianoclaro
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,vermelhoclaro
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,magentaclaro
	call fwritestring	
	
	lea bx,msg_cr_lf
	call fwritestring
	
	lea bx,amarelo
	call fwritestring
	
	lea bx,msg_cr_lf
	call fwritestring
	
	ret
escrevearquivosaida endp


;---------------------------
;Function: resets strings
;
;
;---------------------------
reiniciastrings proc far
	;0
	lea bx,preto
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,preto
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,azul
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,azul
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,verde
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,verde
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,ciano
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,ciano
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,vermelho
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,vermelho
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,magenta
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,magenta
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,marrom
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,marrom
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0

	;8
	lea bx,cinzaclaro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,cinzaclaro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,cinzaescuro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,cinzaescuro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0

	lea bx,azulclaro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,azulclaro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,verdeclaro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,verdeclaro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,cianoclaro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,cianoclaro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0

	lea bx,vermelhoclaro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,vermelhoclaro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0

	
	lea bx,magentaclaro
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,magentaclaro
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	
	lea bx,amarelo
	call strlen
	mov cx,ax
	mov al,'-'
	lea di,amarelo
	repnz scasb 
	mov byte ptr [di],' '
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0
	inc di
	mov byte ptr [di],0

	ret
reiniciastrings endp





end
