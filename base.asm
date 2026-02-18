IDEAL
MODEL small
STACK 100h
DATASEG
	buff    db  26        ;MAX NUMBER OF CHARACTERS ALLOWED (25).
			db  ?         ;NUMBER OF CHARACTERS ENTERED BY USER.
			db  26 dup(0) ;CHARACTERS ENTERED BY USER.
			
	dynamicName db 'V.bmp', 0   ; 'X' is a placeholder
	filehandle dw ?
	Header db 54 dup (0)
	Palette db 256*4 dup (0)
	ScrLine db 320 dup (0)
	ErrorMsg db 'Error', 13, 10 ,'$'
CODESEG

proc OpenFile
    mov ah, 3Dh         
    mov al, 0           
    int 21h             
    
    jc openerror        
    mov [filehandle], ax 
    ret

    openerror:
        mov dx, offset ErrorMsg
        mov ah, 9h
        int 21h

    ret
endp OpenFile

proc ReadHeader
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette

proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB .
	mov al,[si+2] ; Get red value .
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it .
	mov al,[si+1] ; Get green value .
	shr al,2
	out dx,al ; Send it .
	mov al,[si] ; Get blue value .
	shr al,2
	out dx,al ; Send it .
	add si,4 ; Point to next color .
	; (There is a null chr. after every color.)
	loop PalLoop
ret
endp CopyPal

proc CopyBitmap
	; BMP graphics are saved upside-down .
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop :
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	pop cx
	loop PrintBMPLoop
	ret
endp CopyBitmap

proc LoadLetterBMP
    push bp
    mov bp, sp
    mov ax, [bp+4]    

    mov [dynamicName], al ; Replace 'X' in 'X.BMP' with our character

    mov ax, 13h
    int 10h

    mov dx, offset dynamicName
    call OpenFile       ; Opens the file named in DX
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap

    mov ah, 3Eh
    mov bx, [filehandle]
    int 21h

    pop bp
    ret 2               ; Clean up the stack (pushed character)
endp LoadLetterBMP

start :
	mov ax, @data
	mov ds, ax

input:            
    ;CAPTURE STRING FROM KEYBOARD.                                    
        mov ah, 0Ah ;SERVICE TO CAPTURE STRING FROM KEYBOARD.
        mov dx, offset buff
        int 21h                 

    ;CHANGE CHR(13) BY '$'.
        mov si, offset buff + 1 ;NUMBER OF CHARACTERS ENTERED.
        mov cl, [ si ] ;MOVE LENGTH TO CL.
        mov ch, 0      ;CLEAR CH TO USE CX. 
        inc cx ;TO REACH CHR(13).
        add si, cx ;NOW SI POINTS TO CHR(13).
        mov al, '$'
        mov [ si ], al ;REPLACE CHR(13) BY '$'.            

    ;DISPLAY STRING.                   
        mov ah, 9 ;SERVICE TO DISPLAY STRING.
        mov dx, offset buff + 2 ;MUST END WITH '$'.
        int 21h

call StartMorse
mov ax, 4c00h
int 21h

proc StartMorse
    xor ch, ch          ; Clear CH
    mov cl, [buff + 1]  ; Get ACTUAL number of characters entered
    cmp cx,0
    jne keepGoing           ; If user entered 0 chars, skip to end
    jmp Done

    keepGoing:
    mov si, offset buff + 2 ; Point SI to the first real character

    checkCharacter:
        push cx         ; save loop counter
        push si         ; save pointer
        mov al, [si]        ; Load the current character into AL

        cmp al, '0' ; check if it's a digit ('0'-'9')
        jb NotDigit
        cmp al, '9'
        ja NotDigit

        ; --- Comparison Logic ---
        NotDigit:
        and al, 0DFh ; Convert to uppercase if it's a lower case letter ('a'-'z')

        cmp al, 'A'        
        jne CheckB
        call PlayA_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckB:
        cmp al, 'B'       
        jne CheckC
        call PlayB_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckC:
        cmp al, 'C'        
        jne CheckD
        call PlayC_Morse
        jmp NextIteration ; Jump to next character after handling a match
        

        CheckD:
        cmp al, 'D'         
        jne CheckE
        call PlayD_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckE:
        cmp al, 'E'       
        jne CheckF
        call PlayE_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckF:
        cmp al, 'F'        
        jne CheckG
        call PlayF_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckG:
        cmp al, 'G'       
        jne CheckH
        call PlayG_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckH:
        cmp al, 'H'        
        jne CheckI
        call PlayH_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckI:
        cmp al, 'I'        
        jne CheckJ
        call PlayI_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckJ:
        cmp al, 'J'        
        jne CheckK
        call PlayJ_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckK:
        cmp al, 'K'         
        jne CheckL
        call PlayK_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckL:
        cmp al, 'L'        
        jne CheckM
        call PlayL_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckM:
        cmp al, 'M'        
        jne CheckN
        call PlayM_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckN:
        cmp al, 'N'      
        jne CheckO
        call PlayN_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckO:
        cmp al, 'O'      
        jne CheckP
        call PlayO_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckP:
        cmp al, 'P'      
        jne CheckQ
        call PlayP_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckQ:
        cmp al, 'Q'     
        jne CheckR
        call PlayQ_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckR:
        cmp al, 'R'      
        jne CheckS
        call PlayR_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckS:
        cmp al, 'S'        
        jne CheckT
        call PlayS_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckT:
        cmp al, 'T'        
        jne CheckU
        call PlayT_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckU:
        cmp al, 'U'       
        jne CheckV
        call PlayU_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckV:
        cmp al, 'V'        
        jne CheckW
        call PlayV_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckW:
        cmp al, 'W'        
        jne CheckX
        call PlayW_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckX:
        cmp al, 'X'        
        jne CheckY
        call PlayX_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckY:
        cmp al, 'Y'         
        jne CheckZ
        call PlayY_Morse
        jmp NextIteration ; Jump to next character after handling a match

        CheckZ:
        cmp al, 'Z'         
        jne Check0
        call PlayZ_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check0:
        cmp al, '0'        
        jne Check1
        call Play0_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check1:
        cmp al, '1'
        jne Check2
        call Play1_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check2:
        cmp al, '2'
        jne Check3
        call Play2_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check3:
        cmp al, '3'
        jne Check4
        call Play3_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check4:
        cmp al, '4'
        jne Check5
        call Play4_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check5:
        cmp al, '5'
        jne Check6
        call Play5_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check6:
        cmp al, '6'
        jne Check7
        call Play6_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check7:
        cmp al, '7'
        jne Check8
        call Play7_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check8:
        cmp al, '8'
        jne Check9
        call Play8_Morse
        jmp NextIteration ; Jump to next character after handling a match

        Check9:
        cmp al, '9'
        call Play9_Morse

    NextIteration:
        pop si
        pop cx
        inc si              ; Move to next character address
        dec cx              ; Decrement character count
        jcxz Done             ; If no more characters, exit loop
        jmp CheckCharacter ;jump back if CX > 0

    Done:
        ret
endp StartMorse

; --- Letters A-Z ---
proc PlayA_Morse
	push 'A'
	call LoadLetterBMP
	
    call PlayDot
    call PlayDash
    call LetterSpace

    ret
endp PlayA_Morse

proc PlayB_Morse
	push 'B'
	call LoadLetterBMP

    call PlayDash
    call PlayDot
    call PlayDot
    call PlayDot
    call LetterSpace

    ret
endp PlayB_Morse

proc PlayC_Morse
	push 'C'
	call LoadLetterBMP

    call PlayDash
    call PlayDot
    call PlayDash
    call PlayDot
    call LetterSpace

    ret
endp PlayC_Morse

proc PlayD_Morse
    push 'D'
    call LoadLetterBMP

    call PlayDash
    call PlayDot
    call PlayDot
    call LetterSpace

    ret
endp PlayD_Morse

proc PlayE_Morse
    push 'E'
    call LoadLetterBMP

    call PlayDot
    call LetterSpace

    ret
endp PlayE_Morse

proc PlayF_Morse
    push 'F'
    call LoadLetterBMP

    call PlayDot
    call PlayDot
    call PlayDash
    call PlayDot
    call LetterSpace

    ret
endp PlayF_Morse

proc PlayG_Morse
    push 'G'
    call LoadLetterBMP

    call PlayDash
    call PlayDash
    call PlayDot
    call LetterSpace

    ret
endp PlayG_Morse

proc PlayH_Morse
    push 'H'
    call LoadLetterBMP

    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDot
    call LetterSpace

    ret
endp PlayH_Morse

proc PlayI_Morse
    push 'I'
    call LoadLetterBMP

    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp PlayI_Morse

proc PlayJ_Morse
    push 'J'
    call LoadLetterBMP

    call PlayDot
    call PlayDash
    call PlayDash
    call PlayDash
    call LetterSpace

    ret
endp PlayJ_Morse

proc PlayK_Morse
    push 'K'
    call LoadLetterBMP

    call PlayDash
    call PlayDot
    call PlayDash
    call LetterSpace
    ret
endp PlayK_Morse

proc PlayL_Morse
    push 'L'
    call LoadLetterBMP

    call PlayDot
    call PlayDash
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp PlayL_Morse

proc PlayM_Morse
    push 'M'
    call LoadLetterBMP

    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp PlayM_Morse

proc PlayN_Morse
    push 'N'
    call LoadLetterBMP

    call PlayDash
    call PlayDot
    call LetterSpace
    ret
endp PlayN_Morse

proc PlayO_Morse
    push 'O'
    call LoadLetterBMP

    call PlayDash
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp PlayO_Morse

proc PlayP_Morse
    push 'P'
    call LoadLetterBMP

    call PlayDot
    call PlayDash
    call PlayDash
    call PlayDot
    call LetterSpace
    ret
endp PlayP_Morse

proc PlayQ_Morse
    push 'Q'
    call LoadLetterBMP

    call PlayDash
    call PlayDash
    call PlayDot
    call PlayDash
    call LetterSpace
    ret
endp PlayQ_Morse

proc PlayR_Morse
    push 'R'
    call LoadLetterBMP
    
    call PlayDot
    call PlayDash
    call PlayDot
    call LetterSpace
    ret
endp PlayR_Morse

proc PlayS_Morse
    push 'S'
    call LoadLetterBMP

    call PlayDot
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp PlayS_Morse

proc PlayT_Morse
    push 'T'
    call LoadLetterBMP

    call PlayDash
    call LetterSpace
    ret
endp PlayT_Morse

proc PlayU_Morse
    push 'U'
    call LoadLetterBMP

    call PlayDot
    call PlayDot
    call PlayDash
    call LetterSpace
    ret
endp PlayU_Morse

proc PlayV_Morse
    push 'V'
    call LoadLetterBMP

    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDash
    call LetterSpace
    ret
endp PlayV_Morse

proc PlayW_Morse
    push 'W'
    call LoadLetterBMP
    
    call PlayDot
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp PlayW_Morse

proc PlayX_Morse
    push 'X'
    call LoadLetterBMP

    call PlayDash
    call PlayDot
    call PlayDot
    call PlayDash
    call LetterSpace
    ret
endp PlayX_Morse

proc PlayY_Morse
    push 'Y'
    call LoadLetterBMP

    call PlayDash
    call PlayDot
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp PlayY_Morse

proc PlayZ_Morse
    push 'Z'
    call LoadLetterBMP

    call PlayDash
    call PlayDash
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp PlayZ_Morse

; --- Digits 0-9 ---
proc Play0_Morse
    call PlayDash
    call PlayDash
    call PlayDash
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp Play0_Morse

proc Play1_Morse
    call PlayDot
    call PlayDash
    call PlayDash
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp Play1_Morse

proc Play2_Morse
    call PlayDot
    call PlayDot
    call PlayDash
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp Play2_Morse

proc Play3_Morse
    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDash
    call PlayDash
    call LetterSpace
    ret
endp Play3_Morse

proc Play4_Morse
    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDash
    call LetterSpace
    ret
endp Play4_Morse

proc Play5_Morse
    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp Play5_Morse

proc Play6_Morse
    call PlayDash
    call PlayDot
    call PlayDot
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp Play6_Morse

proc Play7_Morse
    call PlayDash
    call PlayDash
    call PlayDot
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp Play7_Morse

proc Play8_Morse
    call PlayDash
    call PlayDash
    call PlayDash
    call PlayDot
    call PlayDot
    call LetterSpace
    ret
endp Play8_Morse

proc Play9_Morse
    call PlayDash
    call PlayDash
    call PlayDash
    call PlayDash
    call PlayDot
    call LetterSpace
    ret
endp Play9_Morse

proc Delay
    push ax
    push cx
    push dx
    push si
    push di

    DelayLoop:
        cmp bx, 0
        je EndDelay

        mov ah, 0
        int 1Ah             ; CX:DX = current ticks
        add dx, 4           ; 4 ticks per BX unit (~220ms each)
        adc cx, 0
        mov si, cx          ; target high in SI
        mov di, dx          ; target low in DI

        WaitTick:
            mov ah, 0
            int 1Ah         ; read current ticks into CX:DX
            cmp cx, si
            jb WaitTick     ; current high < target high, keep waiting
            ja EndWait      ; current high > target high, done
            cmp dx, di
            jb WaitTick     ; same high word, check low word
        EndWait:

        dec bx
        jmp DelayLoop

    EndDelay:
        pop di
        pop si
        pop dx
        pop cx
        pop ax
        ret
endp Delay

; --- Sound Procedures ---
proc StartSound
    push ax
    ; Set Frequency (750Hz)
    mov al, 0B6h
    out 43h, al
    mov ax, 0637h   ; Divisor for 750Hz
    out 42h, al     
    mov al, ah
    out 42h, al     

    ; Turn Speaker On
    in al, 61h
    or al, 03h      
    out 61h, al
    pop ax
    ret            
endp StartSound

proc StopSound
    push ax
    in al, 61h
    and al, 0FCh    ; Cleanly turn off bits 0 and 1 (11111100b)
    out 61h, al
    pop ax
    ret            
endp StopSound

proc PlayDot
    call StartSound
    mov bx, 1       
    call Delay
    call StopSound
    mov bx, 1       ; Space BETWEEN parts of same letter
    call Delay
    ret
endp PlayDot

proc PlayDash
    call StartSound
    mov bx, 3       
    call Delay
    call StopSound
    mov bx, 1       ; Space BETWEEN parts of same letter
    call Delay
    ret
endp PlayDash

proc LetterSpace
    mov bx, 3       ; Space BETWEEN different letters
    call Delay
    ret
endp LetterSpace
exit:
	mov ax, 4c00h
	int 21h
END start


