[org 0x100]

 jmp start

 goodLuck_line: db 'GOOD LUCK!',0
 score_line: db 'SCORE' ,0
 timer_line: db 'TIME' ,0
 next_line:db 'NEXT SHAPE',0
 game_over: db 'GAME OVER!'
 score: dw 0
 gameEnded: db 'n'
 empty: dw 0x0020
 block: dw 0x3020;2220h  ;7896
 InitialDiPosition: dw 54
 NextShape: dw 0x7020

;;;;--------------------variables for outerbloundries of active shape

;;;;for right side
r1: dw 0
r2: dw 0
r3: dw 0
;;;;for left side
l1: dw 0
l2: dw 0
l3: dw 0
;;;;for bottom
b1: dw 0
b2: dw 0
b3: dw 0
b4: dw 0
b5: dw 0
b6: dw 0
;;;;--------------------edge detection variables
can_appear: db'y'        ;    y -> allowed        n -> not allowed
can_i_go_down: db'y'
can_i_go_right: db 'y'
can_i_go_left: db 'y'
right_collision: dw 2220h, 7020h

;;;;--------------------
promotion: db 'Powered By Brox Gaming'
oldisr: dd 0 ; space for saving old isr
oldtimer: dd 0 ; space for old timer
right: db 0
left:db 0
down: db 0
escape: db 0
pauseCheck: db 0
tickcount: dw 0
seconds: dw 0
minutes: dw 0
colon: db ':'
random: dw 0
time_check: db '1'

;;;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;;;
;;;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---------FUNCTIONS---------xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;;;
;;;xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;;;

clr:

    push es
    push ax
    push cx
    push di
    mov ax, 0xb800
    mov es,ax
    mov di,0
    mov ax,0x0020
    mov cx,2000
    rep stosw
    pop di
    pop cx
    pop ax
    pop es
    ret

welcome_clr:

    push es
    push ax
    push cx
    push di
    mov ax, 0xb800
    mov es,ax
    mov di,0
    ; mov al, 'o'    ;;;;----occupied space, shapes cant move here
    ; mov ah,0x00
    mov ah,0x03
    mov al,176
    mov cx,2000
    rep stosw

    pop di
    pop cx
    pop ax
    pop es
    ret

;;;===============================================================;;;
;;;==========================DELAY FUNCTIONS======================;;;
;;;===============================================================;;;

short_delay:
    push bx
    push cx
    mov bx,0
    mov cx,0x6666
    short_delay_loop:
        inc bx
        loop short_delay_loop
    pop cx
    pop bx
    ret

delay:
    push bx
    push cx
    mov bx,0
    mov cx,0xffff
    delay_loop:
        inc bx
        loop delay_loop
    pop cx
    pop bx
    ret

medium_delay:
    push bx
    push cx
    push dx
    mov dx,3
    mov bx,0
    mov cx,0x9999
    medium_delay_loop:
        inc bx
        loop medium_delay_loop
        mov cx,0x9999
        dec dx
        jnz medium_delay_loop
    pop dx
    pop cx
    pop bx
    ret
 
long_delay:
    push cx
    mov cx,35
    cd:
        call delay
        loop cd
    pop cx
    ret 

;;;===========================================================================================;;;
;;;==================================SCREEN SET-UP FUNCTIONS==================================;;;
;;;===========================================================================================;;;

StartingScreen:
    pusha
    push es

    call welcome_clr
    mov ax,0xb800
    mov es,ax
    mov ax,0x3320
    ;-----------printing 3
    mov di, 8*160  ;starting from 9th row
    add di,2*34
    mov bx,3
    three1:
        mov cx, 9
        rep stosw
        add di,3*160
        sub di,18
        dec bx
        jnz three1

    mov di,9*160
    add di,2*41
    mov cx,5
    three2:
        stosw
        stosw
        add di,156
        loop three2

    ;------------printing two
    call long_delay
    mov di,12*160
    add di,2*34
    mov cx,2
    two1:
        stosw
        stosw
        add di,156
        loop two1

    mov di,12*160
    add di,2*41
    mov ax,0x03b0
    mov cx, 2
    two2:
        stosw
        stosw
        add di,156
        loop two2

    ;------------printing one
    call long_delay
    call welcome_clr
    mov ax, 0x3320
    mov di, 8*160  ;;;;starting from 9th row
    add di,2*37
    mov cx,7
    one1:
        stosw
        stosw
        add di,156
        loop one1

    pop es
    popa
    call long_delay
    ret
 
SetUpGameSpace:
    pusha
    push es
    ;--------for grey part which is going to be our actual game area  
    mov dx,0xb800
    mov es,dx
    mov ax,[empty]
    mov di,0
    add di,2*7   ;;starting from 6th col and 1st row
    outer_for_grey:
        mov cx,48  ;width of grey i.e game area
        rep stosw
        add di,160
        sub di,96  ;to mov di to next line and 6th col
        cmp di,4000
        jnae outer_for_grey
    ;---------This is going to make side bar
    mov es,dx             ;dx b800h
    mov ax,160             
    mov di,0              ;move to 1st row
    add di, 2*56          ;move to 56th column
    mov ax,0x4020
    loop_for_bar:
        mov cx, 18            ; counter for number of chars in every printed at the right
        rep stosw
        cmp di,3988
        jae side_bar_done
        add di,124            ; di moves to 56th column of next row
        jmp loop_for_bar

    side_bar_done:
        ;----------Good Luck box
        mov di, 2*60  ; 60th column
        add di, 2*160  ; 3rd row
        mov es,dx
        mov si,goodLuck_line
        mov ah,0x4f
        mov cx,10
    nameLine:
        lodsb
        stosw
        loop nameLine

    ;----------Timer box
    mov di, 2*60  ; 59th column
    add di, 5*160  ; 7th row
    mov es,dx
    mov si,timer_line
    mov ah,0x4f
    mov cx,4
    timeLine:
        lodsb
        stosw
        loop timeLine
    

    ;----------Score box
    mov di, 2*59  ; 59th column
    add di, 8*160  ; 9th row
    mov es,dx
    mov si,score_line
    mov ah,0x4f
    mov cx,5
    scoreLine:
        lodsb
        stosw
        loop scoreLine

    add di, 2
    mov cx, 6   ;;;;width of box
    mov ax, 0x7020
    rep stosw
    ;-------------NExt shape box
    mov di,11*160
    add di,2*60   ;;;; start from 60th col and 10th row
    mov si,next_line
    mov ah,0x4f
    mov cx,10
    nextLine:
        lodsb
        stosw
        loop nextLine

    call print_promotion
    call delay
    pop es
    popa
    ret

print_promotion:
    pusha
    push es

    mov si,promotion
    mov cx,10
    mov di,23*160
    add di,2*57
    mov ah,0x47
    promo_loop1:
        lodsb
        stosw
        loop promo_loop1

    mov cx, 11
    inc si
    add di,148 ; to get to next line
    promo_loop2:
        lodsb
        stosw
        loop promo_loop2

    pop es
    popa
    ret

GameOverScreen:
    pusha 
    push es

    call long_delay
    mov ax,0xb800
    mov es,ax
    mov ax,0x03b0
    mov cx,2001
    rep stosw
    mov ax,0x3020
    mov di,10*160
    add di, 2*25
    mov bx,3
    bb:
        mov cx,30
        rep stosw
        add di,160
        sub di,60
        dec bx
        jnz bb

    mov cx,10   ;length of message
    mov si,game_over
    mov di,11*160
    add di,2*35
    mov ah,0xbf
    go1:
        lodsb
        stosw
        loop go1

    mov ah,0
    int 16h

    pop es
    popa
    ret

;;;========================================================================;;;
;;;==========================COLLISION FUNCTIONS===========================;;;
;;;========================================================================;;;

CheckIfBlockAvailable:
    push bp
    mov bp, sp
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    mov ax,[empty]  ;empty=0x7020
    mov dx,[bp+4]   ;width of block to be checked
    mov di,[bp+6]
    checkBox:
        mov cx,[bp+8]
        inc cx
        repe scasw
        cmp cx,0
        jne CantAppear
        sub di,16
        add di,160
        dec dx
        jnz checkBox
        jmp CanAppear

    CantAppear:
        mov byte[can_appear],'n'
        jmp Checked
    CanAppear:
        mov byte[can_appear],'y'
    Checked:
        pop es
        popa
        pop bp
        ret 6
    
BottomCollision:
    push bp
    mov bp, sp
    pusha
    push es
    mov ax,0xb800
    mov es, ax
    mov ax,[empty]
    mov cx,[bp+4]   ;number of pixels we have to check
    mov di,[bp+6]   ;bottom pixels passed as parameter
    add di,160
    cmp di,4000
    jae noBottom
    cld
    repe scasw
    cmp cx,0
    jne noBottom
    jmp yesBottom

    noBottom:
        mov byte[can_i_go_down],'n'
        jmp exitBC
    yesBottom:
        mov byte[can_i_go_down],'y'
    exitBC:
        pop es
        popa
        pop bp
        ret 4

RightCollision:
    push bp
    mov bp, sp
    pusha
    push es
    mov ax,0xb800
    mov es, ax
    mov ax,[empty]
    mov dx,[bp+4]   ;number of pixels we have to check
    mov di,[bp+6]   ;right pixel passed as parameter
    add di,2
    RightLoop:
        cmp word[es:di],ax
        jne NoRight
        add di,160
        dec dx
        jnz RightLoop
    jmp yesRight

    NoRight:
        mov byte[can_i_go_right],'n'
        jmp exitRC
    yesRight:
        mov byte[can_i_go_right],'y'
    exitRC:
        pop es
        popa
        pop bp
        ret 4

LeftCollision:
    push bp
    mov bp, sp
    pusha
    push es
    mov ax,0xb800
    mov es, ax
    mov ax,[empty]
    mov dx,[bp+4]   ;number of pixels we have to check
    mov di,[bp+6]   ;right pixel passed as parameter
    sub di,2
    LeftLoop:
        cmp word[es:di],ax
        jne NoLeft
        add di,160
        dec dx
        jnz LeftLoop
    jmp yesLeft

    NoLeft:
        mov byte[can_i_go_left],'n'
        jmp exitRC
    yesLeft:
        mov byte[can_i_go_left],'y'
    exitLC:
        pop es
        popa
        pop bp
        ret 4

CheckIfLinesAreFilled:
    pusha
    push word[l3]
    call CheckLine
    push word[l2]
    call CheckLine
    push word[l1]
    call CheckLine
    popa
    ret

CheckLine:
    push bp
    mov bp,sp
    pusha
    push es
    mov ax,0xb800
    mov es,ax

    ; mov di,12
    ; mov word[es:di],0x5520
    ; add di,94
    ; mov word[es:di],0x5520

    mov cl,160
    mov ax,[bp+4]
    cmp ax,0
    je DoNothing
    div cl
    mul cl
    mov di,ax 
    add di,14   ; Now di contains staring point of the line that contains top of the current shaoe
    ;mov word[es:di],0x5520
    mov ax,[block]
 
    startChecking:
        mov cx,49
        repe scasw
        sub di,98
           ;mov word[es:di],0x5520
        cmp cx,0
        jne DoNothing
        push di
        call RemoveLine

    DoNothing:
        pop es
        popa
        pop bp
        ret 2

RemoveLine:
    push bp
    mov bp,sp
    pusha
    push es
    push ds
    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
    mov ax,[empty]
    push di     ; for latter use i.e scrolling
    mov cx,48
    push di
    call BlinkLine
    RL1:
        stosw
        call short_delay
        loop RL1

    push es
    pop ds
    pop ax
    mov di,ax
    mov cl,160
    div cl
    mov dx,0
    mov dl,al ; now dx contains number of lines to be shifted down
    cld
    ScrollLoop:
        mov si,di
        sub si,160
        mov cx,48
        rep movsw
        sub si,96
        sub si,160
        sub di,96
        sub di,160
        dec dx
        jnz ScrollLoop

    pop ds
    call UpdateScore
    mov ax,[empty]
    mov cx,48
    mov di,14
    rep stosw
 
    pop es
    popa
    pop bp
    ret 2

BlinkLine:
    push bp
    mov bp,sp
    pusha
    push es
    mov cx,48
    mov ax,0xb800
    mov es,ax
    mov ax,[empty]
    mov di,[bp+4]
    rep stosw
    call medium_delay
    call medium_delay
    mov ax,[block]
    mov cx,48
    mov di,[bp+4]
    rep stosw
    call medium_delay
    call medium_delay
    mov ax,[empty]
    mov cx,48
    mov di,[bp+4]
    rep stosw
    call medium_delay
    call medium_delay
    mov ax,[block]
    mov cx,48
    mov di,[bp+4]
    rep stosw
    call medium_delay
    call medium_delay
    mov ax,[empty]
    mov cx,48
    mov di,[bp+4]
    rep stosw
    call medium_delay
    call medium_delay
    mov ax,[block]
    mov cx,48
    mov di,[bp+4]
    rep stosw

    pop es
    popa
    pop bp
    ret 2
 
;;;================================================================================;;;
;;;==================          DRAW SHAPES FUNCTIONS             ==================;;;
;;;================================================================================;;;

draw_Lshape:

    push bp
    mov bp,sp
    pusha
    push es
    mov dx,0xb800
    mov es,dx
    mov di,[bp+4]
 
    cmp word[bp+8],0
    jne NoCheckForL

    push 4
    push di
    push 2  ;length of block
    call CheckIfBlockAvailable

    cmp byte[can_appear],'y'
    je NoCheckForL
 
    LCantAppear:
        mov byte[gameEnded],'y'
        mov byte [time_check],0
        jmp Ldraw_exit

    NoCheckForL: ;After checking for space now we draw it
       
        mov ax,[bp+6]
        mov[l1],di
        stosw
        mov[r1],di
        stosw
        mov cx,4 ;there are 4 bottom pixels of L shape
        sub di,4
        add di,160
        mov [l2],di
        mov si,0
        drawL1:
            mov [b1+si],di
            stosw
            add si,2
            loop drawL1
        
        sub di,2
        mov [r2],di

        ;boundry ssegment that is not used by current shape
        mov word[b5],0
        mov word[b6],0
        mov word[r3],0
        mov word[l3],0

    Ldraw_exit:
        pop es
        popa
        pop bp
        ret 6

draw_Tshape:

    push bp
    mov bp,sp
    pusha
    push es
    mov dx,0xb800
    mov es,dx
  
    mov di,[bp+4]
    cmp word[bp+8],0
    jne NoCheckForT
    push 6
    push di
    push 2  ;width of block
    call CheckIfBlockAvailable
    cmp byte[can_appear],'y'
    je NoCheckForT

    TCantAppear:
        mov byte[gameEnded],'y'
        mov byte [time_check],0
        jmp Tdraw_exit

    NoCheckForT: ;After checking for space now we draw it
        mov ax,[bp+6]
        mov [l1],di
        mov si,0
        mov cx,6
        drawT1:
            mov [b1+si],di
            stosw
            add si,2
            loop drawT1    
        sub di,2
        mov [r1],di
        sub di,6
        add di,160
        mov[l2],di
        mov [b3],di
        stosw
        mov[r2],di
        mov [b4],di
        stosw
        ;boundry ssegment that is not used by current shape
        mov word[r3],0
        mov word[l3],0

    Tdraw_exit:
        pop es
        popa
        pop bp
        ret 6

draw_Ushape:

    push bp
    mov bp,sp
    pusha
    push es
    mov dx,0xb800
    mov es,dx     
    mov di,[bp+4]

    cmp word[bp+8],0
    jne NoCheckForU

    push 6
    push di
    push 2  ;length of block
    call CheckIfBlockAvailable
    cmp byte[can_appear],'y'
    je NoCheckForU

    UCantAppear:
        mov byte[gameEnded],'y'
        mov byte [time_check],0
        jmp Tdraw_exit
    NoCheckForU: ;After checking for space now we draw it
        mov ax,[bp+6] ;attribute
        mov si,0
        mov [l1],di
        stosw
        stosw
        add di,4
        stosw
        mov [r1],di
        stosw
        sub di,12
        add di,160
        mov cx,6
        mov si,0
        mov[l2],di
        drawU1:
            mov [b1+si],di
            stosw
            add si,2
            loop drawU1

        sub di,2
        mov [r2],di
        ;boundry ssegment that is not used by current shape
        mov word[r3],0
        mov word[l3],0

    Udraw_exit:
        pop es
        popa
        pop bp
        ret 6

draw_Hshape:

    push bp
    mov bp,sp
    pusha
    push es
    mov dx,0xb800
    mov es,dx
    mov di,[bp+4]
 
    cmp word[bp+8],0
    jne NoCheckForH
    push 6
    push di
    push 1  ;hight of block
    call CheckIfBlockAvailable
    cmp byte[can_appear],'y'
    je NoCheckForH

    HCantAppear:
        mov byte[gameEnded],'y'
        mov byte [time_check],0
        jmp Hdraw_exit
    NoCheckForH: ;After checking for space now we draw it
        mov ax,[bp+6]
        mov[l1],di
        mov cx,6
        mov si,0
        drawH1:
            mov [b1+si],di
            stosw
            add si,2
            loop drawH1
        sub di,2
        mov word[r1],di
        ;boundry ssegment that is not used by current shape
        mov word[r2],0
        mov word[l2],0
        mov word[r3],0
        mov word[l3],0
    Hdraw_exit:
        pop es
        popa
        pop bp
        ret 6

draw_Bshape:

    push bp
    mov bp,sp
    pusha
    push es
    mov dx,0xb800
    mov es,dx
    mov di,[bp+4]
 
    cmp word[bp+8],0
    jne NoCheckForB
    push 4
    push di
    push 2  ;hight of block
    call CheckIfBlockAvailable
    cmp byte[can_appear],'y'
    je NoCheckForB

    BCantAppear:
        mov byte[gameEnded],'y'
        mov byte [time_check],0
        jmp Bdraw_exit
    NoCheckForB: ;After checking for space now we draw it
        mov ax,[bp+6]
        mov si,0
        mov cx,4
        mov [l1],di
        rep stosw
        sub di,2
        mov [r1],di
        sub di,6
        add di,160
        mov [l2],di
        mov cx,4
        drawB1:
            mov [b1+si],di
            stosw
            add si,2
            loop drawB1
        sub di,2
        mov [r2],di
        ;boundry ssegment that is not used by current shape
        mov word[b5],0
        mov word[b6],0
        mov word[r3],0
        mov word[l3],0

    Bdraw_exit:
        pop es
        popa
        pop bp
        ret 6

draw_Vshape:

    push bp
    mov bp,sp
    pusha
    push es
    mov dx,0xb800
    mov es,dx     
    mov di,[bp+4]
    add di,4

    cmp word[bp+8],0
    jne NoCheckForV

    push 2
    push di
    push 3  ;length of block
    call CheckIfBlockAvailable
    cmp byte[can_appear],'y'
    je NoCheckForV

    VCantAppear:
        mov byte[gameEnded],'y'
        mov byte [time_check],0
        jmp Vdraw_exit
    NoCheckForV: ;After checking for space now we draw it
        mov ax,[bp+6] ;attribute
        mov cx,3
        mov si,0
        drawV1:
            mov [l1+si],di
            stosw
            mov [r1+si],di
            stosw
            add si,2
            sub di,4
            add di,160
            loop drawV1

        sub di,160
        mov [b1],di
        add di,2
        mov [b2],si
        ;boundry ssegment that is not used by current shape
        mov word[b3],0
        mov word[b4],0
        mov word[b5],0
        mov word[b6],0

    Vdraw_exit:
        pop es
        popa
        pop bp
        ret 6

;;;=============================================================================;;;
;;;=========================       MOVE DOWN FUNCTIONS       ===================;;;
;;;=============================================================================;;;

move_Ldown:

    pusha
    mov dx,[l1]
    LdownLoop:
        push word[b1]
        push 5
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noLDown
        push 1
        push word[empty]
        push dx
        call draw_Lshape
        add dx,160
        push 1
        push word[block]
        push dx
        call draw_Lshape
        ;jmp LdownLoop
         
    noLDown:
        popa
        ret

move_Tdown:
    pusha
    mov dx,[l1]
    TdownLoop:
        push word[b1]
        push 3
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noTDown
        push word[b3]
        push 3
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noTDown
        push word[b5]
        push 3
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noTDown
        push 1
        push word[empty]
        push dx
        call draw_Tshape
        add dx,160
        push 1
        push word[block]
        push dx
        call draw_Tshape
        ;jmp TdownLoop
         
    noTDown:
        popa
        ret

move_Udown:

    pusha
    mov dx,[l1]
    UdownLoop:
        push word[b1]
        push 7
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noUDown
        push 1
        push word[empty]
        push dx
        call draw_Ushape
        add dx,160
        push 1
        push word[block]
        push dx
        call draw_Ushape
        ;jmp UdownLoop
         
    noUDown:
        popa
        ret

move_Hdown:

    pusha
    mov dx,[l1]
    HdownLoop:
        push word[b1]
        push 7
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noHDown
        push 1
        push word[empty]
        push dx
        call draw_Hshape
        add dx,160
        push 1
        push word[block]
        push dx
        call draw_Hshape
        ;jmp HdownLoop
         
    noHDown:
        popa
        ret

move_Bdown:

    pusha
    mov dx,[l1]
    BdownLoop:
        push word[b1]
        push 5
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noBDown
        push 1
        push word[empty]
        push dx
        call draw_Bshape
        add dx,160
        push 1
        push word[block]
        push dx
        call draw_Bshape
        ;jmp BdownLoop
         
    noBDown:
        popa
        ret

move_Vdown:
    pusha
    mov dx,[l1]
    sub dx,4
    VdownLoop:
        push word[b1]
        push 3
        call BottomCollision
        cmp byte[can_i_go_down],'n'
        je noVDown
        push 1
        push word[empty]
        push dx
        call draw_Vshape
        add dx,160
        push 1
        push word[block]
        push dx
        call draw_Vshape
          
    noVDown:
        popa
        ret


;;;================================================================;;;
;;;=========================MOVE RIGHT FUNCTIONS===================;;;
;;;================================================================;;;
move_Uright:
    pusha
    mov dx,[l1]
    URightLoop:
        push word[r1]
        push 2
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noURight
        push 1
        push word[empty]
        push dx
        call draw_Ushape
        add dx,2
        push 1
        push word[block]
        push dx
        call draw_Ushape
        ;jmp URightLoop
         
    noURight:
        popa
        ret

move_Lright:
    pusha
    mov dx,[l1]
    LRightLoop:
        push word[r1]
        push 1
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noLRight
        push word[r2]
        push 1
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noLRight
        push 1
        push word[empty]
        push dx
        call draw_Lshape
        add dx,2
        push 1
        push word[block]
        push dx
        call draw_Lshape
        ;jmp LRightLoop
         
    noLRight:
        popa
        ret

move_Tright:
    pusha
    mov dx,[l1]
    TRightLoop:
        push word[r1]
        push 1
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noTRight
        push word[r2]
        push 1
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noTRight
        push 1
        push word[empty]
        push dx
        call draw_Tshape
        add dx,2
        push 1
        push word[block]
        push dx
        call draw_Tshape
        ;jmp TRightLoop
         
    noTRight:
        popa
        ret

move_Hright:
    pusha
    mov dx,[l1]
    HRightLoop:
        push word[r1]
        push 1
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noHRight
        push 1
        push word[empty]
        push dx
        call draw_Hshape
        add dx,2
        push 1
        push word[block]
        push dx
        call draw_Hshape
        ;jmp HRightLoop
         
    noHRight:
        popa
        ret

move_Bright:
    pusha
    mov dx,[l1]
    BRightLoop:
        push word[r1]
        push 2
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noBRight
        push 1
        push word[empty]
        push dx
        call draw_Bshape
        add dx,2
        push 1
        push word[block]
        push dx
        call draw_Bshape
        ;jmp BRightLoop
         
    noBRight:
        popa
        ret

move_Vright:
    pusha
    mov dx,[l1]
    sub dx,4
    VRightLoop:
        push word[r1]
        push 3
        call RightCollision
        cmp byte[can_i_go_right],'n'
        je noVRight
        push 1
        push word[empty]
        push dx
        call draw_Vshape
        add dx,2
        push 1
        push word[block]
        push dx
        call draw_Vshape
          
    noVRight:
        popa
        ret

    

;;;===============================================================;;;
;;;=========================MOVE LEFT FUNCTIONS===================;;;
;;;===============================================================;;;
move_Uleft:
    pusha
    mov dx,[l1]
    ULeftLoop:
        push word[l1]
        push 2
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noULeft
        push 1
        push word[empty]
        push dx
        call draw_Ushape
        sub dx,2
        push 1
        push word[block]
        push dx
        call draw_Ushape
        ;jmp ULeftLoop
         
    noULeft:
        popa
        ret

move_Lleft:
    pusha
    mov dx,[l1]
    LLeftLoop:
        push word[l1]
        push 2
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noLLeft
        push 1
        push word[empty]
        push dx
        call draw_Lshape
        sub dx,2
        push 1
        push word[block]
        push dx
        call draw_Lshape
        ;jmp LLeftLoop
         
    noLLeft:
        popa
        ret

move_Tleft:
    pusha
    mov dx,[l1]
    TLeftLoop:
        push word[l1]
        push 1
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noTLeft
        push word[l2]
        push 1
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noTLeft
        push 1
        push word[empty]
        push dx
        call draw_Tshape
        sub dx,2
        push 1
        push word[block]
        push dx
        call draw_Tshape
        ;jmp TLeftLoop
         
    noTLeft:
        popa
        ret

move_Hleft:
    pusha
    mov dx,[l1]
    HLeftLoop:
        push word[l1]
        push 1
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noHLeft
        push 1
        push word[empty]
        push dx
        call draw_Hshape
        sub dx,2
        push 1
        push word[block]
        push dx
        call draw_Hshape
        ;jmp HLeftLoop
         
    noHLeft:
        popa
        ret

move_Bleft:
    pusha
    mov dx,[l1]
    BLeftLoop:
        push word[l1]
        push 2
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noBLeft
        push 1
        push word[empty]
        push dx
        call draw_Bshape
        sub dx,2
        push 1
        push word[block]
        push dx
        call draw_Bshape
        ;jmp BLeftLoop
         
    noBLeft:
        popa
        ret

move_Vleft:
    pusha
    mov dx,[l1]
    sub dx,4
    VLeftLoop:
        push word[l1]
        push 3
        call LeftCollision
        cmp byte[can_i_go_left],'n'
        je noVLeft
        push 1
        push word[empty]
        push dx
        call draw_Vshape
        sub dx,2
        push 1
        push word[block]
        push dx
        call draw_Vshape
          
    noVLeft:
        popa
        ret

;;;=====================================================================;;;
;;;=====================ALL IN ONE MOVEMENTS FUNCTIONS==================;;;
;;;=====================================================================;;;

MoveLshape:
    pusha
    push 0   ;0 means new shape 1 means old shape
    push word[block]
    push word[InitialDiPosition]
    call draw_Lshape
    MoveLshapeLoop:
        call Pause_func
        cmp byte[gameEnded],'y'
        je LbhaiGameEnded
		cmp word [right],1
		jne check_left_movementL
        call move_Lright
        call move_Lright
		check_left_movementL:
            cmp byte[gameEnded],'y'
            je LbhaiGameEnded
            cmp word [left],1
            jne Ldown
            call move_Lleft
            call move_Lleft
        Ldown:
            cmp byte[gameEnded],'y'
            je LbhaiGameEnded   
            cmp word[down],1
            jne oneLdown
            call move_Ldown
            oneLdown:
            call medium_delay
            call move_Ldown
        cmp byte[can_i_go_down],'n'
        jne MoveLshapeLoop
    LbhaiGameEnded:
    popa 
    ret

MoveTshape:
    pusha
    push 0   ;0 means new shape 1 means old shape
    push word[block]
    push word[InitialDiPosition]
    call draw_Tshape
    MoveTshapeLoop:
        call Pause_func
        cmp byte[gameEnded],'y'
        je TbhaiGameEnded
		cmp word [right],1
		jne check_left_movementT
        call move_Tright
        call move_Tright
		check_left_movementT:
            cmp byte[gameEnded],'y'
            je TbhaiGameEnded
            cmp word [left],1
            jne Tdown
            call move_Tleft
            call move_Tleft
        Tdown:
            cmp byte[gameEnded],'y'
            je TbhaiGameEnded
            cmp word[down],1
            jne oneTdown
            call move_Tdown
            oneTdown:
            call medium_delay
            call move_Tdown
        cmp byte[can_i_go_down],'n'
        jne MoveTshapeLoop

    TbhaiGameEnded:
        popa 
        ret

MoveUshape:
    pusha
    push 0   ;0 means new shape 1 means old shape
    push word[block]
    push word[InitialDiPosition]
    call draw_Ushape
    MoveUshapeLoop:
         call Pause_func
        cmp byte[gameEnded],'y'
        je UbhaiGameEnded
		cmp word [right],1
		jne check_left_movementU
        call move_Uright
        call move_Uright
		check_left_movementU:
            cmp byte[gameEnded],'y'
            je UbhaiGameEnded
            cmp word [left],1
            jne Udown
            call move_Uleft
            call move_Uleft
        Udown:
            cmp byte[gameEnded],'y'
            je UbhaiGameEnded
            cmp word[down],1
            jne oneUdown
            call move_Udown
            oneUdown:
            call medium_delay
            call move_Udown
        cmp byte[can_i_go_down],'n'
        jne MoveUshapeLoop
    UbhaiGameEnded:
        popa 
        ret

MoveHshape:
    pusha
    push 0   ;0 means new shape 1 means old shape
    push word[block]
    push word[InitialDiPosition]
    call draw_Hshape
    MoveHshapeLoop:
        call Pause_func
        cmp byte[gameEnded],'y'
        je HbhaiGameEnded
		cmp word [right],1
		jne check_left_movementH
        call move_Hright
        call move_Hright
		check_left_movementH:
            cmp byte[gameEnded],'y'
            je HbhaiGameEnded
            cmp word [left],1
            jne Hdown
            call move_Hleft
            call move_Hleft
        Hdown:
            cmp byte[gameEnded],'y'
            je HbhaiGameEnded
            cmp word[down],1
            jne oneHdown
            call move_Hdown
            oneHdown:
            call medium_delay
            call move_Hdown
        cmp byte[can_i_go_down],'n'
        jne MoveHshapeLoop
    HbhaiGameEnded:
        popa 
        ret

MoveBshape:
    pusha
    push 0   ;0 means new shape 1 means old shape
    push word[block]
    push word[InitialDiPosition]
    call draw_Bshape
    MoveBshapeLoop:
        call Pause_func
        cmp byte[gameEnded],'y'
        je BbhaiGameEnded
		cmp word [right],1
		jne check_left_movementB
        call move_Bright
        call move_Bright
		check_left_movementB:
            cmp byte[gameEnded],'y'
            je BbhaiGameEnded
            cmp word [left],1
            jne Bdown
            call move_Bleft
            call move_Bleft
        Bdown:
            cmp byte[gameEnded],'y'
            je BbhaiGameEnded
            cmp word[down],1
            jne oneBdown
            call move_Bdown
            oneBdown:
            call medium_delay
            call move_Bdown
        cmp byte[can_i_go_down],'n'
        jne MoveBshapeLoop
    BbhaiGameEnded:
        popa 
        ret

MoveVshape:
    pusha
    push 0   ;0 means new shape 1 means old shape
    push word[block]
    push word[InitialDiPosition]
    call draw_Vshape
    MoveVshapeLoop:
         call Pause_func
        cmp byte[gameEnded],'y'
        je VbhaiGameEnded
		cmp word [right],1
		jne check_left_movementV
        call move_Vright
        call move_Vright
        ;call move_Vright
		check_left_movementV:
            cmp byte[gameEnded],'y'
            je VbhaiGameEnded
            cmp word [left],1
            jne Vdown
            call move_Vleft
            call move_Vleft
            ;call move_Vleft
        Vdown:
            cmp byte[gameEnded],'y'
            je VbhaiGameEnded
            cmp word[down],1
            jne oneVdown
            call move_Vdown
            oneVdown:
            call medium_delay
            call move_Vdown
        cmp byte[can_i_go_down],'n'
        jne MoveVshapeLoop
    VbhaiGameEnded:
        popa 
        ret

MoveCurrentShape:
    push bp
    mov bp,sp
    pusha

    mov ax,[bp+4]
    cmp ax,1
    je L
    cmp ax,2
    je T
    cmp ax,3
    je B
    cmp ax,4
    je U
    cmp ax,5
    je H
    cmp ax,6
    je V

    L:
        call MoveLshape
        jmp MovementDone
    U:
        call MoveUshape
        jmp MovementDone
    H:
        call MoveHshape
        jmp MovementDone
    B:
        call MoveBshape
        jmp MovementDone
    T:
        call MoveTshape
        jmp MovementDone
    V:
        call MoveVshape
        jmp MovementDone

    MovementDone:
        call CheckIfLinesAreFilled

    popa
    pop bp
    ret 2

;;;===========================================================;;;
;;;=====================NEXT SHAPE FUNCTIONS==================;;;
;;;===========================================================;;;

clr_next_shape_box:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    mov ax,0x4420
    mov di,2*61
    add di,14*160
    mov dx,4
    cnsb:
        mov cx,8
        rep stosw
        sub di,16
        add di,160
        dec dx
        jnz cnsb
     
    pop es
    popa
    ret

draw_NextL:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    call clr_next_shape_box
    mov ax,[NextShape]
    mov di,2*63
    add di,14*160
    
    dnl1:
        stosw
        stosw
        sub di,4
        add di,160
        ; loop dnl1

    mov cx,4
    rep stosw
    

    pop es
    popa
    ret

draw_NextU:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    call clr_next_shape_box
    mov ax,[NextShape]
    mov di,2*62
    add di,14*160
    
    dnu1:
         stosw
        stosw
        sub di,4
        add di,160

    mov cx,6
    rep stosw

    sub di,4
    sub di,160
    mov cx,3
    dnu2:
        stosw
        stosw
       

    pop es
    popa
    ret

draw_NextT:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    call clr_next_shape_box
    mov ax,[NextShape]
    mov di,2*62
    add di,14*160
    mov cx,6
    rep stosw
    sub di,8
    add di,160
    
    dnt1:
        stosw
        stosw
    
    pop es
    popa
    ret

draw_NextH:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    call clr_next_shape_box
    mov ax,[NextShape]
    mov di,2*62
    add di,14*160
    mov cx,6
    rep stosw
     
    pop es
    popa
    ret

draw_NextB:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    call clr_next_shape_box
    mov ax,[NextShape]
    mov di,2*62
    add di,14*160
    dnb1:
        mov cx,5
        rep stosw
        sub di,10
        add di,160
        mov cx,5
        rep stosw
        
        ;dec dx
       ; jnz dnb1
     
    pop es
    popa
    ret

draw_NextV:
    pusha
    push es
    mov ax,0xb800
    mov es,ax
    call clr_next_shape_box
    mov ax,[NextShape]
    mov di,2*64
    add di,14*160
    mov cx,2

    dnv1:
    stosw
    stosw
    sub di,4
    add di,160
    loop dnv1

    pop es
    popa
    ret

AppearNextShape:
    push bp
    mov bp,sp
    pusha

    mov ax,[bp+4]
    cmp ax,1
    je Ln
    cmp ax,2
    je Tn
    cmp ax,3
    je Bn
    cmp ax,4
    je Un
    cmp ax,5
    je Hn
    cmp ax,6
    je Vn

    Ln:
        call draw_NextL
        jmp AppearenceDone
    Un:
        call draw_NextU
        jmp AppearenceDone
    Hn:
        call draw_NextH
        jmp AppearenceDone
    Bn:
        call draw_NextB
        jmp AppearenceDone
    Tn:
        call draw_NextT
        jmp AppearenceDone
    Vn:
        call draw_NextV
        jmp AppearenceDone

    AppearenceDone:
        popa
        pop bp
        ret 2

;;;==========================================================;;;
;;;==================SCORE RELATED FUNCTIONS=================;;;
;;;==========================================================;;;

UpdateScore:
    pusha
    add word[score],50
    push word[score]
    call PrintScore
    popa
    ret

PrintScore:
    push bp
    mov bp, sp
    pusha
    push es
    mov ax, 0xb800
    mov es, ax  
    mov ax, [bp+4]   ; load number in ax
    mov bx, 10       ; use base 10 for division
    mov cx, 0        ; initialize count of digits
    NextDigit:
        mov dx, 0        ; zero upper half of dividend
        div bx           ; divide by 10
        add dl, 0x30     ; convert digit into ascii value
        push dx          ; save ascii value on stack
        inc cx           ; increment count of values
        cmp ax, 0        ; is the quotient zero
        jnz NextDigit    ; if no divide it again
        mov di, 1412     ; point di to statring of score box
    nextnum: 
        pop dx           ; remove a digit from the stack
        mov dh, 0x70     ; use which back and black front color
        mov [es:di], dx  ; print char on screen
        add di, 2        ; move to next screen location
        loop nextnum     ; repeat for all digits on stack
    
    pop es
    popa
    pop bp
    ret 2
 
;;;==========================================================;;;
;;;==================TIMER AND INPUTS STUFF==================;;;
;;;==========================================================;;;
kbisr: 
    push ax
    push es
    mov ax, 0xb800
    mov es, ax ; point es to video memory
    
    in al, 0x60 ; read a char from keyboard port
    cmp al, 0x4b ; is the key left arrow
    jne check_right ; no, try next comparison

    mov byte [left],1
    mov byte [right],0
    mov byte [escape],0
    mov byte [down],0
    mov byte [pauseCheck],0
    
 
    jmp nomatch ; leave interrupt routine
    check_right: cmp al, 0x4d ; is the key right arrow
        jne check_down ; no, leave interrupt routine
       
        mov byte [right],1
        mov byte [left],0
        mov byte [escape],0
        mov byte [down],0
        mov byte [pauseCheck],0
        jmp nomatch

    check_down:
        cmp al,0x50
        jne check_esc
        mov byte [down],1
        mov byte [right],0
        mov byte [left],0
        mov byte [escape],0
        mov byte [pauseCheck],0
        jmp nomatch

    check_esc:
        cmp al,0x01
        jne check_pause
        mov byte [escape],1
        mov byte [right],0
        mov byte [left],0
        mov byte [down],0
        mov byte [pauseCheck],0
        jmp GameOver

    check_pause:
        cmp al,0x19
        jne check_if_Lreleased
        mov byte [pauseCheck],1
        mov byte [escape],0
        mov byte [right],0
        mov byte [left],0
        mov byte [down],0
        jmp nomatch

    check_if_Lreleased:
        cmp al,0xcb
        jne check_if_Rreleased
        mov byte [right],0
        mov byte [left],0
        mov byte [down],0
        mov byte [escape],0
        mov byte [pauseCheck],0
        jmp nomatch

    check_if_Rreleased:
        cmp al,0xcd
        jne check_if_Dreleased
        mov byte [right],0
        mov byte [left],0
        mov byte [down],0
        mov byte [escape],0
        mov byte [pauseCheck],0
        jmp nomatch

    check_if_Dreleased:
        cmp al,0xD0
        jne nomatch
        mov byte [right],0
        mov byte [left],0
        mov byte [down],0
        mov byte [escape],0
        mov byte [pauseCheck],0
        jmp nomatch

    GameOver:
    mov byte [gameEnded],'y'
    mov byte [time_check],0

    nomatch: ; mov al, 0x20
        ;out 0x20, al
        pop es
        pop ax
        jmp far [cs:oldisr] ; call the original ISR
        ;  iret

    printnum_timer: 
        push bp
        mov bp, sp
        push es
        push ax
        push bx
        push cx
        push dx
        push di
        mov ax, 0xb800
        mov es, ax ; point es to video base
        mov ax, [bp+4] ; load number in ax
        mov bx, 10 ; use base 10 for division
        mov cx, 0 ; initialize count of digits
    nextdigit: mov dx, 0 ; zero upper half of dividend
        div bx ; divide by 10
        add dl, 0x30 ; convert digit into ascii value
        push dx ; save ascii value on stack
        inc cx ; increment count of values
        cmp ax, 0 ; is the quotient zero
        jnz nextdigit ; if no divide it again
        mov di, [bp+6] ;point di to 70th column
    nextpos: pop dx ; remove a digit from the stack
        mov dh, 0x4F ; use normal attribute
        mov [es:di], dx ; print char on screen
        add di, 2 ; move to next screen location
        loop nextpos ; repeat for all digits on stack
    pop di
    pop dx
    pop cx
    pop bx
    pop ax 
    pop es
    pop bp
    ret 4
    ; timer interrupt service routine
    timer: 
        push ax
        push es
        mov ax,0xb800
        mov es,ax

cmp byte [pauseCheck],1
je end_timer
        inc word [cs:tickcount]; increment tick count
        cmp word [cs:minutes],5
        je GameEnd
        ;  cmp byte [time_check],0
        ; je end_timer
        cmp word [cs:tickcount],18
        jle end_timer
        mov word [cs:tickcount],0
        add word [cs:seconds], 1
        cmp word [cs:seconds],60
        jl printing
        mov word [cs:seconds],0
        inc word [cs:minutes]
        mov word [es:938],0x4F20
     
    printing:
        mov word [es:934],0x4F3A
        push 936
        push word [cs:seconds]
        call printnum_timer
    
    push 932
    push word [cs:minutes]
    call printnum_timer
    jmp end_timer
        
    GameEnd:
        mov byte [gameEnded],'y'

    end_timer:
        ; mov al, 0x20
        ; out 0x20, al ; end of interrupt
        pop es
        pop ax
        jmp far [cs:oldtimer]
       ; iret ; return from interrupt
 
GenerateRandomNumber:
    pusha
    mov ax,[seconds]
    mov bl,6
    div bl
    add ah,1
    mov cx,0
    mov cl,ah
    mov word [random],cx
    popa
    ret

Pause_func:
    pusha


    cmp byte [pauseCheck],1
    jne exit_pause_func

    continue:
    mov ah,0
    int 16h

    cmp al,0x0d
    jne continue

    exit_pause_func:
    mov byte [pauseCheck],0
    popa
    ret

Execute:
    pusha
    push es
    call StartingScreen
    xor ax, ax
    mov es, ax ; point es to IVT base
    mov ax, [es:8*4]
    mov [oldtimer], ax ; save offset of old routine
    mov ax, [es:8*4+2]
    mov [oldtimer+2], ax ; save segment of old routine
    cli ; disable interrupts
    mov word [es:8*4], timer; store offset at n*4
    mov [es:8*4+2], cs ; store segment at n*4+2
    sti ; enable interrupts
    call SetUpGameSpace
    xor ax, ax
    mov es,ax
    mov ax, [es:9*4]
    mov [oldisr], ax ; save offset of old routine
    mov ax, [es:9*4+2]
    mov [oldisr+2], ax ; save segment of old routine
    cli ; disable interrupts
    mov word [es:9*4], kbisr ; store offset at n*4
    mov [es:9*4+2], cs ; store segment at n*4+2
    sti ; enable interrupts
    push word[score]
    call PrintScore
     
    call GenerateRandomNumber
    mov ax,[random]
    GamingLoop:
        call GenerateRandomNumber
        mov dx,[random]
        inc dx
        push dx
        call AppearNextShape
        push ax
        call MoveCurrentShape
        mov ax,dx

        call Pause_func

        cmp byte[gameEnded],'y'
        jne GamingLoop
 
    xor ax,ax  ;unhooking timer
    mov es,ax
    mov ax,[oldtimer]
    mov bx,[oldtimer+2]
    cli ; disable interrupts
    mov word [es:8*4], ax ; store offset at n*4
    mov [es:8*4+2], bx ; store segment at n*4+2
    sti ; enable interrupts
        
    xor ax,ax                     ;;;unhooking input timer
    mov es,ax
    mov ax,[oldisr]
    mov bx,[oldisr+2]
    cli ; disable interrupts
    mov word [es:8*4], ax ; store offset at n*4
    mov [es:8*4+2], bx ; store segment at n*4+2
    sti ; enable interrupts
  
    call GameOverScreen

    pop es
    popa
    ret

start:
    ;call StartingScreen
    call clr
    call Execute
    
    mov ah,0x1
    int 0x21

    mov ax, 0x4c00
    int 0x21