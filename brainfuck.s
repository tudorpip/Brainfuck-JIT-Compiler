.global brainfuck

format_str: .asciz "We should be executing the following code:\n%s"
fstring: .asciz "%lld\n"
fstr: .asciz "%c"

.data

array:
    .fill 30000

bytecode:
    .fill 1048576

output_buff:
    .fill 1048576

has_input:
    .quad 0x00

.text

str: .asciz "merge boule\n"
newline: .asciz "\n"
minus_loop: .asciz "[-]"
plus_loop: .asciz "[+]"
cp_l_1: .asciz "[-<+>]"
cp_r_1: .asciz "[->+<]"
cp_l_2: .asciz "[-<<+>>]"
cp_r_2: .asciz "[->>+<<]"
cp_l_3: .asciz "[-<<<+>>>]"
sub_l_8: .asciz "[-<<<<<<<<->>>>>>>>]"
sub_l_7: .asciz "[-<<<<<<<->>>>>>>]"
sub_l_5: .asciz "[-<<<<<->>>>>]"
sub_l_3: .asciz "[-<<<->>>]"
sub_l_2: .asciz "[-<<->>]"
sub_l_1: .asciz "[-<->]"
cp_r_3: .asciz "[->>>+<<<]"
cp_l_4: .asciz "[-<<<<+>>>>]"
sub_l_4: .asciz "[-<<<<->>>>]"
cp_r_4: .asciz "[->>>>+<<<<]"
cp_l_5: .asciz "[-<<<<<+>>>>>]"
cp_l_6: .asciz "[-<<<<<<+>>>>>>]"
cp_r_5: .asciz "[->>>>>+<<<<<]"
cp_r_6: .asciz "[->>>>>>+<<<<<<]"
cp_r_7: .asciz "[->>>>>>>+<<<<<<<]"
cp_l_7: .asciz "[-<<<<<<<+>>>>>>>]"
cp_l_8: .asciz "[-<<<<<<<<+>>>>>>>>]"
cp_r_9: .asciz "[->>>>>>>>>+<<<<<<<<<]"
cp_l_36: .asciz "[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>]"
sub_l_1_cp_r_6: .asciz "[<->-<<<<<<+>>>>>>]"
sub_l_1_cp_l_7: .asciz "[<->-<<<<<<<+>>>>>>>]"
sub_l_1_cp_l_3: .asciz "[<->-<<<+>>>]"
sub_l_1_cp_l_2: .asciz "[<->-<<+>>]"
sub_l_2_cp_l_12: .asciz "[-<<-<<<<<<<<<<+>>>>>>>>>>>>]"
sub_l_1_cp_l_11: .asciz "[-<-<<<<<<<<<<+>>>>>>>>>>>]"
sub_l_1_cp_l_5: .asciz "[-<-<<<<+>>>>>]"
sub_r_3_cp_l_11: .asciz "[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>]"
sub_r_3_cp_l_10: .asciz "[->>>-<<<<<<<<<<<<<+>>>>>>>>>>]"
sub_r_4_cp_l_10: .asciz "[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>]"
sub_r_2_cp_l_11: .asciz "[->>-<<<<<<<<<<<<<+>>>>>>>>>>>]"
sub_r_4_cp_r_1: .asciz "[->+>>>-<<<<]"
cp_r_7_cp_r_5: .asciz "[->>>>>>>+<<+<<<<<]"
cp_r_4_cp_r_1: .asciz "[->>>>+<<<+<]"
cp_r_5_cp_r_4: .asciz "[->>>>>+<++<<<<]"
cp_r_4_cp_r_2: .asciz "[->>+>>+<<<<]"
cp_r_3_cp_r_2: .asciz "[->>+>+<<<]"
cp_r_4_cp_r_3: .asciz "[->>>+>+<<<<]"

# Your brainfuck subroutine will receive one argument:
# a zero termianted string containing the code to execute.
brainfuck:
	# Prologue
    push %rbp
    mov %rsp, %rbp

    # Parse the bf program code
    # Store the current character address in %r12
    mov %rdi, %r12

    # Store the top of the byte code in %r13
    mov $bytecode, %r13

    # Store the sum (++--+) in %r14
    mov $0, %r14

    # Store the displacement (<<>>>) in %r15
    mov $0, %r15

    # Initial check if program contains ','
    call strlen
    mov %r12, %rdi
    mov %rax, %rdx  # length
    mov $',, %rsi   # char to search for
    call memchr
    movq %rax, has_input

    # Stack will contain the addresses of the start of the loops
    loop:
        # Check which operation the current character designates
        cmpb $'+, (%r12)
        je plus
        cmpb $'-, (%r12)
        je minus
        cmpb $'<, (%r12)
        je sh_left
        cmpb $'>, (%r12)
        je sh_right
        cmpb $'[, (%r12)
        je loop_open
        cmpb $'], (%r12)
        je loop_close
        cmpb $',, (%r12)
        je get_input
        cmpb $'., (%r12)
        je output
        jmp end_if

        plus:
            inc %r14

            # Check if next char is not +/-, in this case apply the changes
            # aka write to bytecode
            cmpb $'+, 1(%r12)
            je end_if
            cmpb $'-, 1(%r12)
            je end_if

            jmp add_grouped

        minus:
            dec %r14

            # Check if next char is not +/-, in this case apply the changes
            # aka write to bytecode
            cmpb $'+, 1(%r12)
            je end_if
            cmpb $'-, 1(%r12)
            je end_if

            jmp add_grouped

        sh_left:
            dec %r15

            jmp end_if

        sh_right:
            inc %r15

           jmp end_if

        loop_open:
            # Update pointer
            call update_pointer

            # Check if the current loop is copy loop
            # First check if this is a frequently occuring loop we can optimize
            mov $minus_loop, %rdi
            mov %r12, %rsi
            mov $3, %rdx
            call strncmp
            test %eax, %eax
            jz minus_loop_code

            mov $sub_l_1_cp_r_6, %rdi
            mov %r12, %rsi
            mov $19, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_cp_r_6_code

            mov $sub_l_1_cp_l_7, %rdi
            mov %r12, %rsi
            mov $21, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_cp_l_7_code

            mov $sub_l_1_cp_l_3, %rdi
            mov %r12, %rsi
            mov $13, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_cp_l_3_code

             mov $sub_l_1_cp_l_2, %rdi
            mov %r12, %rsi
            mov $11, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_cp_l_2_code

            mov $sub_l_1_cp_l_11, %rdi
            mov %r12, %rsi
            mov $27, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_cp_l_11_code

            mov $sub_l_1_cp_l_5, %rdi
            mov %r12, %rsi
            mov $15, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_cp_l_5_code

            mov $sub_l_2_cp_l_12, %rdi
            mov %r12, %rsi
            mov $29, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_2_cp_l_12_code

            mov $sub_r_3_cp_l_11, %rdi
            mov %r12, %rsi
            mov $33, %rdx
            call strncmp
            test %eax, %eax
            jz sub_r_3_cp_l_11_code

            mov $sub_r_3_cp_l_10, %rdi
            mov %r12, %rsi
            mov $31, %rdx
            call strncmp
            test %eax, %eax
            jz sub_r_3_cp_l_10_code

            mov $sub_r_4_cp_l_10, %rdi
            mov %r12, %rsi
            mov $33, %rdx
            call strncmp
            test %eax, %eax
            jz sub_r_4_cp_l_10_code

            mov $sub_r_2_cp_l_11, %rdi
            mov %r12, %rsi
            mov $31, %rdx
            call strncmp
            test %eax, %eax
            jz sub_r_2_cp_l_11_code

            mov $sub_r_4_cp_r_1, %rdi
            mov %r12, %rsi
            mov $13, %rdx
            call strncmp
            test %eax, %eax
            jz sub_r_4_cp_r_1_code

            mov $cp_r_7_cp_r_5, %rdi
            mov %r12, %rsi
            mov $19, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_7_cp_r_5_code

            mov $cp_r_4_cp_r_1, %rdi
            mov %r12, %rsi
            mov $13, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_4_cp_r_1_code

            mov $cp_r_5_cp_r_4, %rdi
            mov %r12, %rsi
            mov $16, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_5_cp_r_4_code

            mov $cp_r_4_cp_r_2, %rdi
            mov %r12, %rsi
            mov $13, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_4_cp_r_2_code

             mov $cp_r_3_cp_r_2, %rdi
            mov %r12, %rsi
            mov $11, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_3_cp_r_2_code


            mov $cp_r_4_cp_r_3, %rdi
            mov %r12, %rsi
            mov $13, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_4_cp_r_3_code
            # mov $plus_loop, %rdi
            # mov %r12, %rsi
            #  mov $3, %rdx
            # call strncmp
            # test %eax, %eax
            # jz minus_loop_code

            mov $cp_r_1, %rdi
            mov %r12, %rsi
            mov $6, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_1_code

            mov $cp_l_1, %rdi
            mov %r12, %rsi
            mov $6, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_1_code

            mov $cp_r_2, %rdi
            mov %r12, %rsi
            mov $8, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_2_code

            mov $cp_l_2, %rdi
            mov %r12, %rsi
            mov $8, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_2_code

            mov $cp_r_3, %rdi
            mov %r12, %rsi
            mov $10, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_3_code

            mov $cp_l_3, %rdi
            mov %r12, %rsi
            mov $10, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_3_code

            mov $sub_l_3, %rdi
            mov %r12, %rsi
            mov $10, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_3_code

            mov $sub_l_5, %rdi
            mov %r12, %rsi
            mov $14, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_5_code

            mov $sub_l_7, %rdi
            mov %r12, %rsi
            mov $18, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_7_code

            mov $sub_l_8, %rdi
            mov %r12, %rsi
            mov $20, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_8_code

            mov $sub_l_2, %rdi
            mov %r12, %rsi
            mov $8, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_2_code

            mov $sub_l_1, %rdi
            mov %r12, %rsi
            mov $6, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_1_code

            mov $cp_r_4, %rdi
            mov %r12, %rsi
            mov $12, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_4_code

            mov $cp_l_4, %rdi
            mov %r12, %rsi
            mov $12, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_4_code

            mov $sub_l_4, %rdi
            mov %r12, %rsi
            mov $12, %rdx
            call strncmp
            test %eax, %eax
            jz sub_l_4_code

            mov $cp_r_5, %rdi
            mov %r12, %rsi
            mov $14, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_5_code

            mov $cp_r_6, %rdi
            mov %r12, %rsi
            mov $16, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_6_code
            
            mov $cp_r_7, %rdi
            mov %r12, %rsi
            mov $18, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_7_code

            mov $cp_l_5, %rdi
            mov %r12, %rsi
            mov $14, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_5_code

            mov $cp_l_6, %rdi
            mov %r12, %rsi
            mov $16, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_6_code


            mov $cp_l_7, %rdi
            mov %r12, %rsi
            mov $18, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_7_code

            mov $cp_l_8, %rdi
            mov %r12, %rsi
            mov $20, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_8_code

            mov $cp_r_9, %rdi
            mov %r12, %rsi
            mov $22, %rdx
            call strncmp
            test %eax, %eax
            jz cp_r_9_code

            mov $cp_l_36, %rdi
            mov %r12, %rsi
            mov $76, %rdx
            call strncmp
            test %eax, %eax
            jz cp_l_36_code

            # Check if the current loop is a multiplication loop
            mov %r12, %rdi
            call is_mul_loop

            # Optimize the loop in case it is a multiplication loop
            cmp $0, %rax
            jne optimize_mul_loop

            skip_optimizations:
            # Encode cmp instr (3 bytes)
            # cmpb $0, (%rax) (rax is the current byte pointer)
            movb $0x80, (%r13)
            movb $0x38, 1(%r13)
            movb $0x00, 2(%r13)
            add $3, %r13

            # Encode jne instr. (2 bytes for the opcode)
            # je + 4 bytes for offset
            movb $0x0f, (%r13)
            movb $0x84, 1(%r13)
            add $2, %r13

            # Reserve 4 bytes for the rel jmp offset
            add $4, %r13

            # Push the current top of the bytecode (address to return to after loop)
            push %r13

            jmp end_if     # de completat
        
        loop_close:
            # Update pointer
            call update_pointer

            # Pop the opening bracket (holding the first instruction of the loop address)
            pop %rbx
            mov %r13, %rax
            sub %rbx, %rax
            add $9, %rax

            # Encode the rel. jmp instr.
            movl %eax, -4(%rbx)   # rel. addr = [ - ] - 3(cmp) - 6(jne + offset) 

            # Encode the end of while check
            # Encode cmp instr (3 bytes)
            # cmpb $0, (%rax) (rax is the current byte pointer)
            movb $0x80, (%r13)
            movb $0x38, 1(%r13)
            movb $0x00, 2(%r13)
            add $3, %r13

            # Encode the end of while check
            # Encode je instr. (2 bytes)
            # je +5 bytes
            movb $0x0f, (%r13)
            movb $0x85, 1(%r13)
            add $2, %r13
            neg %eax
            movl %eax, (%r13)   # rel. addr = [ - ] - 3(cmp) - 2(jne) - 5(jmp) + 3(cmp) + 2(je) + 5(jmp)
            add $4, %r13

            jmp end_if

        update_pointer:
            # prologue
            push %rbp
            mov %rsp, %rbp

            cmp $0, %r15
            je skip_update_pointer

            # add $immediate=%r15d, %rax
            movb $0x48, (%r13)
            movb $0x05, 1(%r13)
            movl %r15d, 2(%r13)

            add $6, %r13

            mov $0, %r15 # reset displacement to 0

            skip_update_pointer:
            mov %rbp, %rsp
            pop %rbp
            ret

        add_grouped:
            cmpb $0, %r14b
            je skip_add_grouped

            # addb $r14b, $immediate=%r15(%rax)
            movb $0x80, (%r13)
            movb $0x80, 1(%r13)
            movl %r15d, 2(%r13)
            movb %r14b, 6(%r13)
            add $7, %r13

            mov $0, %r14    # restore r14 to zero (amount to be added/sub)

            skip_add_grouped:
            jmp end_if  

        output:
            # Update pointer
            call update_pointer

            movq $has_input, %rax
            cmpq $0, (%rax)
            je post_output

            synced_output:
                movb $0x50, (%r13)  # push %rax
                inc %r13

                movb $0x48, (%r13)
                movb $0xc7, 1(%r13)
                movb $0xc2, 2(%r13)
                movb $0x01, 3(%r13)
                movb $0x00, 4(%r13)
                movb $0x00, 5(%r13)
                movb $0x00, 6(%r13)

                movb $0x48, 7(%r13)
                movb $0x89, 8(%r13)
                movb $0xc6, 9(%r13)

                movb $0x48, 10(%r13)
                movb $0xc7, 11(%r13)
                movb $0xc0, 12(%r13)
                movb $0x01, 13(%r13)
                movb $0x00, 14(%r13)
                movb $0x00, 15(%r13)
                movb $0x00, 16(%r13)

                movb $0x48, 17(%r13)
                movb $0xc7, 18(%r13)
                movb $0xc7, 19(%r13)
                movb $0x01, 20(%r13)
                movb $0x00, 21(%r13)
                movb $0x00, 22(%r13)
                movb $0x00, 23(%r13)

                movb $0x0f, 24(%r13)
                movb $0x05, 25(%r13)

                movb $0x58, 26(%r13)

                add $27, %r13
    
                jmp end_if

            post_output:
                movb $0x44, (%r13)
                movb $0x8a, 1(%r13)
                movb $0x28, 2(%r13)
                movb $0x45, 3(%r13)
                movb $0x88, 4(%r13)
                movb $0x2c, 5(%r13)
                movb $0x24, 6(%r13)

                movb $0x49, 7(%r13)
                movb $0xff, 8(%r13)
                movb $0xc4, 9(%r13)

                add $10, %r13

                jmp end_if

        get_input:
            # Update pointer
            call update_pointer

            movb $0xc6, (%r13)
            movb $0x00, 1(%r13)
            movb $0x00, 2(%r13)
            add $3, %r13

            movb $0x50, (%r13)
            inc %r13
            movb $0x50, (%r13)
            inc %r13
            movb $0x48, (%r13)
            inc %r13
            movb $0xc7, (%r13)
            inc %r13
            movb $0xc7, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x48, (%r13)
            inc %r13
            movb $0x89, (%r13)
            inc %r13
            movb $0xc6, (%r13)
            inc %r13
            movb $0x48, (%r13)
            inc %r13
            movb $0xc7, (%r13)
            inc %r13
            movb $0xc2, (%r13)
            inc %r13
            movb $0x01, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x48, (%r13)
            inc %r13
            movb $0xc7, (%r13)
            inc %r13
            movb $0xc0, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x00, (%r13)
            inc %r13
            movb $0x0f, (%r13)
            inc %r13
            movb $0x05, (%r13)
            inc %r13
            movb $0x58, (%r13)
            inc %r13
            movb $0x58, (%r13)
            inc %r13

            jmp end_if

        end_if:

        # Next character
        inc %r12

        # Stop if null is found
        cmpb $0, (%r12)
        jne loop

    # Make sure we return from bytecode
    movb $0xc3, (%r13)
    inc %r13

    # Call the compiled bytecode
    # %rax is the pointer to the current byte in the array
    call wx_bytecode

    # Check if we have to post-output
    movq $has_input, %rax
    cmpq $0, (%rax)
    jne end_program 

    mov $output_buff, %rdi
    call strlen
    mov %rax, %rdx
    mov $1, %rdi
    mov $1, %rax
    mov $output_buff, %rsi
    syscall

    # Epilogue
    end_program:
        mov %rbp, %rsp
        pop %rbp
        mov $0, %rdi
        call exit

wx_bytecode:
    # Prologue
    push %rbp
    mov %rsp, %rbp

    # allocate RW memory
	movq $0, %r9 # offset
	movq $-1, %r8 # fd
	movq $34, %rcx # flags (MAP_PRIVATE | MAP_ANONYMOUS)
	movq $3, %rdx # prot (PROT_READ | PROT_WRITE)
	movq $1048576, %rsi # length
	movq $0, %rdi # addr
	call mmap

    pushq %rax # store program memory on the stack

    # copy instructions to new memory region
	movq $1048576, %rdx # size
	leaq (bytecode), %rsi # source
	movq (%rsp), %rdi # destination
	call memcpy

    # make memory executable
	movq $5, %rdx # prot (PROT_READ | PROT_EXEC)
	movq $1048576, %rsi # size
	movq (%rsp), %rdi # address
	call mprotect

    # %rax will hold the current array element pointer
    mov $array, %rax

    # %r12 will hold the output buffer pointer
    mov $output_buff, %r12

    # execute the code at the new memory 
	call * (%rsp)

    # clean the allocated memory
	movq $1048576, %rsi # length
	movq (%rsp), %rdi # address
	call munmap

    # Epilogue
    mov %rbp, %rsp
    pop %rbp
    ret

minus_loop_code:
    # Jump over [-] in the program
    add $2, %r12

    # Encode the simplified loop bytecode
    movb $0xc6, (%r13) 
    movb $0x00, 1(%r13)
    movb $0x00, 2(%r13)
    add $3, %r13

    jmp end_if

cp_r_1_code:
    # Jump over [->+<] in the program
    add $5, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x01, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_1_code:
    # Jump over [-<+>] in the program
    add $5, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xff, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_r_2_code:
    # Jump over [->>+<<] in the program
    add $7, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x02, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_2_code:
    # Jump over [-<<+>>] in the program
    add $7, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfe, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_r_3_code:
    # Jump over [->>>+<<<] in the program
    add $9, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x03, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_3_code:
    # Jump over [-<<<+>>>] in the program
    add $9, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfd, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if


sub_l_3_code:
    # Jump over [-<<<->>>] in the program
    add $9, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfd, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

sub_l_8_code:
    # Jump over [-<<<->>>] in the program
    add $19, %r12
    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf8, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

sub_l_7_code:
    # Jump over [-<<<->>>] in the program
    add $17, %r12
    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf9, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if
sub_l_5_code:
    # Jump over [-<<<->>>] in the program
    add $13, %r12
    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfb, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if
sub_l_2_code:
    # Jump over [-<<->>] in the program
    add $7, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfe, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

sub_l_1_code:
    # Jump over [-<<->>] in the program
    add $5, %r12
    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xff, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if
cp_r_4_code:
    # Jump over [->>>>+<<<<] in the program
    add $11, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x04, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_4_code:
    # Jump over [-<<<<+>>>>] in the program
    add $11, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfc, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

sub_l_4_code:
    # Jump over [-<<<<->>>>] in the program
    add $11, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x28, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfc, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_r_5_code:
    # Jump over [->>>>+<<<<] in the program
    add $13, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x05, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_r_6_code:
    # Jump over [->>>>+<<<<] in the program
    add $15, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x06, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if
cp_r_7_code:

    # Jump over [->>>>+<<<<] in the program
    add $17, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x07, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_5_code:
    # Jump over [-<<<<+>>>>] in the program
    add $13, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfb, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_6_code:
    # Jump over [-<<<<+>>>>] in the program
    add $15, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfa, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_7_code:
    # Jump over [-<<<<+>>>>] in the program
    add $17, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf9, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_8_code:
    # Jump over [-<<<<+>>>>] in the program
    add $19, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf8, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_r_9_code:
    # Jump over [->>>>+<<<<] in the program
    add $21, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x09, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

cp_l_36_code:
    # Jump over [-<<<<+>>>>] in the program
    add $75, %r12

    # Encode the simplified loop bytecode
    movb $0x8a, (%r13) 
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xdc, 4(%r13)
    movb $0xc6, 5(%r13) 
    movb $0x00, 6(%r13)
    movb $0x00, 7(%r13)
    add $8, %r13

    jmp end_if

sub_l_1_cp_r_6_code:
    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $18, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfa, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xff, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_l_1_cp_l_7_code:
    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $20, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf9, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xff, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_l_1_cp_l_3_code:
    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $12, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfd, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xff, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_l_1_cp_l_2_code:

    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $10, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfe, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xff, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_l_2_cp_l_12_code:

    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $28, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf4, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xfe, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_l_1_cp_l_11_code:

    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $26, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf5, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xff, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_l_1_cp_l_5_code:

    # Jump over [<->-<<<<<<+>>>>>>] in the program
    add $14, %r12

    # Encode the simplified loop bytecode (cp_l_6)
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xfb, 4(%r13)

    # Encode the simplified loop bytecode (sub_r)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0xff, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if
sub_r_3_cp_l_11_code:
    # Jump over [->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>] in the program
    add $32, %r12

    # Encode the simplified loop bytecode ()
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf5, 4(%r13)


    # Encode the simplified loop bytecode (sub_l)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x03, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if
sub_r_3_cp_l_10_code:
    # Jump over [->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>] in the program
    add $30, %r12

    # Encode the simplified loop bytecode ()
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf6, 4(%r13)


    # Encode the simplified loop bytecode (sub_l)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x03, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_r_4_cp_l_10_code:
    # Jump over [->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>] in the program
    add $32, %r12

    # Encode the simplified loop bytecode ()
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf6, 4(%r13)


    # Encode the simplified loop bytecode (sub_l)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x04, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_r_2_cp_l_11_code:
    # Jump over [->>-<<<<<<<<<<<<<+>>>>>>>>>>>] in the program
    add $30, %r12

    # Encode the simplified loop bytecode ()
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0xf5, 4(%r13)


    # Encode the simplified loop bytecode (sub_l)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x02, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

sub_r_4_cp_r_1_code:

# Jump over [->>-<<<<<<<<<<<<<+>>>>>>>>>>>] in the program
    add $12, %r12

    # Encode the simplified loop bytecode ()
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x01, 4(%r13)


    # Encode the simplified loop bytecode (sub_l)
    movb $0x28, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x04, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if
    
cp_r_7_cp_r_5_code:
    add $18, %r12

    # Encode the simplified loop bytecode () add after 5 poz
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x05, 4(%r13)


    # Encode the simplified loop bytecode (sub_l) add after 7 poz
    movb $0x00, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x07, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

cp_r_4_cp_r_1_code:
    add $12, %r12
    # Encode the simplified loop bytecode () add after 5 poz
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x01, 4(%r13)


    # Encode the simplified loop bytecode (sub_l) add after 7 poz
    movb $0x00, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x04, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

cp_r_5_cp_r_4_code:
    add $15, %r12
    # Encode the simplified loop bytecode () add after 5 poz
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x04, 4(%r13)
    movb $0x00, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x04, 7(%r13)


    # Encode the simplified loop bytecode (sub_l) add after 7 poz
    movb $0x00, 8(%r13) 
    movb $0x48, 9(%r13)
    movb $0x05, 10(%r13)
    movb $0xc6, 11(%r13) 
    movb $0x00, 12(%r13)
    movb $0x00, 13(%r13)
    add $14, %r13

    jmp end_if

cp_r_4_cp_r_2_code:

    add $12, %r12
    # Encode the simplified loop bytecode () add after 5 poz
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x02, 4(%r13)


    # Encode the simplified loop bytecode (sub_l) add after 7 poz
    movb $0x00, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x04, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

cp_r_3_cp_r_2_code:

    add $10, %r12
    # Encode the simplified loop bytecode () add after 5 poz
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x02, 4(%r13)


    # Encode the simplified loop bytecode (sub_l) add after 7 poz
    movb $0x00, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x03, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if


cp_r_4_cp_r_3_code:
    add $12, %r12
    # Encode the simplified loop bytecode () add after 5 poz
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0x00, 2(%r13) 
    movb $0x48, 3(%r13)
    movb $0x03, 4(%r13)


    # Encode the simplified loop bytecode (sub_l) add after 7 poz
    movb $0x00, 5(%r13) 
    movb $0x48, 6(%r13)
    movb $0x04, 7(%r13)
    movb $0xc6, 8(%r13) 
    movb $0x00, 9(%r13)
    movb $0x00, 10(%r13)
    add $11, %r13

    jmp end_if

is_mul_loop:
    # returns 1 if it is a multiplication loop with [+..]
    # returns -1 if it is a multiplication loop with [-..]
    # return 0 otherwise (so if it's NOT a multiplication loop)
    # string to check is in %rdi

    # epilogue
    push %rbp
    mov %rsp, %rbp

    # preserve callee-saved registers
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
    push %r15

    mov $0, %r12    # displacement
    mov $0, %r13    # (src_add) how much is added to starting pointer

    while_loop_end_not_reached:
        inc %rdi    # next character

        movb (%rdi), %cl

        cmpb $'], %cl
        je end_loop

        cmpb $'[, %cl
        je ret_0
        cmpb $',, %cl
        je ret_0
        cmpb $'., %cl
        je ret_0

        cmpb $'>, %cl
        je inc_displ
        cmpb $'<, %cl
        je dec_displ

        # increase r13 (src_add) if displ = 0 and char is +
        cmpb $'+, %cl
        jne skip_plus
        cmp $0, %r12
        jne skip_plus
        inc %r13
        skip_plus:

        # decrease r13 (src_add) if displ = 0 and char is -
        cmpb $'-, %cl
        jne skip_minus
        cmp $0, %r12
        jne skip_minus
        dec %r13
        skip_minus:
        
        jmp while_loop_end_not_reached

        inc_displ:
            inc %r12
            jmp while_loop_end_not_reached

        dec_displ:
            dec %r12
            jmp while_loop_end_not_reached

    end_loop:

    cmp $0, %r12
    jne ret_0

    cmp $1, %r13
    je ret_1

    cmp $-1, %r13
    je ret_minus_1

    ret_0:
        mov $0, %rax
        jmp end_func
    ret_1:
        mov $1, %rax
        jmp end_func
    ret_minus_1:
        mov $-1, %rax
        jmp end_func

    end_func:
    # restore callee-saved registers
    pop %r15
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx

    # epilogue
    mov %rbp, %rsp
    pop %rbp
    ret

optimize_mul_loop:
    # %r12 will be the current char addr
    # %r13 will hold the top of the bytecode

    # preserve callee-saved registers
    push %r14
    push %r15

    mov $0, %r14    # displacement
    mov $0, %r15    # add

    while_loop_end_not_reached_2:
        inc %r12

        # avoid "too many memory references" err
        movb (%r12), %cl

        cmpb $'+, %cl
        jne skip_add
        inc %r15
        jmp end_while_check
        skip_add:

        cmpb $'-, %cl
        jne skip_sub
        dec %r15
        jmp end_while_check
        skip_sub:

        else:
        cmpb $0, %r15b   # if add != 0
        je skip_bytecode
        cmpb $0, %r14b   # if displ != 0
        je skip_bytecode
        cmp $1, %rax
        je plus_bytecode

	    minus_bytecode:
            cmpb $0, %r15b
            jl create_bytecode_mul_minus_neg_add
            jmp create_bytecode_mul_minus_pos_add  # create bytecode
            
        plus_bytecode:
            cmpb $0, %r15b
            jl create_bytecode_mul_plus_neg_add
            jmp create_bytecode_mul_plus_pos_add  # create bytecode
        skip_bytecode:

        cmpb $'>, %cl
        jne skip_inc_displ
        inc %r14
        mov $0, %r15    # add = 0
        skip_inc_displ:

        cmpb $'<, %cl
        jne skip_dec_displ
        dec %r14
        mov $0, %r15    # add = 0
        skip_dec_displ:

        end_while_check:
        cmpb $'], %cl
        jne while_loop_end_not_reached_2

    # make (rax) = 0 (so zero the pointer at the start of the loop)
    movb $0xc6, (%r13)
    movb $0x00, 1(%r13)
    movb $0x00, 2(%r13)
    add $3, %r13

    # restore callee-saved registers
    pop %r15
    pop %r14

    jmp end_if

create_bytecode_mul_minus_pos_add:
    cmpb $5, %r15b
    jge create_bytecode_mul_minus_vechi

    # movb (%rax), %cl      
    # addb %cl, add=r15(%rax) 
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)

    add $2, %r13

    while_add_not_0:
        movb $0x00, (%r13)
        movb $0x88, 1(%r13)
        movl %r14d, 2(%r13)

        add $6, %r13

        dec %r15
        cmp $0, %r15
        jne while_add_not_0

    jmp skip_bytecode


create_bytecode_mul_minus_neg_add:
    cmpb $-5, %r15b
    jle create_bytecode_mul_minus_vechi

    # movb (%rax), %cl      
    # addb %cl, add=r15(%rax) 
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)

    add $2, %r13

    while_add_not_0_neg:
        movb $0x28, (%r13)
        movb $0x88, 1(%r13)
        movl %r14d, 2(%r13)

        add $6, %r13

        inc %r15
        cmp $0, %r15
        jne while_add_not_0_neg

    jmp skip_bytecode

create_bytecode_mul_plus_pos_add:
    cmpb $5, %r15b
    jge create_bytecode_mul_plus_vechi

    # movb (%rax), %cl      
    # negb %cl
    # addb %cl, add=r15(%rax) 
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0xf6, 2(%r13)
    movb $0xd9, 3(%r13)

    add $4, %r13

    while_add_not_0_3:
        movb $0x00, (%r13)
        movb $0x88, 1(%r13)
        movl %r14d, 2(%r13)

        add $6, %r13

        dec %r15
        cmp $0, %r15
        jne while_add_not_0_3

    jmp skip_bytecode


create_bytecode_mul_plus_neg_add:
    cmpb $-5, %r15b
    jle create_bytecode_mul_plus_vechi

    # movb (%rax), %cl      
    # negb %cl
    # addb %cl, add=r15(%rax) 
    movb $0x8a, (%r13)
    movb $0x08, 1(%r13)
    movb $0xf6, 2(%r13)
    movb $0xd9, 3(%r13)

    add $4, %r13

    while_add_not_0_4:
        movb $0x28, (%r13)
        movb $0x88, 1(%r13)
        movl %r14d, 2(%r13)

        add $6, %r13

        inc %r15
        cmp $0, %r15
        jne while_add_not_0_4

    jmp skip_bytecode




    ################## VECHI

create_bytecode_mul_minus_vechi:
    # top of bytecode = %r13
    # displ = %r14
    # add = %r15
    # 49 89 c7 
    # 4c 89 f8 
    movb $0x49, (%r13)
    movb $0x89, 1(%r13)
    movb $0xc7, 2(%r13)
    movb $0x8a, 3(%r13)
    movb $0x00, 4(%r13)
    movb $0xb2, 5(%r13)
    movb %r15b, 6(%r13)   # add
    movb $0xf6, 7(%r13)
    movb $0xe2, 8(%r13)
    movb $0x88, 9(%r13)
    movb $0xc1, 10(%r13)
    movb $0x4c, 11(%r13)
    movb $0x89, 12(%r13)
    movb $0xf8, 13(%r13)
    movb $0x00, 14(%r13)
    movb $0x88, 15(%r13)
    movl %r14d, 16(%r13) # displ
    add $20, %r13        # update the top of bytecode
    mov $0, %r15         # add = 0
    jmp skip_bytecode

create_bytecode_mul_plus_vechi:
    # top of bytecode = %r13
    # displ = %r14
    # add = %r15
    # 49 89 c7 
    # 4c 89 f8 
    movb $0x49, (%r13)
    movb $0x89, 1(%r13)
    movb $0xc7, 2(%r13)
    movb $0x8a, 3(%r13)
    movb $0x00, 4(%r13)
    movb $0xf6, 5(%r13)
    movb $0xd8, 6(%r13)
    movb $0xb2, 7(%r13)
    movb %r15b, 8(%r13)   # add
    movb $0xf6, 9(%r13)
    movb $0xe2, 10(%r13)
    movb $0x88, 11(%r13)
    movb $0xc1, 12(%r13)
    movb $0x4c, 13(%r13)
    movb $0x89, 14(%r13)
    movb $0xf8, 15(%r13)
    movb $0x00, 16(%r13)
    movb $0x88, 17(%r13)
    movl %r14d, 18(%r13)  # displ
    add $22, %r13        # update the top of bytecode
    mov $0, %r15         # add = 0
    jmp skip_bytecode