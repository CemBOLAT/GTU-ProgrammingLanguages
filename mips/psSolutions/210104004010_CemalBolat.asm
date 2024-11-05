.data
	enter_number_msg: .asciiz "Please Enter a number :"
	yes_msg: .asciiz "Yes! The given number is divisible by 9"
	no_msg: .asciiz "No! The given number is NOT divisible by 9"

.text
	.globl main
	
	
main:
	la $a0 enter_number_msg
	jal print_str
	jal read_int
	move $s0 $v0 # read number contains s0
	move $a0 $s0
	jal sum_of_numbers
	move $s1 $v0 # sum contains s1
	move $a0 $s1
	
	jal check_divisible
	
	j exit
	
sum_of_numbers:
	li $t0 0 # result
	move $t1 $a0 # number_to_sum_digits
	li $t2 10 #div
	j sum_process
sum_process:
	beq $t1 $zero finish_sum
	div $t1 $t2 # hi remainder
	mfhi $t3
	mflo $t1
	add $t0 $t0 $t3
	j sum_process
	
finish_sum:
	move $v0 $t0
	jr $ra

read_int:
	li $v0 5
	syscall
	jr $ra
	
check_divisible:
	move $t5 $ra
	move $t1 $a0
	li $t2 9
	div $t1 $t2
	mfhi $t3
	beq $t3 $zero yes_divisible
	la $a0 no_msg
	jal print_str
	jr $t5
yes_divisible:
	la $a0 yes_msg
	jal print_str
	jr $t5
	
print_str:
	li $v0 4
	syscall
	jr $ra
	
exit:
	li $v0 10
	syscall
