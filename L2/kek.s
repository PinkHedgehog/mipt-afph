
kek.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <Main_main_info-0x70>:
	...
   8:	15 00 00 00 00       	adc    eax,0x0
   d:	00 00                	add    BYTE PTR [rax],al
   f:	00 48 8d             	add    BYTE PTR [rax-0x73],cl
  12:	45                   	rex.RB
  13:	f0 4c 39 f8          	lock cmp rax,r15
  17:	72 3f                	jb     58 <Main_main_info-0x18>
  19:	48 83 ec 08          	sub    rsp,0x8
  1d:	4c 89 e8             	mov    rax,r13
  20:	48 89 de             	mov    rsi,rbx
  23:	48 89 c7             	mov    rdi,rax
  26:	31 c0                	xor    eax,eax
  28:	e8 00 00 00 00       	call   2d <Main_main_info-0x43>
  2d:	48 83 c4 08          	add    rsp,0x8
  31:	48 85 c0             	test   rax,rax
  34:	74 20                	je     56 <Main_main_info-0x1a>
  36:	48 c7 45 f0 00 00 00 	mov    QWORD PTR [rbp-0x10],0x0
  3d:	00 
  3e:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
  42:	41 be 00 00 00 00    	mov    r14d,0x0
  48:	bb 00 00 00 00       	mov    ebx,0x0
  4d:	48 83 c5 f0          	add    rbp,0xfffffffffffffff0
  51:	e9 00 00 00 00       	jmp    56 <Main_main_info-0x1a>
  56:	ff 23                	jmp    QWORD PTR [rbx]
  58:	41 ff 65 f0          	jmp    QWORD PTR [r13-0x10]
  5c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]
	...
  68:	15 00 00 00 00       	adc    eax,0x0
  6d:	00 00                	add    BYTE PTR [rax],al
	...

0000000000000070 <Main_main_info>:
  70:	48 8d 45 f0          	lea    rax,[rbp-0x10]
  74:	4c 39 f8             	cmp    rax,r15
  77:	72 3f                	jb     b8 <Main_main_info+0x48>
  79:	48 83 ec 08          	sub    rsp,0x8
  7d:	4c 89 e8             	mov    rax,r13
  80:	48 89 de             	mov    rsi,rbx
  83:	48 89 c7             	mov    rdi,rax
  86:	31 c0                	xor    eax,eax
  88:	e8 00 00 00 00       	call   8d <Main_main_info+0x1d>
  8d:	48 83 c4 08          	add    rsp,0x8
  91:	48 85 c0             	test   rax,rax
  94:	74 20                	je     b6 <Main_main_info+0x46>
  96:	48 c7 45 f0 00 00 00 	mov    QWORD PTR [rbp-0x10],0x0
  9d:	00 
  9e:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
  a2:	41 be 00 00 00 00    	mov    r14d,0x0
  a8:	bb 00 00 00 00       	mov    ebx,0x0
  ad:	48 83 c5 f0          	add    rbp,0xfffffffffffffff0
  b1:	e9 00 00 00 00       	jmp    b6 <Main_main_info+0x46>
  b6:	ff 23                	jmp    QWORD PTR [rbx]
  b8:	41 ff 65 f0          	jmp    QWORD PTR [r13-0x10]
  bc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]
	...
  c8:	15 00 00 00 00       	adc    eax,0x0
  cd:	00 00                	add    BYTE PTR [rax],al
	...

00000000000000d0 <ZCMain_main_info>:
  d0:	48 8d 45 f0          	lea    rax,[rbp-0x10]
  d4:	4c 39 f8             	cmp    rax,r15
  d7:	72 3f                	jb     118 <ZCMain_main_info+0x48>
  d9:	48 83 ec 08          	sub    rsp,0x8
  dd:	4c 89 e8             	mov    rax,r13
  e0:	48 89 de             	mov    rsi,rbx
  e3:	48 89 c7             	mov    rdi,rax
  e6:	31 c0                	xor    eax,eax
  e8:	e8 00 00 00 00       	call   ed <ZCMain_main_info+0x1d>
  ed:	48 83 c4 08          	add    rsp,0x8
  f1:	48 85 c0             	test   rax,rax
  f4:	74 20                	je     116 <ZCMain_main_info+0x46>
  f6:	48 c7 45 f0 00 00 00 	mov    QWORD PTR [rbp-0x10],0x0
  fd:	00 
  fe:	48 89 45 f8          	mov    QWORD PTR [rbp-0x8],rax
 102:	41 be 00 00 00 00    	mov    r14d,0x0
 108:	bb 00 00 00 00       	mov    ebx,0x0
 10d:	48 83 c5 f0          	add    rbp,0xfffffffffffffff0
 111:	e9 00 00 00 00       	jmp    116 <ZCMain_main_info+0x46>
 116:	ff 23                	jmp    QWORD PTR [rbx]
 118:	41 ff 65 f0          	jmp    QWORD PTR [r13-0x10]
