!
!	setup.s		(C) 1991 Linus Torvalds
!
! setup.s is responsible for getting the system data from the BIOS,
! and putting them into the appropriate places in system memory.
! both setup.s and system has been loaded by the bootblock.
!
! This code asks the bios for memory/disk/other parameters, and
! puts them in a "safe" place: 0x90000-0x901FF, ie where the
! boot-block used to be. It is then up to the protected mode
! system to read them from there before the area is overwritten
! for buffer-blocks.
!

! NOTE! These had better be the same as in bootsect.s!

INITSEG  = 0x9000	! we move boot here - out of the way
SYSSEG   = 0x1000	! system loaded at 0x10000 (65536).
SETUPSEG = 0x9020	! this is the current segment

.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:
.text

entry start
start:

! ok, the read went well so we get current cursor position and save it for
! posterity.

	mov	ax,#INITSEG	! this is done in bootsect already, but...
	mov	ds,ax
	mov	ah,#0x03	! read cursor pos
	xor	bh,bh
	int	0x10		! save it in known place, con_init fetches
	mov	[0],dx		! it from 0x90000.
! Get memory size (extended mem, kB)

	mov	ah,#0x88
	int	0x15
	mov	[2],ax

! Get video-card data:

	mov	ah,#0x0f
	int	0x10
	mov	[4],bx		! bh = display page
	mov	[6],ax		! al = video mode, ah = window width

! check for EGA/VGA and some config parameters

	mov	ah,#0x12
	mov	bl,#0x10
	int	0x10
	mov	[8],ax
	mov	[10],bx
	mov	[12],cx

| 复制 磁盘16个byte的数据到 0x90080 ~ 0x9008F
! Get hd0 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x41]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0080
	mov	cx,#0x10
	rep
	movsb

| 复制 磁盘16个byte的数据到 0x90090 ~ 0x9009F
! Get hd1 data

	mov	ax,#0x0000
	mov	ds,ax
	lds	si,[4*0x46]
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	rep
	movsb

! Check that there IS a hd1 :-)

	mov	ax,#0x01500
	mov	dl,#0x81
	int	0x13
	jc	no_disk1
	cmp	ah,#3
	je	is_disk1
no_disk1:
	mov	ax,#INITSEG
	mov	es,ax
	mov	di,#0x0090
	mov	cx,#0x10
	mov	ax,#0x00
	rep
	stosb
is_disk1:

! now we want to move to protected mode ...

	cli			! no interrupts allowed !

! first we move the system to it's rightful place
// 如下代码把 head.s 和linux 0.11中的其他编译好的代码从 [段寄存器, 偏移地址]: [0x9000, 0] -> [0x1000, 0], 挪动长度是 0x8000 * 2个字节
	mov	ax,#0x0000
	cld			! 'direction'=0, movs moves forward
do_move:
	mov	es,ax		! destination segment
	add	ax,#0x1000 // ax = ax + 0x1000 = 0x0000 + 0x1000 = 0x1000
	cmp	ax,#0x9000
	jz	end_move
	mov	ds,ax		! source segment
	sub	di,di
	sub	si,si
	mov 	cx,#0x8000
	rep
	movsw
	jmp	do_move

! then we load the segment descriptors
// 加载idt、gdt表的首地址到idtr和gdtr中 (idt、gdt表是嵌入到汇编代码中的，随着之前步骤的bootsect.s/head.s中从磁盘上加载到内存，而被一起加载到内存中)
end_move:
	mov	ax,#SETUPSEG	! right, forgot this at first. didn't work :-)
	mov	ds,ax
	lidt	idt_48		! load idt with 0,0
	lgdt	gdt_48		! load gdt with whatever appropriate

! that was painless, now we enable A20
// 打开 A20 地址线 
// 这是因为开发linux0.11时，CPU已经到了32位时代之后，由于要考虑兼容性，还必须保持一个只能用 20 位地址线的模式，所以如果你不手动开启的话，即使地址线已经有 32 位了，仍然会限制只能使用其中的 20 位

	call	empty_8042
	mov	al,#0xD1		! command write
	out	#0x64,al
	call	empty_8042
	mov	al,#0xDF		! A20 on
	out	#0x60,al
	call	empty_8042

// 下面的代码可以不看。其含义是对 可编程中断控制器 8259 芯片进行的编程。使得8259芯片上的每一个针脚都对应一个intel内置的 或者 Linux中自定义的中断号。然后每一个终端号 又对应一个中断程序。
! well, that went ok, I hope. Now we have to reprogram the interrupts :-(
! we put them right after the intel-reserved hardware interrupts, at
! int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
! messed this up with the original PC, and they haven't been able to
! rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
! which is used for the internal hardware interrupts as well. We just
! have to reprogram the 8259's, and it isn't fun.

	mov	al,#0x11		! initialization sequence
	out	#0x20,al		! send it to 8259A-1
	.word	0x00eb,0x00eb		! jmp $+2, jmp $+2
	out	#0xA0,al		! and to 8259A-2
	.word	0x00eb,0x00eb
	mov	al,#0x20		! start of hardware int's (0x20)
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x28		! start of hardware int's 2 (0x28)
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x04		! 8259-1 is master
	out	#0x21,al
	.word	0x00eb,0x00eb
	mov	al,#0x02		! 8259-2 is slave
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0x01		! 8086 mode for both
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al
	.word	0x00eb,0x00eb
	mov	al,#0xFF		! mask off all interrupts for now
	out	#0x21,al
	.word	0x00eb,0x00eb
	out	#0xA1,al

! well, that certainly wasn't fun :-(. Hopefully it works, and we don't
! need no steenking BIOS anyway (except for the initial loading :-).
! The BIOS-routine wants lots of unnecessary data, and it's less
! "interesting" anyway. This is how REAL programmers do it.
!
! Well, now's the time to actually move into protected mode. To make
! things as simple as possible, we do no register set-up or anything,
! we let the gnu-compiled 32-bit programs do that. We just jump to
! absolute address 0x00000, in 32-bit protected mode.
 // lmsw 指令操作cr0寄存器，加载2字节的word到cr0中，其中第0位是1表示开启protected mode (PE),即开启保护模式，即从实模式 -> 切换到保护模式了。
 // 实模式：								 即real-address mode，表示：cs/ds/ss 寄存器左移4位 + 代码偏移量(ip寄存器)/数据段偏移量(si寄存器)/栈偏移量(bp sp寄存器) = 物理内存地址
 // 保护模式(假定32位机，且开启了分页且2级分页)：  即protected mode, 表示: 
 // 					     				1. cs/ds/ss 寄存器高13位作为段选择子，得到gdt表的index, 从gdt表中取出8字节的段描述符，从8字节段描述符中取出 段基地址
 //							 				2. 段基地址 + 代码偏移量(ip寄存器)/数据段偏移量(si寄存器)/栈偏移量(bp sp寄存器) = 线性地址
 //							 				3. 把32位线性地址拆分成 10-10-12，然后从cr3寄存器中取出当前进程页表，紧接着前两个10 分别从页表中找到一级页表 和 二级页表的 准确index，后12位作为偏移量 从pageTable[index]中的entity中找到具体位置，然后得到真实物理地址
// 总结：实模式 与 保护模式 的核心区别，在于计算物理地址的方式不同。实模式 对用户开放 操作真实物理地址，保护模式 对用户关闭 操作真实物理地址。
// 		其实这里bit 31 of CR0代表开启分页，由于0x0001 第31 bit(0 ~ 31)为0，即关闭分页，所以下面的指令其实 无需考虑分页的计算逻辑了。

	mov	ax,#0x0001	! protected mode (PE) bit
	lmsw	ax		! This is it! 	// 下面的指令都是在intel CPU保护模式下 执行的
	jmpi	0,8		! jmp offset 0 of segment 8 (cs) // 等价于 cs=8 (0000, 0000, 0000, 1000的前13位作为段选择子index即=1, 取出gdt[1]位置的8字节的段描述符), ip=0, 然后jmpi远跳转。----- 至此setup.s代码执行完毕，跳转到物理地址 0的位置执行代码，此时0位置的代码是head.o的首地址

! This routine checks that the keyboard command queue is empty
! No timeout is used - if this hangs there is something wrong with
! the machine, and we probably couldn't proceed anyway.
empty_8042:
	.word	0x00eb,0x00eb
	in	al,#0x64	! 8042 status port
	test	al,#2		! is input buffer full?
	jnz	empty_8042	! yes - loop
	ret

gdt:
	// 第一个段描述符(64位 空)
	.word	0,0,0,0		! dummy 第一个段描述符(64位)为空

	// 第二个段描述符(64位 代码段)有值
	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9A00		! code read/exec
	.word	0x00C0		! granularity=4096, 386

	// 第三个段描述符(64位 数据段)有值
	.word	0x07FF		! 8Mb - limit=2047 (2048*4096=8Mb)
	.word	0x0000		! base address=0
	.word	0x9200		! data read/write
	.word	0x00C0		! granularity=4096, 386

idt_48:
	.word	0			! idt limit=0
	.word	0,0			! idt base=0L

gdt_48:
	.word	0x800		! gdt limit=2048, 256 GDT entries 2048个字节，每个entry是8个字节，则最多2048/8=256个项。16位的cs/ds/ss段选择子
	.word	512+gdt,0x9	! gdt base = 0X9xxxx 执行lgdt gdt_48之前已经设置ds=0x9020, 为什么加上512呢，因为这512是代表head.s二进制代码在内存的大小。因为 head.s 和 setup.s 和 bootsect.s 都是编译到一个二进制文件中的，所以setup.s中的相对offset是包括 head.s 的长度的。
	
.text
endtext:
.data
enddata:
.bss
endbss:
