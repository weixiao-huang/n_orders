!*****************************************************************
!
!   PROGRAM：重排九宫格的推广，n维格子的还原
!
!   PROGRAMMER：黄维啸
!
!   PURPOSE：通过键盘输入数组大小n（n的大小满足1<n<31），
!            程序自动生成大小为n的数组a，并自动判断能否还
!            原到目标情形，若可以，则自动还原，把还原步骤
!            打印在屏幕上，并记录在文件中(文件record_1.txt
!            记录n行n列矩阵a的移动情况；文件record.txt按
!            照a的列来储存移动步骤，最后一位是移动的步数)
!
!   DATE：   Jan. 8th, 2014
!*****************************************************************

    program main
   use moves
    implicit none
    integer,allocatable :: a(:,:)
    integer :: i,  n, num = 0
    integer m1,n1, m2,n2
    integer m0,n0
    logical judge
    integer l(2)
    
!  输入a的大小n，然后随机生成a，并自动判断能否达到目标位置
    print *, 'Please input the number of n (1<n<31) :'
    read *, n
    allocate(a(n,n))
    call create_array(a, n, judge)
    if(.not. judge)stop
 
!-----------------------------------------------------------------!
!  if you want to know more about why it's impossible, you can    !
!  cut the program at line 32 and paste it at line 55.            !
!-----------------------------------------------------------------!

!  逐步还原a的第1行到第n-2行
    do i = 1, n - 2
        call move_row(a, i, num)
    enddo

!  逐步还原a的第n-1、n行的第1列到第n-2列
    do i = 1, n-2
        call move(a, i+n*(n-2), n-1,i, num)             !把正确的数字放在左上角
        if(a(n,i) == 0) call move_0(a, n,i+1, -1, num)  !若左下角是0，移动出来
        if(a(n,i) == i+n*(n-1))cycle                    !若左下角是正确的数字，ok!
        call move(a, i+n*(n-1), n,i+2, num)             !否则移动该数字到右下角
        call move_near(a, n-1,i, n,i+2, num)
    enddo

!  把0移动到右下角
    call move_0(a, n,n, 1, num)

!  分析两种不处于目标位置的情形，将它们还原回目标位置
    if(a(n,n-1) == n*(n-1)-1 .and. a(n-1,n) == n*n-1)then       !左下为n*(n-1)-1，右上为n*n-1的情况
        call move_0(a, n-1,n-1, n*(n-1)-1, num)
        call move_0(a, n,n, n*(n-1), num)
    elseif(a(n,n-1) == n*(n-1) .and. a(n-1,n) == n*(n-1)-1)then !左下为n*(n-1)，右上为n*(n-1)-1的情况
        call move_0(a, n-1,n-1, n*(n-1)-1, num)
        call move_0(a, n,n, n*n-1, num)
    endif
    
    print "('移动步数：',I5)", num
    close(10)
    if (judge) print*, "OK!"
    
    end program
    
    
!=================================================================
!   子程序create_array(c,n,judge)用于随机生成大小为n的数组c，并
!   且判断能否还原到目标状态其中judge为逻辑变量，判断c能否还原，
!   能则返回为T，否则返回为F，而且整个程序结束。
!=================================================================
	subroutine create_array(c, n, judge)
	implicit none
    
    integer         :: m                    ! size(a)
    integer,intent(in):: n                  ! a的大小
	integer		    :: b(2)                 ! 在将a随机打乱的程序中记录a的两个将要对换的数字的值
	integer,intent(out) :: c(n,n)
	integer,allocatable :: a(:)             ! 按行储存c时所获得的1维数组，n为奇数情况下不管怎么移动c，a的逆序数不变
	integer,allocatable :: d(:)             ! 按Z字形储存c时所获得的1维数组，n为偶数情况下不管怎么移动c，d的逆序数不变
	integer			:: i1,j1,rand, l,  i,j
	integer ::  k1 = 0, k2 = 0              ! k1为初始a逆序数，k2为初始d逆序数
	real			   rand_seed, i2,j2
	logical judge

    m = n * n
    allocate(a(m), d(m))
    a(1:m) = [1:m]
    a(m) = 0
	call random_seed()
	call random_number(rand_seed)
	rand = ceiling(rand_seed * m*m)  ! 循环次数（打乱次数）

!  以两两对换的形式将a打乱rand次
	do l = 1, rand
	  call random_number(i2)
	  call random_number(j2)
	  i1 = ceiling(i2 * m)
	  j1 = ceiling(j2 * m)
	  b(1) = a(i1)
	  b(2) = a(j1)
	  a(i1) = b(2)
	  a(j1) = b(1)
    enddo

!  将打乱后的a按行赋值给源数组c
	c = reshape(a, [n,n], order=[2,1])

!  以Z字形将c中数据赋值给d
	do i = 1, n
		if(mod(i,2) == 1)then
			d(1+(i-1)*n : i*n) = c(i, 1:n)
		else
			d(1+(i-1)*n : i*n) = c(i, n:1:-1)
		endif
    enddo

!  求初始时刻a和d的逆序数k1,k2
	do i = 1, m-1
		do j = i+1,m
	      if(a(i) > a(j) .and. a(j) /= 0) k1 = k1+1
	      if(d(i) > d(j) .and. d(j) /= 0) k2 = k2+1
		enddo
    enddo

!  打印初始数组c
    print*, '---The original array---'
    print*
    print "(<n>(<n>I4,/))", ((c(i,j), j=1,n), i=1,n)

!  若n为奇数则逆序数用k1，否则就用逆序数k2
    if(mod(n,2) == 1)then
        if(mod(k1,2) == 0)then  !目标情况中的k1=0，故初始状态k1应为偶数才可解
	      print*, "It's OK!"
	      judge = .true.
	    else                    !若k1为奇数，则不可解，Impossible
	      print*, "Impossible!"
	      judge = .false.
        endif
    else
        if(mod(n/2, 2) == 0)then      !若n/2是偶数，则目标情况中的k2应为奇数才可解
            if(mod(k2,2) == 1)then      !若初始情况k2为奇数，OK!
	            print*, "It's OK!"
	            judge = .true.
	        else                        !若初始情况k2为偶数，Impossible!
                print*, "Impossible!"   
	            judge = .false.
            endif
        else                          !若n/2是奇数，则目标情况中的k2应为偶数才可解
            if(mod(k2,2) == 0)then      !若初始情况k2为偶数，OK!
	            print*, "It's OK!"
	            judge = .true.
	        else                        !若初始情况k2为奇数，Impossible!
                print*, "Impossible!"
	            judge = .false.
            endif
        endif       
    endif
    if (.not. judge)print*,"If you want to know more, see line 34 to 38 in the program main."
    
    end subroutine