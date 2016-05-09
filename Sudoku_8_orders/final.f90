!**********************************************************************
!
!   PROGRAM：重排九宫格
!
!   PROGRAMMER：黄维啸
!
!   PURPOSE：程序随机生成大小为3的数组a，并自动判断能否还
!            原到目标情形，若可以，则自动还原，把还原步骤
!            打印在屏幕上，并记录在文件record.txt中
!
!   DATE    ：Jan. 3ed, 2014
!   MODIFIED：Jan. 10th, 2014  
!**********************************************************************   
    
    program main
	implicit none
	integer :: a(3,3)
	integer i, j                        ! 循环变量
    integer :: num = 0                  ! 计步器
    logical judge                       ! 逻辑变量，判断a是否能达到目标位置，能则返回T
    integer :: a1(10) = 1, a0(10) = 0   ! 用于写入文档的第二行，方便MATLAB读取并判断是否能够达到目标位置
    real    :: t1 = 0, t2 = 0           ! 用于计算程序运行时间
    
    call create_array(a, judge)     ! 随机生成数组a，并返回逻辑变量judge

!  将a数组按列记录在文件record.txt中，最后一位是计步器num
    open(unit = 10, file='record.txt', action='write')
    write(10, "(10I4)") a, num

!  若judge为T，则在record.txt第二行写入10个0，否则在record.txt第二行写入10个1，程序停止
    if(.not. judge) then
        write(10, "(10I4)") a1
        stop
    else
        write(10, "(10I4)") a0
    endif

!  打印a在屏幕上
	print*
    print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)

!  以下是具体移动步骤
    call CPU_TIME(t1)           ! 在此处记录此时的cpu时间
    call move(a, 1, 1, 1, num)  ! 将0移动到（1,1）处
    call move_2(a, num)         ! 将2移动到（1,2）处
    
!  将3移动到目标位置
    if (a(2,3)==3 .and. a(1,3)==0)then          ! 如果0在右上角，3恰好在0下面
        call move_0(a, 2,3, 2, num)                ! 直接0和3交换即可
    elseif(a(1,3) /= 3)then                     ! 若3不在目标位置（3,3）
        if(a(1,3) == 0) call move_0(a, 2, 3, 2,num) ! 若0在右上角，则往下移动一格，防止之后0的移动影响已经还原的1和2
        call move(a, 3, 3, 3, num)                     ! 调用子程序将数字3移动到（3,3）处
        call move_0(a, 1,3, 2, num)                    ! 将数字0移动到（1,3）处，以满足子程序move_near的使用条件
        call move_near(a, 1,2, 3,3, num)               ! 调用子程序move_near将3移回目标位置（1,3）
    endif
    
!  将4和7移动到目标位置，并放在一起
    call move(a, 4, 2, 1, num)          ! 将4移动到目标位置（2,1）
    if(a(3,2)==7 .and. a(3,1)==0)then   ! 如果0在左下角，7刚好在0右边
        call move_0(a, 3,2, 4, num)        ! 直接交换0和7即可
    elseif(a(3,1) /= 7)then             ! 如果7不在目标位置（3,1）
        call move(a, 7, 3, 3, num)         ! 把7移动到（3,3）处
        call move_0(a, 3,1, 4, num)        ! 把0移动到（3,1）处，以满足子程序move_near的使用条件
        call move_near(a, 2,1, 3,3, num)   ! 调用子程序move_near将7移回目标位置（3,1）
    endif

!  最后一步，移动5,6,8回归
    call move_0(a, 3,3, 1, num)         ! 将0移动到右下角（3,3）处，则只有两种情况未达到目标位置
    if(a(3,2) == 5)then                 ! 若a(3,2)=5，a(2,3)=8
        call move_0(a, 2,2, 5, num)
        call move_0(a, 3,3, 6, num)
    elseif(a(3,2) == 6)then             ! 若a(3,2)=6，a(2,3)=8
        call move_0(a, 2,2, 5, num)
        call move_0(a, 3,3, 8, num)
    endif
    
    call CPU_TIME(t2)                   ! 查看此时的cpu时间
!  打印最终所需的步数及运算占用的cpu时间
    print "('所需步数：',I3)", num
    print "('程序运行时间：',F9.6)",t2-t1
    
!  将最终结果写入文件record.txt中
    open(unit=10, file='record.txt', action='write')
    write(10, "(F9.6)")t2-t1
    end program
    
    
!======================================================================
!   子程序find_loc(a,x,m,n)用于寻找数字x在数组a中的位置(m,n)
!======================================================================
    subroutine find_loc(a, x ,m, n)
    implicit none
    integer a(3,3), x
    integer,intent(out) :: m,n
    integer i,j
    do i = 1,3
       do j = 1,3
          if(a(i,j) == x)then  ! 若a(i,j) == x,则记录下此时的i,j值赋给m,n
            m = i
            n = j
            return
          endif
       enddo
    enddo
    end subroutine
    
!======================================================================
!   子程序move_near(a,m1,n1,m2,n2,num)用于将a(m2,n2)上的数字放到
!   a(m1,n1)的相邻右侧或相邻下侧。使用条件如下：a(m1,n1+1)==0且
!   (m2-m1==2.and.n2-n1==1) .or. (m2-m1==1.and.n2-n1==2)
!=======================================================================
    subroutine move_near(a, m1,n1, m2,n2, num)
    implicit none
    integer a(3,3)
    integer m1, n1, m2, n2  ! m1,n1记录左上角那个数的位置，m2,n2记录右下角那个数的位置
    integer temp            ! 最后一步中作为替换变量
    integer num             ! 计步器
    if (m2/=3 .or. n2/=3)then   ! 若不满足使用条件，则退出，显示‘Input error！’
        print*, 'Input error!'
        stop
    endif
    if (m1==1 .and. n1==2)then      ! 若左上角那个数处在（1,2）位置
        call move_0(a, 1,2, 3, num)     ! 将0移动到（1,2）处，避开数字3
        call move_0(a, 2,3, 2, num)     ! 将0移动到（2,3）处，避开数字2
        call move_0(a, 3,3, 2, num)     ! 将0移动到（3,3）处，避开数字2
        call move_0(a, 2,2, 3, num)     ! 将0移动到（2,2）处，避开数字3
        call move_0(a, 1,2, 3, num)     ! 将0移动到（1,2）处，避开数字3
        temp = a(2,2)                   ! temp记录a(2,2)处的数字
        call move_0(a, 2,3, temp, num)  ! 将0移动到（2,3）处，避开数字temp
    elseif(m1==2 .and. n1==1)then   ! 若左上角那个数处在（2,1）位置
        call move_0(a, 2,1, 7, num)     ! 将0移动到（2,1）处，避开数字7
        call move_0(a, 3,2, 4, num)     ! 将0移动到（3,2）处，避开数字4
        call move_0(a, 3,3, 4, num)     ! 将0移动到（3,3）处，避开数字4
        call move_0(a, 2,2, 7, num)     ! 将0移动到（2,2）处，避开数字7
        call move_0(a, 2,1, 7, num)     ! 将0移动到（2,1）处，避开数字7
        temp = a(2,2)                   ! temp记录a(2,2)处的数字
        call move_0(a, 3,2, temp, num)  ! 将0移动到（2,3）处，避开数字temp
    else
        print*, 'Input error1 !'
    endif
    end subroutine
    
!=======================================================================
!   子程序move_2(a,num)用于将数字2移动到目标位置（1,2）
!=======================================================================
    subroutine move_2(a, num)
    implicit none
    integer a(3,3)
    integer m, n, m20, n20      ! m,n为2所在的位置，m20,n20为2下一步要移到的位置
    integer x,y,  l(2)          ! l记录0所在的行列位置
    integer i,j                 ! 循环变量
    integer num                 ! 计步器
    
    call find_loc(a, 2, m,n)
    x = m - 1           ! 2现在的位置与目标位置的行相对偏差
    y = n - 2           ! 2想在的位置与目标位置的列相对偏差
    do while(x/=0 .or. y/=0)    ! 若2不在目标位置就进行循环
      l = minloc(a)             ! 寻找0所在的位置
       if((m==3 .and. n==1) .and. (l(1)==1 .and. l(2)==2))call move_0(a, 2,1, 1, num)    ! 若2在(3,1)处而0在(1,2)处，应先把0移到第二排(2,1)处
       if(m==2 .and. n==2)then  ! 特殊情况：若2处在（2,2）处
         m20 = 1
         n20 = 2
         l = minloc(a)
          if((l(1)==2 .and. l(2)==1).or.(l(1)==3 .and. l(2)==1)) then  ! 若0在（2,1）或者（3,1）处
            call move_0(a,2,3,2, num)                                  ! 先把0移动到（3,2）这个位置，防止0移动过程中打乱0所在的位置
          endif
       else                     ! 若2不在（2,2）处，就用子程序move的方式来移动2到目标位置
          if(abs(x) > abs(y))then
            m20 = m - x/abs(x)
            n20 = n
          elseif(abs(x) < abs(y))then
            m20 = m
            n20 = n - y/abs(y)
          else
            m20 = m
            n20 = n - y/abs(y)
          endif
       endif
      call move_0(a, m20, n20, 2, num)  ! 将0移动到2将要移动到的位置（m20,n20），以便0和2对换使得2移到目标位置
      a(m20, n20) = a(m, n)
      a(m ,n) = 0
      m = m20           ! 重新记录2的位置（m,n）
      n = n20
      x = m - 1         ! 重新记录x,y的值
      y = n - 2

!   将a打印在屏幕上
      print*
      print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)
      num = num + 1

!   将数组a以及移动步数记录在文件record.txt中
      open(unit = 10, file='record.txt', action='write')
      write(10, "(10I4)") a, num
      
    enddo
    end subroutine
    
!=======================================================================
!   子程序move(a,z,m2,n2,num)用于将数字z移动到(m2,n2)处，注意：此程序
!   可能会打乱已排好的格子，使用需慎重！
!=======================================================================
	subroutine move(a, z, m2,n2, num)
    implicit none
	integer a(3,3)
	integer m, n, m2, n2, m10, n10  ! m10, n10为1的下一目标位置
	integer x, y
	integer z
    integer num                     ! 计步器
    integer i,j                     ! 循环变量

    call find_loc(a, z, m,n)    ! 寻找数字z所在的位置（m,n）
    x = m - m2                  ! 计算z与目标位置的行相对偏差
	y = n - n2                  ! 计算z与目标位置的列相对偏差
	do while(x/=0 .or. y/=0)    ! 循环在x和y均不为0时运行
		if(abs(x) > abs(y))then       ! 若x绝对值比y大，则行移动（竖着走）
		  m10 = m - x/abs(x)
		  n10 = n
		elseif(abs(x) == abs(y))then  ! 若x的绝对值等于y，则行移动（竖着走）  
		  m10 = m - x/abs(x)
		  n10 = n
		else                          ! 若x绝对值小于y，则列移动（横着走）
		  m10 = m
		  n10 = n - y/abs(y)
		endif
	  call move_0 (a, m10, n10, z, num)  ! 将0移动到(m10, n10)处
    
!   将（m10，n10）和（m,n）交换
	  a(m10, n10) = a(m, n)
	  a(m, n) = 0
      m = m10           ! 重新记录z所在的位置（m,n）
      n = n10
	  x = m - m2        ! 重新记录x,y的值
	  y = n - n2
      num = num + 1

!   将a打印在屏幕上
      print*
      print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)

!   将a的值以及num值记录在文件record.txt中
      open(unit = 10, file='record.txt', action='write')
      write(10, "(10I4)") a, num
    
    enddo
    end subroutine

!=======================================================================
!   子程序move_0(a,m,n,k,num)用于将数字0移动到(m,n)处，并且在移动过
!   程中能够自动避开数字k，即移动过程中不打乱数字k所在的位置
!=======================================================================
	subroutine move_0(a, m, n, k, num)
    implicit none
	integer a(3,3), m, n, l(2)  ! l记录0所在的位置
	integer m_0, n_0            ! 0下一步要移动到的位置（m_0，n_0）
	integer x, y
	integer k
    integer i,j
    integer num                 ! 记录所走过的步数

	l = minloc(a)
	x = l(1) - m                ! 记录0与目标位置的行相对偏差
	y = l(2) - n                ! 记录0与目标位置的列相对偏差
	
	do while(x/=0 .or. y/=0)    ! 当0不在目标位置的时候进行循环
	if(abs(x) > abs(y))then         ! 行相对偏差较大时，移动行数(竖着走)
	  m_0 = l(1) - x/abs(x)
	  n_0 = l(2)
	   if(a(m_0, n_0) /= k)then      ! 如果目标位置不是k，直接替换即可
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   else						     ! 如果目标位置是k，想办法避开
		 m_0 = l(1)		                ! 办法：改为列移动（横着走）
	      if(y /= 0)then                 ! 若列相对偏差不为0，则按列接近
			n_0 = l(2) - y/abs(y)
		  else				             ! 若列数差为0，则按照下述方式移动：
		     if(l(2) == 3) n_0 = 2          ! 若0在第3列，则往第2列移动
			 if(l(2) == 2) n_0 = 3          ! 若0在第2列，则往第3列移动
			 if(l(2) == 1) n_0 = 2          ! 若0在第1列，则往第2列移动
		  endif
	     a(l(1), l(2)) = a(m_0, n_0) ! 实际移动
		 a(m_0, n_0) = 0
	   endif
	elseif(abs(y) > abs(x))then     !当列相对偏差较小时，移动列数（横着走），下来的代码与上述代码类似
	  m_0 = l(1)
	  n_0 = l(2) - y/abs(y)
	   if(a(m_0, n_0) /= k)then
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   else
		 n_0 = l(2)
		  if(x /= 0)then
		    m_0 = l(1) - x/abs(x)
		  else
		     if(l(1) == 3) m_0 = 2
			 if(l(1) == 2) m_0 = 3
			 if(l(1) == 1) m_0 = 2
		  endif
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   endif
	else                            ! 当行列相对偏差相等时
	   if(a(l(1)-x/abs(x), l(2)) == k)then     ! 如果行移动有障碍k，就列移动（横着走）
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
	   elseif(a(l(1), l(2)-y/abs(y)) == k)then ! 如果列移动有障碍k，就行移动（竖着走）
	     m_0 = l(1) - x/abs(x)
		 n_0 = l(2)
	   else                                    !若都没有障碍，则就行移动（竖着走）
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
       endif
	  a(l(1), l(2)) = a(m_0, n_0)       ! 实际移动
      a(m_0, n_0) = 0
    endif

!  将此时a的值打印屏幕上
    print*
    print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)
    
	l(1) = m_0          ! 重新记录0所在位置
    l(2) = n_0
	x = l(1) - m        ! 重新记录行列相对偏差
	y = l(2) - n
    num = num + 1

!  将此时a的值以及步数num记录在文件record.txt中
    open(unit = 10, file='record.txt', action='write')
    write(10, "(10I4)") a, num
    
	enddo

    end subroutine

!=======================================================================
!   子程序create_array(c,judge)用于产生随机的数组c，并判断c能否还原。
!   其中judge为逻辑变量，用于判断c能否还原，能则返回为T，否则返回为F，
!   并且整个程序结束。
!=======================================================================
	subroutine create_array(c, judge)
	implicit none
	integer		        :: b(2)                         ! 用于a的打乱
	integer,intent(out) :: c(3,3)
	integer			    :: a(9) = [1,2,3,4,5,6,7,8,0]   ! c的源数组，通过随机打乱a然后赋值给c
	integer			    :: i1,j1,rand, l,  i,j,   k=0
	real                :: rand_seed, i2,j2
	logical             :: judge

	call random_seed()
	call random_number(rand_seed)       ! 产生（0,1）之间随机数rand_seed
	rand = ceiling(rand_seed * 50)      ! 产生（0,50）之间的随机整数rand

	do l = 1, rand      ! 进行rand次循环
	  call random_number(i2)    ! 产生（0,1）之间的随机数i2
	  call random_number(j2)    ! 产生（0,1）之间的随机数j2
	  i1 = ceiling(i2 * 9)      ! 产生（0,9）之间的随机整数i1
	  j1 = ceiling(j2 * 9)      ! 产生（0,9）之间的随机整数j1

!    通过对换的方式打乱a
	  b(1) = a(i1)
	  b(2) = a(j1)
	  a(i1) = b(2)
	  a(j1) = b(1)
    enddo

!  计算a的逆序数k
	do i = 1,8
		do j = i+1,9
	      if(a(i) > a(j) .and. a(j) /= 0) k = k+1
		enddo
    enddo

!　将打乱后的a赋值给3×3数组c，此时随机生成c完毕
	c = reshape(a, [3,3], order=[2,1])

!  将此时的c数组打印在屏幕上，并判断k的奇偶性
    print*, '---The original array---'
    print*
    print "(3(3I3,/))", ((c(i,j), j=1,3), i=1,3)
    if(mod(k,2) == 0)then       ! 若为奇数，则逻辑变量值为T
	  print*, "It's OK!"
	  judge = .true.
	else                        ! 若为偶数，则逻辑变量值为F
	  print*, "Impossible!"
	  judge = .false.
    endif
    
	end subroutine