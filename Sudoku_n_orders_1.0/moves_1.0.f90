!*****************************************************************
!
!   MODULE：moves
!
!   PURPOSE：封装各种具有移动数字功能的子程序
!
!   PROGRAMMER：黄维啸
!
!   DATE：   Jan. 8th, 2014
!*****************************************************************
    
    module moves
    
!*****************************************************************
!   PUBLIC VARIABLES OF SUBROUTINES IN THE MODULE:
!    a   源数组   l(2)   数字0所处在的位置   i,j 循环变量          
!    num 计步器   shp(2) 源数组的形状        n0  源数组的行列数
!*****************************************************************

    contains
 
!=================================================================
!   子程序move_row(a,r,num)中a为源数组，r为想要还原的行指标，通
!   过这个子程序，可依次将a数组中1到n-2行还原为初始状态 
!=================================================================
    subroutine move_row(a, r, num)
    implicit none
    integer a(:,:), r, num, i
    integer z1
    integer l(2)
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)

    z1 = 1 + n0 * (r - 1)
    
    call move(a,z1, r,1, num)
    do i = 1,n0-2
        call move_2(a,z1+i-1,z1+i, r,i+1, num)
    enddo
    
    if (a(r, n0) == z1+n0-1)return          !如果‘3’已经在目标位置，就退出
    
    l = minloc(a)
    if(l(1)==r .and. l(2)==n0)then          !如果0在右上角
        if(a(r+1, n0) == z1+n0-1)then           !如果‘3’在0相邻的下面，直接移动0可回归，程序结束
          call move_0(a, r+1,n0, -1, num)
        elseif(a(r+2, n0) == z1+n0-1)then       !如果‘3’在右下角，直接调用move_near子程序回归
          call move_near(a, r,n0-1, r+2,n0, num)
        else                                    !若都不是上述情况
          call move_0(a, r+1,n0, -1, num)           !先把0向下移动一格出来
          call move(a, z1+n0-1, r+2,n0, num)        !把‘3’移动到右下角
          call move_near(a, r,n0-1, r+2,n0, num)    !调用move_near子程序回归
        endif
    else                                    !如果0不在右上角，由于‘3’不在右上角的情况已经排除了，于是就先把‘3’移动到右下角
        call move(a, z1+n0-1, r+2,n0, num)
        call move_near(a, r,n0-1, r+2,n0, num)  !调用move_near子程序回归
    endif
    end subroutine

!=================================================================
!   子程序move_near(a,m1,n1,m2,n2,num)用于将a(m2,n2)上的数字放到
!   a(m1,n1)的相邻右侧或相邻下侧。使用条件如下：
!      (m2-m1==2.and.n2-n1==1) .or. (m2-m1==1.and.n2-n1==2)
!   使用前需将数字a(m1,n1),a(m2,n2)挪到满足条件的位置
!=================================================================
    subroutine move_near(a, m1,n1, m2,n2, num)
    implicit none
    integer a(:,:)
    integer m1, n1, m2, n2  !m1,n1记录左上角那个数的位置，m2,n2记录右下角那个数的位置
    integer temp            !记录最后一步的某个数字
    integer num
    integer l(2)
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)
    
    if (.not.( (m2-m1==2 .and. n2-n1==1).or.(m2-m1==1 .and. n2-n1==2) ) )then
        print*, 'Input error!'
        stop
    endif
    call move_0(a, m1+1,n1+1, a(m2,n2),num)   !把0移动到‘3’的上面（防止0移动过程中影响‘3’）

    if (m2-m1==2 .and. n2-n1==1)then
        call move_0(a, m1,n1+1, a(m1,n1), num)
        call move_0(a, m1,n1, -1, num)
        call move_0(a, m1+1,n1+1, a(m1,n1+1), num)
        call move_0(a, m2,n2, -1, num)
        call move_0(a, m1+1,n1, a(m1+1,n1+1), num)
        call move_0(a, m1,n1, -1, num)
        temp = a(m1+1,n1)
        call move_0(a, m1+1,n1+1, temp, num)
    elseif(m2-m1==1 .and. n2-n1==2)then
        call move_0(a, m1+1,n1, a(m1,n1), num)
        call move_0(a, m1,n1, -1, num)
        call move_0(a, m1+1,n1+1, a(m1+1,n1), num)
        call move_0(a, m2,n2, -1, num)
        call move_0(a, m1,n1+1, a(m1+1,n1+1), num)
        call move_0(a, m1,n1, -1, num)
        temp = a(m1,n1+1)
        call move_0(a, m1+1,n1+1, temp, num)
    else
        print*, 'Input error1 !'
    endif
    end subroutine    

!=================================================================
!   子程序move_2(a,z11,z22,md,nd,num)用于将z22移动到z11相邻右侧
!   (md,nd)，使用条件为z11的列指标小于n-1
!=================================================================
    subroutine move_2(a, z11,z22, md,nd, num)  !m2,n2为初始时刻2所在的位置, z11为左上角的数字（'1'）,运行时要避开z1
    implicit none
    integer a(:,:)
    integer m, n, m20, n20  !m, n为每时每刻‘2’所在位置, n20,n20为下一步骤的目标位置
    integer md, nd          !md,nd为‘2’要移到的位置
    integer  l(2)           !记录0所在的位置
    integer i,j 
    integer num             !记录走过的步数
    integer z11,z22
    integer m1,n1           !记录z1位置
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)                  !记录矩阵a的阶数
    
    if(a(md,nd) == z22)return    !如果z22已经在目标位置了，就退出

    call find_loc(a,z22, m,n)    !找到z22所在的位置（m,n）
    call find_loc(a,z11, m1,n1)  !找到z11所在的位置（m1,n1）
    l = minloc(a)                !记录0所在的位置
      
    if(m == m1)then                  !若z22和z11处在相同的排数（第一排）
        call move_0(a, m,n-1, z22, num)  !把0移到z22的左边（避开的数字为z22）
        call move(a, z22, md,nd, num)    !把z22移到目标位置，退出
        return
    else                             !若2和1不处于相同排数
        if(l(1) == m1)then               !若0在第一排——移到第二排
            call move_0(a, l(1)+1,l(2), -1, num)
            if(a(md, nd) == z22)return       !若2已经处在目标位置就退出
        endif
        call move(a, z22, md+1,nd, num)  !把2移动到九宫格中最中间的位置
    endif
      
    m20 = md        !下一位置即为目标位置
    n20 = nd
    l = minloc(a)   !记录0在的位置
    if((l(1)==md .and. l(2)>=nd).or.(l(1)==md+1 .and. l(2)>=nd)) then   !如果0在第1、2排，并且列数在目标列数之后，就不管这个if
    else
        call move_0(a, md+2,nd+1, z22, num)     !把0挪到右下角
    endif
    call move_0(a, m20, n20, z22, num)  !把0移动到目标位置（md,nd）
    call find_loc(a,z22, m,n)           !找到z22所在的位置（m,n）（即中间）

      a(m20, n20) = a(m, n)     !       !0和z22交换
      a(m ,n) = 0
      m = m20
      n = n20
      num = num + 1

    open(unit=10, file='record_1.txt', action='write')
    print*
    print "(<n0>(<n0>I4,/))", ((a(i,j), j=1,n0), i=1,n0)
    write(10, "(<n0>(<n0>I4,/))") ((a(i,j), j=1,n0), i=1,n0)
      
    open(unit=20, file='record.txt', action='write')
    write(20, "(<n0*n0>I4,I7)") a, num
      
    end subroutine
    
!=================================================================
!   子程序move_0(a,m,n,k,num)用于将数字0移动到(m,n)处，并且在移
!   动过程中能够自动避开数字k，即移动过程中不和数字k交换
!=================================================================
    subroutine move_0(a, m, n, k, num)
    implicit none
	integer a(:,:), m, n, l(2)
    integer n0, shp(2)
	integer m_0, n_0            !下一步目标
	integer x, y
	integer k                   !需要避开的整数
    integer i,j
    integer num

    shp = shape(a)
    n0 = shp(1)
	l = minloc(a)
	x = l(1) - m        !x为0与目标位置的行差距
	y = l(2) - n        !y为0与目标位置的列差距
	
	do while(x/=0 .or. y/=0) !若行差距、列差距均不为0（即a(m,n)不为0），则进行以下循环
	if(abs(x) > abs(y))then    !行差距较大时，移动行数(竖着走)，让0更加接近目标位置
	  m_0 = l(1) - x/abs(x)       !下一步的行指标向目标位置接近1步
      n_0 = l(2)                  !列指标不变
	   if(a(m_0, n_0) /= k)then     !如果目标位置不是数字k，直接用0替换即可
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   else						    !如果目标位置是k，想办法避开
		 m_0 = l(1)                   !办法：改为列移动（横着走）
	      if(y /= 0)then                !若列差距不为0，移动列接近目标位置
			n_0 = l(2) - y/abs(y)         !下一步的列指标向目标位置接近一步
		  else                          !若列数差为0
		     if(l(2) == n0)then             !若0在n0列，则列数减1
                 n_0 = l(2) - 1
             else                           !否则列数都加1
                 n_0 = l(2) + 1
             endif
		  endif
	     a(l(1), l(2)) = a(m_0, n_0)  !将0移动到从上面的if语句中所确定的下一步的位置
		 a(m_0, n_0) = 0
	   endif
	elseif(abs(y) > abs(x))then !列差距较大的情况，与行差距的原理相同
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
             if(l(1) == n0)then
                 m_0 = l(1) - 1
             else
                 m_0 = l(1) + 1
             endif
		  endif
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   endif
	else                       !当行列差相等时
	   if(a(l(1)-x/abs(x), l(2)) == k)then	    !如果行移动有障碍，就列移动（横着走）
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
	   elseif(a(l(1), l(2)-y/abs(y)) == k)then  !如果列移动有障碍，就行移动（竖着走）
	     m_0 = l(1) - x/abs(x)
		 n_0 = l(2)
	   else                                     !若都没有障碍，则就行移动（竖着走）
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
	   endif
	  a(l(1), l(2)) = a(m_0, n_0)
      a(m_0, n_0) = 0
    endif
 	l(1) = m_0
    l(2) = n_0
	x = l(1) - m
	y = l(2) - n
    num = num + 1    
   
    open(unit=10, file='record_1.txt', action='write')
    print*
    print "(<n0>(<n0>I4,/))", ((a(i,j), j=1,n0), i=1,n0)
    write(10, "(<n0>(<n0>I4,/))") ((a(i,j), j=1,n0), i=1,n0)
 
    open(unit=20, file='record.txt', action='write')
    write(20, "(<n0*n0>I4,I7)") a, num

	enddo

    end subroutine
    
!=================================================================
!   子程序move(a,z,m2,n2,num)用于将数字z移动到(m2,n2)处，注意：
!   此程序可能会打乱已排好的格子，使用需慎重！
!=================================================================
	subroutine move(a, z, m2, n2, num) !z为所要移动的数字，m2,n2为目标位置
    implicit none
	integer a(:,:)
	integer m, n, m2, n2, m10, n10  !m10, n10为1的下一步的位置
	integer x, y
	integer z                       !所要移动的数字
    integer num
    integer i,j
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)

    call find_loc(a, z, m,n)
	x = m - m2
	y = n - n2
	do while(x/=0 .or. y/=0)  !循环在x和y均不为0时运行
		if(abs(x) > abs(y))then
		  m10 = m - x/abs(x)
		  n10 = n
		elseif(abs(x) == abs(y))then
		  m10 = m - x/abs(x)
		  n10 = n
		else
		  m10 = m
		  n10 = n - y/abs(y)
		endif
	  call move_0 (a, m10, n10, z, num)  !move_0为将0移动到(m10, n10)处的子程序
    
	  a(m10, n10) = a(m, n)
	  a(m, n) = 0
      m = m10
      n = n10
	  x = m - m2
	  y = n - n2
    num = num + 1

    open(unit=10, file='record_1.txt', action='write')
    print*
    print "(<n0>(<n0>I4,/))", ((a(i,j), j=1,n0), i=1,n0)
    write(10, "(<n0>(<n0>I4,/))") ((a(i,j), j=1,n0), i=1,n0)
    
    open(unit=20, file='record.txt', action='write')
    write(20, "(<n0*n0>I4,I7)") a, num
   
    enddo
    end subroutine

!=================================================================
!   子程序find_loc(a,x,m,n)用于寻找数字x在数组a中的位置(m,n)
!=================================================================
    subroutine find_loc(a, x ,m, n)
    implicit none
    integer a(:,:), x
    integer,intent(out) :: m,n
    integer i,j
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)

    do i = 1,n0
       do j = 1,n0
          if(a(i,j) == x)then
            m = i
            n = j
            return
          endif
       enddo
    enddo
    end subroutine
  
end module