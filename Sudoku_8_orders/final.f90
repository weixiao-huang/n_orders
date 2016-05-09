!**********************************************************************
!
!   PROGRAM�����žŹ���
!
!   PROGRAMMER����άХ
!
!   PURPOSE������������ɴ�СΪ3������a�����Զ��ж��ܷ�
!            ԭ��Ŀ�����Σ������ԣ����Զ���ԭ���ѻ�ԭ����
!            ��ӡ����Ļ�ϣ�����¼���ļ�record.txt��
!
!   DATE    ��Jan. 3ed, 2014
!   MODIFIED��Jan. 10th, 2014  
!**********************************************************************   
    
    program main
	implicit none
	integer :: a(3,3)
	integer i, j                        ! ѭ������
    integer :: num = 0                  ! �Ʋ���
    logical judge                       ! �߼��������ж�a�Ƿ��ܴﵽĿ��λ�ã����򷵻�T
    integer :: a1(10) = 1, a0(10) = 0   ! ����д���ĵ��ĵڶ��У�����MATLAB��ȡ���ж��Ƿ��ܹ��ﵽĿ��λ��
    real    :: t1 = 0, t2 = 0           ! ���ڼ����������ʱ��
    
    call create_array(a, judge)     ! �����������a���������߼�����judge

!  ��a���鰴�м�¼���ļ�record.txt�У����һλ�ǼƲ���num
    open(unit = 10, file='record.txt', action='write')
    write(10, "(10I4)") a, num

!  ��judgeΪT������record.txt�ڶ���д��10��0��������record.txt�ڶ���д��10��1������ֹͣ
    if(.not. judge) then
        write(10, "(10I4)") a1
        stop
    else
        write(10, "(10I4)") a0
    endif

!  ��ӡa����Ļ��
	print*
    print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)

!  �����Ǿ����ƶ�����
    call CPU_TIME(t1)           ! �ڴ˴���¼��ʱ��cpuʱ��
    call move(a, 1, 1, 1, num)  ! ��0�ƶ�����1,1����
    call move_2(a, num)         ! ��2�ƶ�����1,2����
    
!  ��3�ƶ���Ŀ��λ��
    if (a(2,3)==3 .and. a(1,3)==0)then          ! ���0�����Ͻǣ�3ǡ����0����
        call move_0(a, 2,3, 2, num)                ! ֱ��0��3��������
    elseif(a(1,3) /= 3)then                     ! ��3����Ŀ��λ�ã�3,3��
        if(a(1,3) == 0) call move_0(a, 2, 3, 2,num) ! ��0�����Ͻǣ��������ƶ�һ�񣬷�ֹ֮��0���ƶ�Ӱ���Ѿ���ԭ��1��2
        call move(a, 3, 3, 3, num)                     ! �����ӳ�������3�ƶ�����3,3����
        call move_0(a, 1,3, 2, num)                    ! ������0�ƶ�����1,3�������������ӳ���move_near��ʹ������
        call move_near(a, 1,2, 3,3, num)               ! �����ӳ���move_near��3�ƻ�Ŀ��λ�ã�1,3��
    endif
    
!  ��4��7�ƶ���Ŀ��λ�ã�������һ��
    call move(a, 4, 2, 1, num)          ! ��4�ƶ���Ŀ��λ�ã�2,1��
    if(a(3,2)==7 .and. a(3,1)==0)then   ! ���0�����½ǣ�7�պ���0�ұ�
        call move_0(a, 3,2, 4, num)        ! ֱ�ӽ���0��7����
    elseif(a(3,1) /= 7)then             ! ���7����Ŀ��λ�ã�3,1��
        call move(a, 7, 3, 3, num)         ! ��7�ƶ�����3,3����
        call move_0(a, 3,1, 4, num)        ! ��0�ƶ�����3,1�������������ӳ���move_near��ʹ������
        call move_near(a, 2,1, 3,3, num)   ! �����ӳ���move_near��7�ƻ�Ŀ��λ�ã�3,1��
    endif

!  ���һ�����ƶ�5,6,8�ع�
    call move_0(a, 3,3, 1, num)         ! ��0�ƶ������½ǣ�3,3��������ֻ���������δ�ﵽĿ��λ��
    if(a(3,2) == 5)then                 ! ��a(3,2)=5��a(2,3)=8
        call move_0(a, 2,2, 5, num)
        call move_0(a, 3,3, 6, num)
    elseif(a(3,2) == 6)then             ! ��a(3,2)=6��a(2,3)=8
        call move_0(a, 2,2, 5, num)
        call move_0(a, 3,3, 8, num)
    endif
    
    call CPU_TIME(t2)                   ! �鿴��ʱ��cpuʱ��
!  ��ӡ��������Ĳ���������ռ�õ�cpuʱ��
    print "('���貽����',I3)", num
    print "('��������ʱ�䣺',F9.6)",t2-t1
    
!  �����ս��д���ļ�record.txt��
    open(unit=10, file='record.txt', action='write')
    write(10, "(F9.6)")t2-t1
    end program
    
    
!======================================================================
!   �ӳ���find_loc(a,x,m,n)����Ѱ������x������a�е�λ��(m,n)
!======================================================================
    subroutine find_loc(a, x ,m, n)
    implicit none
    integer a(3,3), x
    integer,intent(out) :: m,n
    integer i,j
    do i = 1,3
       do j = 1,3
          if(a(i,j) == x)then  ! ��a(i,j) == x,���¼�´�ʱ��i,jֵ����m,n
            m = i
            n = j
            return
          endif
       enddo
    enddo
    end subroutine
    
!======================================================================
!   �ӳ���move_near(a,m1,n1,m2,n2,num)���ڽ�a(m2,n2)�ϵ����ַŵ�
!   a(m1,n1)�������Ҳ�������²ࡣʹ���������£�a(m1,n1+1)==0��
!   (m2-m1==2.and.n2-n1==1) .or. (m2-m1==1.and.n2-n1==2)
!=======================================================================
    subroutine move_near(a, m1,n1, m2,n2, num)
    implicit none
    integer a(3,3)
    integer m1, n1, m2, n2  ! m1,n1��¼���Ͻ��Ǹ�����λ�ã�m2,n2��¼���½��Ǹ�����λ��
    integer temp            ! ���һ������Ϊ�滻����
    integer num             ! �Ʋ���
    if (m2/=3 .or. n2/=3)then   ! ��������ʹ�����������˳�����ʾ��Input error����
        print*, 'Input error!'
        stop
    endif
    if (m1==1 .and. n1==2)then      ! �����Ͻ��Ǹ������ڣ�1,2��λ��
        call move_0(a, 1,2, 3, num)     ! ��0�ƶ�����1,2�������ܿ�����3
        call move_0(a, 2,3, 2, num)     ! ��0�ƶ�����2,3�������ܿ�����2
        call move_0(a, 3,3, 2, num)     ! ��0�ƶ�����3,3�������ܿ�����2
        call move_0(a, 2,2, 3, num)     ! ��0�ƶ�����2,2�������ܿ�����3
        call move_0(a, 1,2, 3, num)     ! ��0�ƶ�����1,2�������ܿ�����3
        temp = a(2,2)                   ! temp��¼a(2,2)��������
        call move_0(a, 2,3, temp, num)  ! ��0�ƶ�����2,3�������ܿ�����temp
    elseif(m1==2 .and. n1==1)then   ! �����Ͻ��Ǹ������ڣ�2,1��λ��
        call move_0(a, 2,1, 7, num)     ! ��0�ƶ�����2,1�������ܿ�����7
        call move_0(a, 3,2, 4, num)     ! ��0�ƶ�����3,2�������ܿ�����4
        call move_0(a, 3,3, 4, num)     ! ��0�ƶ�����3,3�������ܿ�����4
        call move_0(a, 2,2, 7, num)     ! ��0�ƶ�����2,2�������ܿ�����7
        call move_0(a, 2,1, 7, num)     ! ��0�ƶ�����2,1�������ܿ�����7
        temp = a(2,2)                   ! temp��¼a(2,2)��������
        call move_0(a, 3,2, temp, num)  ! ��0�ƶ�����2,3�������ܿ�����temp
    else
        print*, 'Input error1 !'
    endif
    end subroutine
    
!=======================================================================
!   �ӳ���move_2(a,num)���ڽ�����2�ƶ���Ŀ��λ�ã�1,2��
!=======================================================================
    subroutine move_2(a, num)
    implicit none
    integer a(3,3)
    integer m, n, m20, n20      ! m,nΪ2���ڵ�λ�ã�m20,n20Ϊ2��һ��Ҫ�Ƶ���λ��
    integer x,y,  l(2)          ! l��¼0���ڵ�����λ��
    integer i,j                 ! ѭ������
    integer num                 ! �Ʋ���
    
    call find_loc(a, 2, m,n)
    x = m - 1           ! 2���ڵ�λ����Ŀ��λ�õ������ƫ��
    y = n - 2           ! 2���ڵ�λ����Ŀ��λ�õ������ƫ��
    do while(x/=0 .or. y/=0)    ! ��2����Ŀ��λ�þͽ���ѭ��
      l = minloc(a)             ! Ѱ��0���ڵ�λ��
       if((m==3 .and. n==1) .and. (l(1)==1 .and. l(2)==2))call move_0(a, 2,1, 1, num)    ! ��2��(3,1)����0��(1,2)����Ӧ�Ȱ�0�Ƶ��ڶ���(2,1)��
       if(m==2 .and. n==2)then  ! �����������2���ڣ�2,2����
         m20 = 1
         n20 = 2
         l = minloc(a)
          if((l(1)==2 .and. l(2)==1).or.(l(1)==3 .and. l(2)==1)) then  ! ��0�ڣ�2,1�����ߣ�3,1����
            call move_0(a,2,3,2, num)                                  ! �Ȱ�0�ƶ�����3,2�����λ�ã���ֹ0�ƶ������д���0���ڵ�λ��
          endif
       else                     ! ��2���ڣ�2,2�����������ӳ���move�ķ�ʽ���ƶ�2��Ŀ��λ��
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
      call move_0(a, m20, n20, 2, num)  ! ��0�ƶ���2��Ҫ�ƶ�����λ�ã�m20,n20�����Ա�0��2�Ի�ʹ��2�Ƶ�Ŀ��λ��
      a(m20, n20) = a(m, n)
      a(m ,n) = 0
      m = m20           ! ���¼�¼2��λ�ã�m,n��
      n = n20
      x = m - 1         ! ���¼�¼x,y��ֵ
      y = n - 2

!   ��a��ӡ����Ļ��
      print*
      print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)
      num = num + 1

!   ������a�Լ��ƶ�������¼���ļ�record.txt��
      open(unit = 10, file='record.txt', action='write')
      write(10, "(10I4)") a, num
      
    enddo
    end subroutine
    
!=======================================================================
!   �ӳ���move(a,z,m2,n2,num)���ڽ�����z�ƶ���(m2,n2)����ע�⣺�˳���
!   ���ܻ�������źõĸ��ӣ�ʹ�������أ�
!=======================================================================
	subroutine move(a, z, m2,n2, num)
    implicit none
	integer a(3,3)
	integer m, n, m2, n2, m10, n10  ! m10, n10Ϊ1����һĿ��λ��
	integer x, y
	integer z
    integer num                     ! �Ʋ���
    integer i,j                     ! ѭ������

    call find_loc(a, z, m,n)    ! Ѱ������z���ڵ�λ�ã�m,n��
    x = m - m2                  ! ����z��Ŀ��λ�õ������ƫ��
	y = n - n2                  ! ����z��Ŀ��λ�õ������ƫ��
	do while(x/=0 .or. y/=0)    ! ѭ����x��y����Ϊ0ʱ����
		if(abs(x) > abs(y))then       ! ��x����ֵ��y�������ƶ��������ߣ�
		  m10 = m - x/abs(x)
		  n10 = n
		elseif(abs(x) == abs(y))then  ! ��x�ľ���ֵ����y�������ƶ��������ߣ�  
		  m10 = m - x/abs(x)
		  n10 = n
		else                          ! ��x����ֵС��y�������ƶ��������ߣ�
		  m10 = m
		  n10 = n - y/abs(y)
		endif
	  call move_0 (a, m10, n10, z, num)  ! ��0�ƶ���(m10, n10)��
    
!   ����m10��n10���ͣ�m,n������
	  a(m10, n10) = a(m, n)
	  a(m, n) = 0
      m = m10           ! ���¼�¼z���ڵ�λ�ã�m,n��
      n = n10
	  x = m - m2        ! ���¼�¼x,y��ֵ
	  y = n - n2
      num = num + 1

!   ��a��ӡ����Ļ��
      print*
      print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)

!   ��a��ֵ�Լ�numֵ��¼���ļ�record.txt��
      open(unit = 10, file='record.txt', action='write')
      write(10, "(10I4)") a, num
    
    enddo
    end subroutine

!=======================================================================
!   �ӳ���move_0(a,m,n,k,num)���ڽ�����0�ƶ���(m,n)�����������ƶ���
!   �����ܹ��Զ��ܿ�����k�����ƶ������в���������k���ڵ�λ��
!=======================================================================
	subroutine move_0(a, m, n, k, num)
    implicit none
	integer a(3,3), m, n, l(2)  ! l��¼0���ڵ�λ��
	integer m_0, n_0            ! 0��һ��Ҫ�ƶ�����λ�ã�m_0��n_0��
	integer x, y
	integer k
    integer i,j
    integer num                 ! ��¼���߹��Ĳ���

	l = minloc(a)
	x = l(1) - m                ! ��¼0��Ŀ��λ�õ������ƫ��
	y = l(2) - n                ! ��¼0��Ŀ��λ�õ������ƫ��
	
	do while(x/=0 .or. y/=0)    ! ��0����Ŀ��λ�õ�ʱ�����ѭ��
	if(abs(x) > abs(y))then         ! �����ƫ��ϴ�ʱ���ƶ�����(������)
	  m_0 = l(1) - x/abs(x)
	  n_0 = l(2)
	   if(a(m_0, n_0) /= k)then      ! ���Ŀ��λ�ò���k��ֱ���滻����
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   else						     ! ���Ŀ��λ����k����취�ܿ�
		 m_0 = l(1)		                ! �취����Ϊ���ƶ��������ߣ�
	      if(y /= 0)then                 ! �������ƫ�Ϊ0�����нӽ�
			n_0 = l(2) - y/abs(y)
		  else				             ! ��������Ϊ0������������ʽ�ƶ���
		     if(l(2) == 3) n_0 = 2          ! ��0�ڵ�3�У�������2���ƶ�
			 if(l(2) == 2) n_0 = 3          ! ��0�ڵ�2�У�������3���ƶ�
			 if(l(2) == 1) n_0 = 2          ! ��0�ڵ�1�У�������2���ƶ�
		  endif
	     a(l(1), l(2)) = a(m_0, n_0) ! ʵ���ƶ�
		 a(m_0, n_0) = 0
	   endif
	elseif(abs(y) > abs(x))then     !�������ƫ���Сʱ���ƶ������������ߣ��������Ĵ�����������������
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
	else                            ! ���������ƫ�����ʱ
	   if(a(l(1)-x/abs(x), l(2)) == k)then     ! ������ƶ����ϰ�k�������ƶ��������ߣ�
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
	   elseif(a(l(1), l(2)-y/abs(y)) == k)then ! ������ƶ����ϰ�k�������ƶ��������ߣ�
	     m_0 = l(1) - x/abs(x)
		 n_0 = l(2)
	   else                                    !����û���ϰ���������ƶ��������ߣ�
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
       endif
	  a(l(1), l(2)) = a(m_0, n_0)       ! ʵ���ƶ�
      a(m_0, n_0) = 0
    endif

!  ����ʱa��ֵ��ӡ��Ļ��
    print*
    print "(3(3I3,/))", ((a(i,j), j=1,3), i=1,3)
    
	l(1) = m_0          ! ���¼�¼0����λ��
    l(2) = n_0
	x = l(1) - m        ! ���¼�¼�������ƫ��
	y = l(2) - n
    num = num + 1

!  ����ʱa��ֵ�Լ�����num��¼���ļ�record.txt��
    open(unit = 10, file='record.txt', action='write')
    write(10, "(10I4)") a, num
    
	enddo

    end subroutine

!=======================================================================
!   �ӳ���create_array(c,judge)���ڲ������������c�����ж�c�ܷ�ԭ��
!   ����judgeΪ�߼������������ж�c�ܷ�ԭ�����򷵻�ΪT�����򷵻�ΪF��
!   �����������������
!=======================================================================
	subroutine create_array(c, judge)
	implicit none
	integer		        :: b(2)                         ! ����a�Ĵ���
	integer,intent(out) :: c(3,3)
	integer			    :: a(9) = [1,2,3,4,5,6,7,8,0]   ! c��Դ���飬ͨ���������aȻ��ֵ��c
	integer			    :: i1,j1,rand, l,  i,j,   k=0
	real                :: rand_seed, i2,j2
	logical             :: judge

	call random_seed()
	call random_number(rand_seed)       ! ������0,1��֮�������rand_seed
	rand = ceiling(rand_seed * 50)      ! ������0,50��֮����������rand

	do l = 1, rand      ! ����rand��ѭ��
	  call random_number(i2)    ! ������0,1��֮��������i2
	  call random_number(j2)    ! ������0,1��֮��������j2
	  i1 = ceiling(i2 * 9)      ! ������0,9��֮����������i1
	  j1 = ceiling(j2 * 9)      ! ������0,9��֮����������j1

!    ͨ���Ի��ķ�ʽ����a
	  b(1) = a(i1)
	  b(2) = a(j1)
	  a(i1) = b(2)
	  a(j1) = b(1)
    enddo

!  ����a��������k
	do i = 1,8
		do j = i+1,9
	      if(a(i) > a(j) .and. a(j) /= 0) k = k+1
		enddo
    enddo

!�������Һ��a��ֵ��3��3����c����ʱ�������c���
	c = reshape(a, [3,3], order=[2,1])

!  ����ʱ��c�����ӡ����Ļ�ϣ����ж�k����ż��
    print*, '---The original array---'
    print*
    print "(3(3I3,/))", ((c(i,j), j=1,3), i=1,3)
    if(mod(k,2) == 0)then       ! ��Ϊ���������߼�����ֵΪT
	  print*, "It's OK!"
	  judge = .true.
	else                        ! ��Ϊż�������߼�����ֵΪF
	  print*, "Impossible!"
	  judge = .false.
    endif
    
	end subroutine