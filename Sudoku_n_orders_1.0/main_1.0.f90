!*****************************************************************
!
!   PROGRAM�����žŹ�����ƹ㣬nά���ӵĻ�ԭ
!
!   PROGRAMMER����άХ
!
!   PURPOSE��ͨ���������������Сn��n�Ĵ�С����1<n<31����
!            �����Զ����ɴ�СΪn������a�����Զ��ж��ܷ�
!            ԭ��Ŀ�����Σ������ԣ����Զ���ԭ���ѻ�ԭ����
!            ��ӡ����Ļ�ϣ�����¼���ļ���(�ļ�record_1.txt
!            ��¼n��n�о���a���ƶ�������ļ�record.txt��
!            ��a�����������ƶ����裬���һλ���ƶ��Ĳ���)
!
!   DATE��   Jan. 8th, 2014
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
    
!  ����a�Ĵ�Сn��Ȼ���������a�����Զ��ж��ܷ�ﵽĿ��λ��
    print *, 'Please input the number of n (1<n<31) :'
    read *, n
    allocate(a(n,n))
    call create_array(a, n, judge)
    if(.not. judge)stop
 
!-----------------------------------------------------------------!
!  if you want to know more about why it's impossible, you can    !
!  cut the program at line 32 and paste it at line 55.            !
!-----------------------------------------------------------------!

!  �𲽻�ԭa�ĵ�1�е���n-2��
    do i = 1, n - 2
        call move_row(a, i, num)
    enddo

!  �𲽻�ԭa�ĵ�n-1��n�еĵ�1�е���n-2��
    do i = 1, n-2
        call move(a, i+n*(n-2), n-1,i, num)             !����ȷ�����ַ������Ͻ�
        if(a(n,i) == 0) call move_0(a, n,i+1, -1, num)  !�����½���0���ƶ�����
        if(a(n,i) == i+n*(n-1))cycle                    !�����½�����ȷ�����֣�ok!
        call move(a, i+n*(n-1), n,i+2, num)             !�����ƶ������ֵ����½�
        call move_near(a, n-1,i, n,i+2, num)
    enddo

!  ��0�ƶ������½�
    call move_0(a, n,n, 1, num)

!  �������ֲ�����Ŀ��λ�õ����Σ������ǻ�ԭ��Ŀ��λ��
    if(a(n,n-1) == n*(n-1)-1 .and. a(n-1,n) == n*n-1)then       !����Ϊn*(n-1)-1������Ϊn*n-1�����
        call move_0(a, n-1,n-1, n*(n-1)-1, num)
        call move_0(a, n,n, n*(n-1), num)
    elseif(a(n,n-1) == n*(n-1) .and. a(n-1,n) == n*(n-1)-1)then !����Ϊn*(n-1)������Ϊn*(n-1)-1�����
        call move_0(a, n-1,n-1, n*(n-1)-1, num)
        call move_0(a, n,n, n*n-1, num)
    endif
    
    print "('�ƶ�������',I5)", num
    close(10)
    if (judge) print*, "OK!"
    
    end program
    
    
!=================================================================
!   �ӳ���create_array(c,n,judge)����������ɴ�СΪn������c����
!   ���ж��ܷ�ԭ��Ŀ��״̬����judgeΪ�߼��������ж�c�ܷ�ԭ��
!   ���򷵻�ΪT�����򷵻�ΪF�������������������
!=================================================================
	subroutine create_array(c, n, judge)
	implicit none
    
    integer         :: m                    ! size(a)
    integer,intent(in):: n                  ! a�Ĵ�С
	integer		    :: b(2)                 ! �ڽ�a������ҵĳ����м�¼a��������Ҫ�Ի������ֵ�ֵ
	integer,intent(out) :: c(n,n)
	integer,allocatable :: a(:)             ! ���д���cʱ����õ�1ά���飬nΪ��������²�����ô�ƶ�c��a������������
	integer,allocatable :: d(:)             ! ��Z���δ���cʱ����õ�1ά���飬nΪż������²�����ô�ƶ�c��d������������
	integer			:: i1,j1,rand, l,  i,j
	integer ::  k1 = 0, k2 = 0              ! k1Ϊ��ʼa��������k2Ϊ��ʼd������
	real			   rand_seed, i2,j2
	logical judge

    m = n * n
    allocate(a(m), d(m))
    a(1:m) = [1:m]
    a(m) = 0
	call random_seed()
	call random_number(rand_seed)
	rand = ceiling(rand_seed * m*m)  ! ѭ�����������Ҵ�����

!  �������Ի�����ʽ��a����rand��
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

!  �����Һ��a���и�ֵ��Դ����c
	c = reshape(a, [n,n], order=[2,1])

!  ��Z���ν�c�����ݸ�ֵ��d
	do i = 1, n
		if(mod(i,2) == 1)then
			d(1+(i-1)*n : i*n) = c(i, 1:n)
		else
			d(1+(i-1)*n : i*n) = c(i, n:1:-1)
		endif
    enddo

!  ���ʼʱ��a��d��������k1,k2
	do i = 1, m-1
		do j = i+1,m
	      if(a(i) > a(j) .and. a(j) /= 0) k1 = k1+1
	      if(d(i) > d(j) .and. d(j) /= 0) k2 = k2+1
		enddo
    enddo

!  ��ӡ��ʼ����c
    print*, '---The original array---'
    print*
    print "(<n>(<n>I4,/))", ((c(i,j), j=1,n), i=1,n)

!  ��nΪ��������������k1���������������k2
    if(mod(n,2) == 1)then
        if(mod(k1,2) == 0)then  !Ŀ������е�k1=0���ʳ�ʼ״̬k1ӦΪż���ſɽ�
	      print*, "It's OK!"
	      judge = .true.
	    else                    !��k1Ϊ�������򲻿ɽ⣬Impossible
	      print*, "Impossible!"
	      judge = .false.
        endif
    else
        if(mod(n/2, 2) == 0)then      !��n/2��ż������Ŀ������е�k2ӦΪ�����ſɽ�
            if(mod(k2,2) == 1)then      !����ʼ���k2Ϊ������OK!
	            print*, "It's OK!"
	            judge = .true.
	        else                        !����ʼ���k2Ϊż����Impossible!
                print*, "Impossible!"   
	            judge = .false.
            endif
        else                          !��n/2����������Ŀ������е�k2ӦΪż���ſɽ�
            if(mod(k2,2) == 0)then      !����ʼ���k2Ϊż����OK!
	            print*, "It's OK!"
	            judge = .true.
	        else                        !����ʼ���k2Ϊ������Impossible!
                print*, "Impossible!"
	            judge = .false.
            endif
        endif       
    endif
    if (.not. judge)print*,"If you want to know more, see line 34 to 38 in the program main."
    
    end subroutine