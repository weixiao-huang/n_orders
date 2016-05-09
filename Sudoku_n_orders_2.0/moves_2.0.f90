!*****************************************************************
!
!   MODULE��moves
!
!   PURPOSE����װ���־����ƶ����ֹ��ܵ��ӳ���
!
!   PROGRAMMER����άХ
!
!   DATE��   Jan. 8th, 2014
!*****************************************************************
    
    module moves
    
!*****************************************************************
!   PUBLIC VARIABLES OF SUBROUTINES IN THE MODULE:
!    a   Դ����   l(2)   ����0�����ڵ�λ��   i,j ѭ������          
!    num �Ʋ���   shp(2) Դ�������״        n0  Դ�����������
!*****************************************************************

    contains
 
!=================================================================
!   �ӳ���move_row(a,r,num)��aΪԴ���飬rΪ��Ҫ��ԭ����ָ�꣬ͨ
!   ������ӳ��򣬿����ν�a������1��n-2�л�ԭΪ��ʼ״̬ 
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
    
    if (a(r, n0) == z1+n0-1)return          !�����3���Ѿ���Ŀ��λ�ã����˳�
    
    l = minloc(a)
    if(l(1)==r .and. l(2)==n0)then          !���0�����Ͻ�
        if(a(r+1, n0) == z1+n0-1)then           !�����3����0���ڵ����棬ֱ���ƶ�0�ɻع飬�������
          call move_0(a, r+1,n0, -1, num)
        elseif(a(r+2, n0) == z1+n0-1)then       !�����3�������½ǣ�ֱ�ӵ���move_near�ӳ���ع�
          call move_near(a, r,n0-1, r+2,n0, num)
        else                                    !���������������
          call move_0(a, r+1,n0, -1, num)           !�Ȱ�0�����ƶ�һ�����
          call move(a, z1+n0-1, r+2,n0, num)        !�ѡ�3���ƶ������½�
          call move_near(a, r,n0-1, r+2,n0, num)    !����move_near�ӳ���ع�
        endif
    else                                    !���0�������Ͻǣ����ڡ�3���������Ͻǵ�����Ѿ��ų��ˣ����Ǿ��Ȱѡ�3���ƶ������½�
        call move(a, z1+n0-1, r+2,n0, num)
        call move_near(a, r,n0-1, r+2,n0, num)  !����move_near�ӳ���ع�
    endif
    end subroutine

!=================================================================
!   �ӳ���move_near(a,m1,n1,m2,n2,num)���ڽ�a(m2,n2)�ϵ����ַŵ�
!   a(m1,n1)�������Ҳ�������²ࡣʹ���������£�
!      (m2-m1==2.and.n2-n1==1) .or. (m2-m1==1.and.n2-n1==2)
!   ʹ��ǰ�轫����a(m1,n1),a(m2,n2)Ų������������λ��
!=================================================================
    subroutine move_near(a, m1,n1, m2,n2, num)
    implicit none
    integer a(:,:)
    integer m1, n1, m2, n2  !m1,n1��¼���Ͻ��Ǹ�����λ�ã�m2,n2��¼���½��Ǹ�����λ��
    integer temp            !��¼���һ����ĳ������
    integer num
    integer l(2)
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)
    
    if (.not.( (m2-m1==2 .and. n2-n1==1).or.(m2-m1==1 .and. n2-n1==2) ) )then
        print*, 'Input error!'
        stop
    endif
    call move_0(a, m1+1,n1+1, a(m2,n2),num)   !��0�ƶ�����3�������棨��ֹ0�ƶ�������Ӱ�졮3����

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
!   �ӳ���move_2(a,z11,z22,md,nd,num)���ڽ�z22�ƶ���z11�����Ҳ�
!   (md,nd)��ʹ������Ϊz11����ָ��С��n-1
!=================================================================
    subroutine move_2(a, z11,z22, md,nd, num)  !m2,n2Ϊ��ʼʱ��2���ڵ�λ��, z11Ϊ���Ͻǵ����֣�'1'��,����ʱҪ�ܿ�z1
    implicit none
    integer a(:,:)
    integer m, n, m20, n20  !m, nΪÿʱÿ�̡�2������λ��, n20,n20Ϊ��һ�����Ŀ��λ��
    integer md, nd          !md,ndΪ��2��Ҫ�Ƶ���λ��
    integer  l(2)           !��¼0���ڵ�λ��
    integer i,j 
    integer num             !��¼�߹��Ĳ���
    integer z11,z22
    integer m1,n1           !��¼z1λ��
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)                  !��¼����a�Ľ���
    
    if(a(md,nd) == z22)return    !���z22�Ѿ���Ŀ��λ���ˣ����˳�

    call find_loc(a,z22, m,n)    !�ҵ�z22���ڵ�λ�ã�m,n��
    call find_loc(a,z11, m1,n1)  !�ҵ�z11���ڵ�λ�ã�m1,n1��
    l = minloc(a)                !��¼0���ڵ�λ��
      
    if(m == m1)then                  !��z22��z11������ͬ����������һ�ţ�
        call move_0(a, m,n-1, z22, num)  !��0�Ƶ�z22����ߣ��ܿ�������Ϊz22��
        call move(a, z22, md,nd, num)    !��z22�Ƶ�Ŀ��λ�ã��˳�
        return
    else                             !��2��1��������ͬ����
        if(l(1) == m1)then               !��0�ڵ�һ�š����Ƶ��ڶ���
            call move_0(a, l(1)+1,l(2), -1, num)
            if(a(md, nd) == z22)return       !��2�Ѿ�����Ŀ��λ�þ��˳�
        endif
        call move(a, z22, md+1,nd, num)  !��2�ƶ����Ź��������м��λ��
    endif
      
    m20 = md        !��һλ�ü�ΪĿ��λ��
    n20 = nd
    l = minloc(a)   !��¼0�ڵ�λ��
    if((l(1)==md .and. l(2)>=nd).or.(l(1)==md+1 .and. l(2)>=nd)) then   !���0�ڵ�1��2�ţ�����������Ŀ������֮�󣬾Ͳ������if
    else
        call move_0(a, md+2,nd+1, z22, num)     !��0Ų�����½�
    endif
    call move_0(a, m20, n20, z22, num)  !��0�ƶ���Ŀ��λ�ã�md,nd��
    call find_loc(a,z22, m,n)           !�ҵ�z22���ڵ�λ�ã�m,n�������м䣩

      a(m20, n20) = a(m, n)     !       !0��z22����
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
!   �ӳ���move_0(a,m,n,k,num)���ڽ�����0�ƶ���(m,n)������������
!   ���������ܹ��Զ��ܿ�����k�����ƶ������в�������k����
!=================================================================
    subroutine move_0(a, m, n, k, num)
    implicit none
	integer a(:,:), m, n, l(2)
    integer n0, shp(2)
	integer m_0, n_0            !��һ��Ŀ��
	integer x, y
	integer k                   !��Ҫ�ܿ�������
    integer i,j
    integer num

    shp = shape(a)
    n0 = shp(1)
	l = minloc(a)
	x = l(1) - m        !xΪ0��Ŀ��λ�õ��в��
	y = l(2) - n        !yΪ0��Ŀ��λ�õ��в��
	
	do while(x/=0 .or. y/=0) !���в�ࡢ�в�����Ϊ0����a(m,n)��Ϊ0�������������ѭ��
	if(abs(x) > abs(y))then    !�в��ϴ�ʱ���ƶ�����(������)����0���ӽӽ�Ŀ��λ��
	  m_0 = l(1) - x/abs(x)       !��һ������ָ����Ŀ��λ�ýӽ�1��
      n_0 = l(2)                  !��ָ�겻��
	   if(a(m_0, n_0) /= k)then     !���Ŀ��λ�ò�������k��ֱ����0�滻����
	     a(l(1), l(2)) = a(m_0, n_0)
		 a(m_0, n_0) = 0
	   else						    !���Ŀ��λ����k����취�ܿ�
		 m_0 = l(1)                   !�취����Ϊ���ƶ��������ߣ�
	      if(y /= 0)then                !���в�಻Ϊ0���ƶ��нӽ�Ŀ��λ��
			n_0 = l(2) - y/abs(y)         !��һ������ָ����Ŀ��λ�ýӽ�һ��
		  else                          !��������Ϊ0
		     if(l(2) == n0)then             !��0��n0�У���������1
                 n_0 = l(2) - 1
             else                           !������������1
                 n_0 = l(2) + 1
             endif
		  endif
	     a(l(1), l(2)) = a(m_0, n_0)  !��0�ƶ����������if�������ȷ������һ����λ��
		 a(m_0, n_0) = 0
	   endif
	elseif(abs(y) > abs(x))then !�в��ϴ����������в���ԭ����ͬ
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
	else                       !�����в����ʱ
	   if(a(l(1)-x/abs(x), l(2)) == k)then	    !������ƶ����ϰ��������ƶ��������ߣ�
	     m_0 = l(1)
		 n_0 = l(2) - y/abs(y)
	   elseif(a(l(1), l(2)-y/abs(y)) == k)then  !������ƶ����ϰ��������ƶ��������ߣ�
	     m_0 = l(1) - x/abs(x)
		 n_0 = l(2)
	   else                                     !����û���ϰ���������ƶ��������ߣ�
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
!   �ӳ���move(a,z,m2,n2,num)���ڽ�����z�ƶ���(m2,n2)����ע�⣺
!   �˳�����ܻ�������źõĸ��ӣ�ʹ�������أ�
!=================================================================
	subroutine move(a, z, m2, n2, num) !zΪ��Ҫ�ƶ������֣�m2,n2ΪĿ��λ��
    implicit none
	integer a(:,:)
	integer m, n, m2, n2, m10, n10  !m10, n10Ϊ1����һ����λ��
	integer x, y
	integer z                       !��Ҫ�ƶ�������
    integer num
    integer i,j
    integer n0, shp(2)
    
    shp = shape(a)
    n0 = shp(1)

    call find_loc(a, z, m,n)
	x = m - m2
	y = n - n2
	do while(x/=0 .or. y/=0)  !ѭ����x��y����Ϊ0ʱ����
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
	  call move_0 (a, m10, n10, z, num)  !move_0Ϊ��0�ƶ���(m10, n10)�����ӳ���
    
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
!   �ӳ���find_loc(a,x,m,n)����Ѱ������x������a�е�λ��(m,n)
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