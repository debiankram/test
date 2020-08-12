ccc   simpson'a 1/3rd rule
      implicit real*8(a-h,o-z)
      common/simp/p,q,r
      open(1,file='simpson.in',status='unknown')
      open(2,file='simpsonout.dat',status='unknown')
      read(1,*)a,b
      read(1,*)p,q,r
      write(*,*)'supply n'
      read(*,*)n
      an=dfloat(n)
      h=(b-a)/an
      s1=0.d0
      s2=0.d0
      do i=1,n-1,2
      ai=dfloat(i)
      s1=s1+f(a+ai*h)
      write(*,*)s1
      enddo
      do j=2,n-2,2
      aj=dfloat(j)
      s2=s2+f(a+aj*h)
      write(*,*)s2
      enddo
      r=(h/3)*(f(a)+f(b)+4.d0*s1+2.d0*s2)
      write(2,*)r
      stop
      end
cc-----------------------------------------------------------------------
      function f(x)
      implicit real*8(a-h,o-z)
      common/simp/p,q,r
      f=p*x*x+q*x+r
      return
      end
