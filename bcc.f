ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       subroutine bcc
c
c      boundary conditions for the mass continuity equation
c      conditions for the confining surfaces
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(*,*)'entered bcc'
c
	i=1
	do 50 j=1,jim
	u2(i,j)=1.0d0
 50 	continue
c
	j=1
	do 51 i=1,iim
	v2(i,j)=0.0
 51	continue
c
	j=jim
	do 52 i=1,iim
	v2(i,j-1)=0.0
 52 	continue
c
c  ******  obstacle boundary conditions ********
c
	call bcobw
c
c **********************************************
c
         if(iti.ge.1) goto 55
	 if(iexit.eq.1) then
	 i=iim-2
	 do 53 j=1,jim
	 u2(i+1,j)=u2(i,j)
	 v2(i+1,j)=v2(i,j)
 53      continue
	 endif
c
	 if (iexit.eq.2) then
	 i=iim-2
	 do 54 j=1,jim
	 u2(i+1,j)=(deltax(i+1)/deltax(i))*(u2(i,j)-u2(i-1,j))
     $            +u2(i,j)
	 v2(i+1,j)=((deltax(i+1)+deltax(i))/(deltax(i)+deltax(i-1)))
     $            *(v2(i,j)-v2(i-1,j))+v2(i,j)
 54      continue
	 endif
c
	 if (iexit.eq.3) then
	 i=iim-2
	 do 74 j=1,jim
	 u2(i+1,j)=(deltax(i+1)/deltax(i))*(u2(i,j)-u2(i-1,j))
     $            +u2(i,j)
	 v2(i+1,j)=v2(i,j)
 74      continue
	 endif
c
	 if (iexit.eq.4) then
	 i=iim-2
	 do 75 j=1,jim
	 u2(i+1,j)=u1(i+1,j)-(deltat/deltax(i))*uc*
     $	           (u1(i+1,j)-u1(i,j))
	 v2(i+1,j)=v1(i+1,j)-(deltat/(0.5*(deltax(i)
     $	           +deltax(i+1))))*uc*(v1(i+1,j)-v1(i,j))
 75	 continue
	 endif
c
 55      continue
c
         return
	 end
