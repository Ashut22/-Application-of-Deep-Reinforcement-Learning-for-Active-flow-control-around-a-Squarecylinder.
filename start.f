ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine start
c
c
c       start conditions
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	zeit=0.
c      
c  ***** start of a parabolic calculation *****
c
c 	write(6,*)'has entered start'
c
c  ***** constnt velocity profile  ******
c
	do 30 j=1,jim
	qinlet(j)=1.0
  30    continue
c
c ****** defining the inlet profile *******
c
	do 31 i=1,iim
	do 31 j=1,jim
        u2(i,j)=qinlet(j)
        u1(i,j)=qinlet(j)
	v2(i,j)=0.0
	v1(i,j)=0.0
	p(i,j)=1.0
  31    continue
c
	return
	end
