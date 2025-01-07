cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine velalt
c
c       alternation of the velocity arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c
	do 80 i=1,iim
		do 80 j=1,jim
			u1(i,j) = u2(i,j)
			v1(i,j) = v2(i,j)
 80	continue
c
c
	return
	end

