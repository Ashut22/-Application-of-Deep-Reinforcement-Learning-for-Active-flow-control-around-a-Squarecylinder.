cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine conti
c
c
c       pressure iteration
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write (6,*)'has entered conti'
c
	iti=0
	ita=ita+1
c
 40	call bcc
c
	call ceqcp
c
	if(divmax.ge.epsi) goto 40
c   
c	write(6,41)ita,iti,isum,divmax,imax,jmax
c 41	format(2x,3i7,4x,e15.8,2i5,1x,'from ceqcp2')
c
	return
	end
