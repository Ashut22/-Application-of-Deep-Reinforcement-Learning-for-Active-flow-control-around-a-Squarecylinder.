ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
        subroutine bcns 
c   
c       boundary conditions for the navier stokes equations 
c       conditions for the confining surfaces 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(*,*)'entered bcns'
c	
        j=2
	do 170 i=1,iim
	u2(i,j-1)=jn1*u2(i,j)
	v2(i,j-1)=0.0
 170 	continue
c
	j=jim
	do 171 i=1,iim
	u2(i,j)=jnim*u2(i,j-1)
	v2(i,j-1)=0.0
 171	continue
c
	i=1
	do 172 j=1,jim
	u2(i,j)=1.0d0
	v2(i,j)=0.0
 172 	continue
c
c   ********* obstacle boundary conditions *********
c
	call bcobw
c   
c    ***********************************************
	return
	end
